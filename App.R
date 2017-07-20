require(shiny)
require(rhandsontable)

# creates string from indices and dataframe for updating database:
# COLUMN NAME = VALUE 
UpdateSingleValueString <- function(rr, cc, changed) {
  newVal <- changed[rr,cc]
  
  if (is.na(newVal) || newVal == '') {
    newVal <- "NULL"
  } else {
    if(class(newVal) %in% c('character','Date')) newVal <- paste0("\'",newVal,"\'")
  }
  
  updateString <- paste(colnames(changed)[cc], '=', newVal)
}

# using UpdateSingleValueString, gets full update statement
# rr: row index
# changed: dataframe containing values to update
# indices: indices of values in dataframe that should be pushed to database.
# table: database table to alter
# whereString: a condition (e.g. "primary_key = 101") to indicate which row should
#   be replaced.
UpdateSingleRowString <- function(rr, changed, indices, table, whereString) {
  # get columns that need to be updated in row rr
  cc <- indices[indices[,1] == rr, 2] %>% as.vector
  
  i <- sapply(changed, is.factor)
  changed[i] <- lapply(changed[i], as.character)
  
  # get pairs of columns and values to replace
  updateStringList <- lapply(cc, UpdateSingleValueString, rr = rr, changed = changed)
  # collapse updateStringList by commas into one string
  colUpdateString <- paste(updateStringList, collapse = ', ')
  # append other values to complete update string
  updateString <- paste0("UPDATE ", table, 
    " SET ", colUpdateString,
    " WHERE ", whereString, ";")
  
  return(updateString)
}



#### UI ####
ui <- shinyUI(fluidPage(
  h1('Editing a Database from Shiny'), 
  hr(),
  
  fluidRow(
    column(width = 6,
      h3('Original Data (from database)'),
      rHandsontableOutput('Original')
    ),
    column(width = 6,
      h3('Editable Data (will be written to database)'),
      rHandsontableOutput('Changed')
    )
  ),
  
  fluidRow(
    column(width = 12,
      h3('Query:'),
      textOutput('query')
    )
  )
))

#### SERVER ####
server <- shinyServer(function(input, output, session) {
  #### FromDatabase ####
  FromDatabase <- reactive({
    # Database <- pool::dbGetQuery(conn, "SELECT ...")
    # Stand-in for data queried from a database
    Database <- data.frame(primary_key = 1:10,
      numeric = round(rnorm(10),2),
      logical = rep(TRUE, 10), 
      character = LETTERS[1:10],
      factor = factor(letters[1:10], levels = letters[10:1], 
        ordered = TRUE),
      factor_allow = factor(letters[1:10], levels = letters[10:1], 
        ordered = TRUE),
      date = seq(from = Sys.Date(), by = "days", length.out = 10),
      stringsAsFactors = FALSE)
  })
  
  #### Original ####
  output$Original <- renderRHandsontable({
    rhandsontable(FromDatabase()) %>%
      hot_col(1:NCOL(FromDatabase()), readOnly = TRUE)
  })
  
  #### Changed ####
  # create handsOnTable with unedited data from database.
  # this is the table that can be edited in Shiny
  output$Changed <- renderRHandsontable({
    rhandsontable(FromDatabase()) %>%
      hot_col("factor_allow", allowInvalid = TRUE) %>% 
      hot_col("primary_key", readOnly = TRUE)  # primary key column is read only for matching purposes
  })
  
  #### Query ####
  Query <- reactive({
    
    Changed <- hot_to_r(input$Changed)
    
    ### Step 1: Separate newly added rows and write INSERT query
    
    # new rows will be ones with no primary key because the primary key column
    #   is read-only, so the user has no way to enter a primary key for new rows.
    NewRows <- Changed[is.na(Changed$primary_key), ]
    
    if (NROW(NewRows) == 0) {
      insertQuery <- NULL
    } else {
      
      ### Step A: Handle missing values
      
      # Numeric and boolean columns aren't single-quoted in query:
      # convert to character, then replace NA values with the string "NULL"
      # so that NULL will be rendered when paste()-ing into query.
      numericIndex <- sapply(NewRows, function(x) {is.numeric(x) || is.logical(x)}) 
      NewRows[numericIndex] <- lapply(NewRows[numericIndex], as.character) 
      naIndex <- is.na(NewRows[numericIndex]) 
      NewRows[numericIndex][is.na(NewRows[numericIndex])] <- "NULL"
      
      # Character, factor, and date columns are single-quoted in query:
      # Convert to character to preserve date integrity, 
      # add single-quotes, then
      # Replace 'NA' with "NULL"
      characterIndex <- sapply(NewRows, function(x) {is.character(x) || is.factor(x) || lubridate::is.Date(x)})
      NewRows[characterIndex] <- lapply(NewRows[characterIndex], as.character)
      NewRows[characterIndex] <- apply(NewRows[characterIndex], MARGIN = c(1,2), function(x) paste0('\'', x, '\''))
      NewRows[characterIndex][NewRows[characterIndex] == "'NA'"] <- 'NULL'
      
      ### Step B: create query
      
      # assume that the column names in R match the database column names,
      # get comma-separated column list
      colNameString <- paste0('(',paste0(colnames(NewRows), collapse = ', '), ')')
      
      # Replace all values in the primary_key column with "DEFAULT"
      # assumes the database can auto-increment primary key upon insert
      NewRows$primary_key <- "DEFAULT"
      
      # Collapse values in each row
      eachRowStrings <- apply(X = NewRows, MARGIN = 1, FUN = function(x) {
        paste0('(', paste0(x, collapse = ', '), ')')
      })
      
      # combine into one giant insert statement
      
      insertQuery <- paste("INSERT INTO", "[schema.table]", colNameString, 
        "VALUES", paste(eachRowStrings, collapse = ', '), ';')
    }
    
    
    ### Step 2: Separate updated rows, identify updates, write update query.
    
    # Get existing rows-- rows that have a primary key already -- and check for updates
    ExistingRows <- Changed[!is.na(Changed$primary_key), ]
    
    Original <-  FromDatabase()
    
    
    
    # convert all factors to characters to prevent issues with factor matching
    i <- sapply(Original, is.factor)
    Original[i] <- lapply(Original[i], as.character)
    ExistingRows[i] <- lapply(ExistingRows[i], as.character)
    
    ### get the difference between Original and ExistingRows
    # Find indices where:
    #   A. Only one is NA (a value has been added or deleted)
    newNAIndex <- which(xor(is.na(Original), is.na(ExistingRows)), arr.ind = TRUE)
    #   B. A non-NA value has been changed
    changedIndex <- which(Original != ExistingRows, arr.ind = TRUE)
    
    indices <- rbind(newNAIndex, changedIndex)
    
    
    ## if there are updates to be made, write update query
    if (NROW(indices) == 0) {
      updateQuery <- NULL
    } else {
      
      rr <- unique(indices[,1]) #unique row values to update
      
      # get list of row update queries 
      # where condition is based on primary_key
      updateStringList <- lapply(rr, function(rr) {
        UpdateSingleRowString(rr = rr,
          changed =  ExistingRows, 
          indices = indices, 
          table = '[schema.table]',
          whereString =  paste("primary_key =", Original$primary_key[rr])
        )
      })
      
      # collapse to one string for querying
      updateQuery <- paste(updateStringList, collapse = '\n')
    }
    
    if (is.null(insertQuery) && is.null(updateQuery)) {
      return("")
    } else if (is.null(insertQuery)) {
      return(updateQuery)
    } else if (is.null(updateQuery)) {
      return(insertQuery)
    } else {
      return(paste(insertQuery, "\n", updateQuery))
    }
    
  })
  
  output$query <- renderText(Query())
  
})

shinyApp(ui, server)