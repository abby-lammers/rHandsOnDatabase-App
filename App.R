require(shiny)
require(rhandsontable)


shinyApp(
  ui = shinyUI(fluidPage(
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
        h3('Query:')
      )
    )
  )),
  
  server = shinyServer(function(input, output, session) {
    
    FromDatabase <- reactive({
      # Database <- pool::dbGetQuery(conn, "SELECT ...")
      # Stand-in for data queried from a database
      Database <- data.frame(primary_key = 1:10,
        numeric = rnorm(10),
        logical = rep(TRUE, 10), 
        character = LETTERS[1:10],
        factor = factor(letters[1:10], levels = letters[10:1], 
          ordered = TRUE),
        factor_allow = factor(letters[1:10], levels = letters[10:1], 
          ordered = TRUE),
        date = seq(from = Sys.Date(), by = "days", length.out = 10),
        stringsAsFactors = FALSE)
    })
    
    # view
    output$Original <- renderRHandsontable({
      rhandsontable(FromDatabase()) %>%
        hot_col(1:NCOL(FromDatabase()), readOnly = TRUE)
    })
    
    # create handsOnTable with unedited data from database.
    # this is the table that can be edited in Shiny
    output$Changed <- renderRHandsontable({
      rhandsontable(FromDatabase()) %>%
        hot_col("factor_allow", allowInvalid = TRUE) %>% 
        hot_col("primary_key", readOnly = TRUE)  # primary key column is read only for matching purposes
    })
    
    output$query <- renderText({
      
      ### Step 1: Separate newly added rows
      GetInsertUsage_CostQuery <- function(Original, Changed, editor) {
        NewRows <- Changed[is.na(Changed$usage_cost_pk), ]
        
        if (NROW(NewRows) == 0) return (NULL)
        
        # for numeric cols:
        #   replace NA with "NULL"
        #   convert column to character
        NewRows.Numeric <- NewRows[ ,c('consumption','cost_dollars','demand')]
        NewRows.Numeric <- lapply(NewRows.Numeric, as.character) %>% data.frame(stringsAsFactors = FALSE) 
        NewRows.Numeric[is.na(NewRows.Numeric)] <- "NULL"
        
        # for character/factor/date cols:
        #   replace NA with "NULL"
        #   add escaped single quotes around all non-"NULL" entries
        NewRows.Character <- NewRows[ ,c('interval_start','interval_end','service_date',
          'baseline_status','consumption_units','demand_units')]
        NewRows.Character <- lapply(NewRows.Character, as.character) %>% data.frame(stringsAsFactors = FALSE)
        NewRows.Character <- apply(NewRows.Character, MARGIN = c(1,2), function(x) paste0('\'', x, '\'')) %>% data.frame(stringsAsFactors = FALSE)
        NewRows.Character[NewRows.Character == "'NA'"] <- 'NULL'
        
        # recombine all columns
        NewRows <- bind_cols(NewRows[,c('usage_cost_pk', 'meter_pk')], NewRows.Character, NewRows.Numeric)
        
        oneRow <- apply(X = NewRows[,-(1:2)], MARGIN =  1, FUN = function(x) {
          paste('(',
            Original$meter_pk[1], ',', 
            paste(x, collapse = ','),',',
            paste0('\'', lubridate::now(), '\''), ',', 
            paste0("'", editor, "'"), ',',
            paste0('\'', lubridate::now(), '\''), ',', 
            paste0("'", editor, "'"), 
            ')'
          )
        })
        
        # write query string
        insertString <- paste("INSERT INTO utility.usage_cost 
          (meter_pk, interval_start, interval_end, service_date,
          baseline_status, consumption_units, demand_units, 
          consumption, cost_dollars, demand, create_timestamp,
          created_by, modify_timestamp, modified_by
          ) VALUES", paste(oneRow, collapse = ','), ';')
        
        return(insertString)
      }
      
      IsDiff_rHandsOnTable <- function(original, changed) {
        original <- original[,colnames(changed)]
        
        i <- sapply(original, is.factor)
        original[i] <- lapply(original[i], as.character)
        changed[i] <- lapply(changed[i], as.character)
        
        ReplaceNAsInColumn <- function(col) {
          if(class(col) %in% c('numeric','Date','character')) {
            replaceVal <- switch(class(col),
              'numeric' = 0,
              'Date' = ymd('1970-01-01'),
              'character' = "NA"
            )
            
            col[is.na(col)] <- replaceVal
          }
          
          return(col)
        }
        
        original <- lapply(original, ReplaceNAsInColumn)
        
        #original <- lapply(original, as.character) %>% data.frame(stringsAsFactors = FALSE)
        #changed <- lapply(changed, as.character) %>% data.frame(stringsAsFactors = FALSE)
        
        which(original != changed, arr.ind = TRUE)
        
      }
      
      # creates string from indices and dataframe for updating database:
      # COLUMN NAME = VALUE 
      UpdateSingleValueString <- function(rr, cc, changed) {
        newVal <- changed[rr,cc]
        if(class(newVal) %in% c('character','Date')) newVal <- paste0("\'",newVal,"\'")
        updateString <- paste(colnames(changed)[cc], '=', newVal)
      }
      
      # using UpdateSingleValueString, gets full update statement
      # rr: row index
      # changed: dataframe containing values to update
      # indices: indices of values in dataframe that should be pushed to database.
      #   Generated by IsDiff_rHandsOnTable()
      # table: table to alter
      # whereString: a condition (e.g. "meter_pk = 101") to indicate which row should
      #   be replaced.
      UpdateSingleRowString <- function(rr, changed, indices, table, whereString, editor) {
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
          " SET ", colUpdateString, ",", 
          " modified_by = ", paste0("'", editor, "'"), ",",
          " modify_timestamp = ", paste0("'", lubridate::now(), "'"),
          " WHERE ", whereString, ";")
        
        return(updateString)
      }
      
      GetUpdateUsage_CostQuery <- function(Original, Changed, editor) {
        # Get rows to update (rows that already have a usage_cost_pk)
        UpdatedRows <- Changed[!is.na(Changed$usage_cost_pk), ]
        
        # if no rows to update, return NULL
        if (NROW(UpdatedRows) == 0) return (NULL)
        
        ## UPDATE ROWS
        # get indices of values that differ between Original and Changed
        indices <- IsDiff_rHandsOnTable(Original, UpdatedRows)
        
        rr <- unique(indices[,1]) #unique row values to update
        
        # get list of row update queries 
        # where condition is based on usage_pk
        updateStringList <- lapply(rr, function(rr) {
          UpdateSingleRowString(rr = rr,
            changed =  UpdatedRows, 
            indices = indices, 
            table = 'utility.usage_cost',
            whereString =  paste("usage_cost_pk =", Original$usage_cost_pk[rr]),
            editor = editor
          )
        })
        
        # collapse to one string for querying
        updateString <- paste(updateStringList, collapse = ' ')
        
        return(updateString)
      }
      
      
      
      
      # get insert query for new rows
    })
    
    })
)

