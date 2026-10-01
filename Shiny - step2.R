
library(shiny)
library(DBI)
library(duckdb)
library(DT)

# ---------------------------------------------------------
# 1. Connect to the DuckDB database
# ---------------------------------------------------------

con <- dbConnect(duckdb::duckdb(), "bryoquelTest.duckdb", read_only = TRUE)

# Close connection when the Shiny app stops
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})


# ---------------------------------------------------------
# 2. User interface
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("BRYOQUEL — Database browser"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "table",
        "Table",
        choices = dbListTables(con)
      ),
      
      uiOutput("search_fields"),
      
      actionButton(
        "search",
        "Search",
        class = "btn-primary"
      ),
      
      actionButton(
        "reset",
        "Reset"
      ),
      
      width = 3
    ),
    
    mainPanel(
      
      textOutput("result_count"),
      
      DTOutput("results"),
      
      width = 9
    )
  )
)


# ---------------------------------------------------------
# 3. Server
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  # -------------------------------------------------------
  # Get information about the selected table
  # -------------------------------------------------------
  
  table_info <- reactive({
    
    req(input$table)
    
    dbGetQuery(
      con,
      paste0(
        "DESCRIBE ",
        dbQuoteIdentifier(con, input$table)
      )
    )
    
  })
  
  
  # -------------------------------------------------------
  # Create search fields automatically
  # -------------------------------------------------------
  
  output$search_fields <- renderUI({
    
    info <- table_info()
    
    fields <- lapply(seq_len(nrow(info)), function(i) {
      
      column <- info$column[i]
      type <- toupper(info$column_type[i])
      
      if (grepl("INT|DOUBLE|FLOAT|DECIMAL|NUMERIC", type)) {
        
        numericInput(
          inputId = paste0("search_", i),
          label = column,
          value = NA
        )
        
      } else if (grepl("DATE", type)) {
        
        dateInput(
          inputId = paste0("search_", i),
          label = column,
          value = NULL
        )
        
      } else {
        
        textInput(
          inputId = paste0("search_", i),
          label = column,
          value = ""
        )
        
      }
      
    })
    
    tagList(fields)
    
  })
  
  
  # -------------------------------------------------------
  # Store current search
  # -------------------------------------------------------
  
  search_trigger <- reactiveVal(0)
  
  observeEvent(input$search, {
    search_trigger(search_trigger() + 1)
  })
  
  
  # -------------------------------------------------------
  # Reset search fields
  # -------------------------------------------------------
  
  observeEvent(input$reset, {
    
    session$reload()
    
  })
  
  
  # -------------------------------------------------------
  # Query the database
  # -------------------------------------------------------
  
  results <- reactive({
    
    search_trigger()
    
    info <- table_info()
    
    table <- dbQuoteIdentifier(con, input$table)
    
    conditions <- character()
    params <- list()
    
    for (i in seq_len(nrow(info))) {
      
      column <- info$column[i]
      type <- toupper(info$column_type[i])
      value <- input[[paste0("search_", i)]]
      
      # Skip empty search fields
      if (is.null(value) || length(value) == 0 || is.na(value)) {
        next
      }
      
      # Numeric fields
      if (grepl("INT|DOUBLE|FLOAT|DECIMAL|NUMERIC", type)) {
        
        conditions <- c(
          conditions,
          paste0(
            dbQuoteIdentifier(con, column),
            " = ?"
          )
        )
        
        params <- c(params, list(value))
        
        # Date fields
      } else if (grepl("DATE", type)) {
        
        conditions <- c(
          conditions,
          paste0(
            dbQuoteIdentifier(con, column),
            " = ?"
          )
        )
        
        params <- c(params, list(as.Date(value)))
        
        # Character fields
      } else {
        
        conditions <- c(
          conditions,
          paste0(
            "CAST(",
            dbQuoteIdentifier(con, column),
            " AS VARCHAR) ILIKE ?"
          )
        )
        
        params <- c(
          params,
          list(paste0("%", value, "%"))
        )
      }
    }
    
    
    # Construct SQL query
    sql <- paste0(
      "SELECT * FROM ",
      table
    )
    
    if (length(conditions) > 0) {
      
      sql <- paste0(
        sql,
        " WHERE ",
        paste(conditions, collapse = " AND ")
      )
      
    }
    
    # Limit initial output for safety
    sql <- paste0(
      sql,
      " LIMIT 5000"
    )
    
    
    dbGetQuery(
      con,
      sql,
      params = params
    )
    
  })
  
  
  # -------------------------------------------------------
  # Display results
  # -------------------------------------------------------
  
  output$results <- renderDT({
    
    dat <- results()
    
    datatable(
      dat,
      filter = "none",
      extensions = c("Scroller"),
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        deferRender = TRUE,
        scroller = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  
  # -------------------------------------------------------
  # Number of results
  # -------------------------------------------------------
  
  output$result_count <- renderText({
    
    dat <- results()
    
    paste(
      format(nrow(dat), big.mark = ","),
      "records displayed"
    )
    
  })
  
}


# ---------------------------------------------------------
# 4. Start application
# ---------------------------------------------------------

shinyApp(ui, server)

