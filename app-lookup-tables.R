
library(shiny)
library(DBI)
library(duckdb)

con <- dbConnect(
  duckdb::duckdb(),
  "bryoquelTest.duckdb",
  read_only = TRUE
)

tables <- dbListTables(con)

ui <- fluidPage(
  
  titlePanel("BRYOQUEL Database"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "table",
        "Select a table:",
        choices = tables
      ),
      
      uiOutput("attribute"),
      
      uiOutput("search_field"),
      
      actionButton(
        "search_button",
        "Search"
      )
      
    ),
    
    mainPanel(
      
      textOutput("selected_table"),
      textOutput("selected_attribute"),
      
      tableOutput("results")
      
    )
  )
)

server <- function(input, output, session) {
  
  # Selected table
  output$selected_table <- renderText({
    
    req(input$table)
    
    paste("Table:", input$table)
    
  })
  
  
  # Attributes of selected table
  output$attribute <- renderUI({
    
    req(input$table)
    
    columns <- dbListFields(
      con,
      input$table
    )
    
    selectInput(
      "column",
      "Select an attribute:",
      choices = columns
    )
    
  })
  
  
  # Search field
  output$search_field <- renderUI({
    
    req(input$column)
    
    textInput(
      "search",
      paste("Search", input$column, ":"),
      value = ""
    )
    
  })
  
  
  # Selected attribute
  output$selected_attribute <- renderText({
    
    req(input$column)
    
    paste(
      "Attribute:",
      input$column
    )
    
  })
  
  
  # Search database
  results <- eventReactive(
    input$search_button,
    {
      
      req(input$table)
      req(input$column)
      req(input$search)
      
      sql <- paste0(
        "SELECT * FROM ",
        dbQuoteIdentifier(con, input$table),
        " WHERE CAST(",
        dbQuoteIdentifier(con, input$column),
        " AS VARCHAR) ILIKE ? ",
        "LIMIT 10"
      )
      
      dbGetQuery(
        con,
        sql,
        params = list(
          paste0("%", input$search, "%")
        )
      )
      
    }
  )
  
  
  # Display results
  output$results <- renderTable({
    
    results()
    
  })
  
}

shinyApp(ui, server)
