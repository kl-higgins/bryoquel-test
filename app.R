library(shiny)
library(DBI)
library(duckdb)

con <- dbConnect(duckdb(), "bryoquelTest.duckdb")

ui <- fluidPage(
  titlePanel("BRYOQUEL test"),
  tableOutput("taxons")
)

server <- function(input, output, session) {
  output$taxons <- renderTable(
    dbGetQuery(con, "SELECT * FROM taxons LIMIT 100")
  )
}

shinyApp(ui, server)