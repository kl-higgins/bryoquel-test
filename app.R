library(shiny)
library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), "bryoquelTest.duckdb")
onStop(function() dbDisconnect(con, shutdown = TRUE))

tables <- dbListTables(con)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #DCE8D5; padding-bottom: 75px; }
    .bottom-nav { position: fixed; bottom: 0; left: 0; width: 100%;
      background: #426B4B; padding: 10px; text-align: center; z-index: 1000; }
    .bottom-nav .btn { margin: 0 8px; }
    .home-choice { margin: 15px 0; }
  "))),
  titlePanel("BRYOQUEL"),
  uiOutput("page"),
  div(class = "bottom-nav",
      actionButton("nav_home", "Accueil"),
      actionButton("nav_view", "Visualiser"),
      actionButton("nav_edit", "Modifier"))
)

server <- function(input, output, session) {
  
  mode <- reactiveVal("home")
  result <- reactiveVal("")
  verification <- reactiveVal(NULL)
  
  observeEvent(input$choose_view, mode("view"))
  observeEvent(input$choose_edit, mode("edit"))
  observeEvent(input$nav_home, mode("home"))
  observeEvent(input$nav_view, mode("view"))
  observeEvent(input$nav_edit, mode("edit"))
  
  output$page <- renderUI({
    if (mode() == "home") {
      return(
        fluidRow(
          column(8, offset = 2,
                 h3("Base de données des bryophytes du Québec-Labrador"),
                 p("Que souhaitez-vous faire?"),
                 div(class = "home-choice",
                     actionButton("choose_view", "Visualiser la base de données",
                                  class = "btn-success", width = "100%")),
                 div(class = "home-choice",
                     actionButton("choose_edit", "Modifier la base de données",
                                  class = "btn-success", width = "100%"))
          )
        )
      )
    }
    
    if (mode() == "view") {
      return(
        sidebarLayout(
          sidebarPanel(
            selectInput("table", "Table :", choices = dbListTables(con)),
            uiOutput("attribute"),
            uiOutput("search_field"),
            actionButton("search_button", "Rechercher")
          ),
          mainPanel(
            textOutput("selected_table"),
            textOutput("selected_attribute"),
            tableOutput("results")
          )
        )
      )
    }
    
    sidebarLayout(
      sidebarPanel(
        h4("Type d'ajout"),
        radioButtons("nouveau_taxon",
                     "Nouveau taxon au Québec-Labrador :",
                     choices = c("Oui" = "oui", "Non" = "non"),
                     selected = "oui"),
        radioButtons("nouveau_nom",
                     "Nouveau nom scientifique :",
                     choices = c("Oui" = "oui", "Non" = "non"),
                     selected = "oui"),
        
        conditionalPanel(
          "input.nouveau_taxon == 'non' && input.nouveau_nom == 'oui'",
          h4("Taxon et nom existants"),
          numericInput("existing_IDtaxon", "IDtaxon existant :", NA, min = 1),
          numericInput("existing_IDname", "IDname existant :", NA, min = 1)
        ),
        
        conditionalPanel(
          "input.nouveau_nom == 'oui'",
          h4("Nouvelle nomenclature"),
          textInput("genus_name", "Genre :"),
          textInput("specificEpithet_name", "Épithète spécifique :"),
          textInput("scientificNameAuthorshipSpecific", "Autorité scientifique :"),
          selectInput("taxonRank", "Rang taxonomique :",
                      c("species", "subspecies", "variety", "form")),
          selectInput("statut", "Statut :",
                      c("accepté", "complexe", "synonyme", "à déterminer"))
        ),
        
        conditionalPanel(
          "input.nouveau_taxon == 'oui'",
          h4("Nouveau taxon"),
          selectInput("etatTaxon", "État du taxon :", c("actif", "inactif")),
          textInput("etatTaxonCommentaire", "Commentaire :")
        ),
        
        actionButton("save_record", "Ajouter", class = "btn-primary")
      ),
      mainPanel(
        h4("Résultat"),
        verbatimTextOutput("result"),
        h4("Vérification"),
        tableOutput("verification")
      )
    )
  })
  
  # ---- LOOKUP MODE ----
  
  output$selected_table <- renderText({
    req(input$table)
    paste("Table :", input$table)
  })
  
  output$attribute <- renderUI({
    req(input$table)
    selectInput("column", "Attribut :",
                choices = dbListFields(con, input$table))
  })
  
  output$search_field <- renderUI({
    req(input$column)
    textInput("search", paste("Rechercher dans", input$column, ":"),
              value = "")
  })
  
  output$selected_attribute <- renderText({
    req(input$column)
    paste("Attribut :", input$column)
  })
  
  results <- eventReactive(input$search_button, {
    req(input$table, input$column, input$search)
    
    sql <- paste0(
      "SELECT * FROM ", dbQuoteIdentifier(con, input$table),
      " WHERE CAST(", dbQuoteIdentifier(con, input$column),
      " AS VARCHAR) ILIKE ? LIMIT 10"
    )
    
    dbGetQuery(con, sql, params = list(paste0("%", input$search, "%")))
  })
  
  output$results <- renderTable(results())
  
  # ---- EDIT MODE ----
  
  observeEvent(input$save_record, {
    tryCatch({
      req(input$nouveau_taxon, input$nouveau_nom)
      
      new_taxon <- input$nouveau_taxon == "oui"
      new_name <- input$nouveau_nom == "oui"
      
      if (!new_taxon && !new_name)
        stop("Sélectionnez Oui pour au moins un type d'ajout.")
      
      if (new_name &&
          (!nzchar(input$genus_name) ||
           !nzchar(input$specificEpithet_name)))
        stop("Le genre et l'épithète spécifique sont obligatoires.")
      
      if (!new_taxon && new_name) {
        id_taxon <- input$existing_IDtaxon
        id_name <- input$existing_IDname
        
        if (is.null(id_taxon) || is.na(id_taxon) || id_taxon <= 0)
          stop("Entrez un IDtaxon existant.")
        
        if (is.null(id_name) || is.na(id_name) || id_name <= 0)
          stop("Entrez un IDname existant.")
        
        existing <- dbGetQuery(
          con,
          "SELECT IDname, IDtaxon, statut
     FROM nomenclature
     WHERE IDname = ? AND IDtaxon = ?",
          params = list(id_name, id_taxon)
        )
        
        if (nrow(existing) == 0)
          stop("L'IDname n'existe pas pour cet IDtaxon.")
        
        if (input$statut == "accepté" && existing$statut[1] != "accepté")
          stop("L'IDname existant doit être le nom actuellement accepté.")
      }
      
      dbBegin(con)
      now <- Sys.Date()
      newIDtaxon <- NA_integer_
      newIDname <- NA_integer_

     #CAS nouveau taxon sur le territoire####
      
      if (new_taxon) {
        newIDtaxon <- dbGetQuery(
          con, "SELECT COALESCE(MAX(IDtaxon), 0) + 1 AS id FROM taxons"
        )$id
        
        dbAppendTable(con, "taxons", data.frame(
          IDtaxon = newIDtaxon,
          IDnameForExport = NA_integer_,
          etatTaxon = input$etatTaxon,
          dateModif = now,
          dateCreation = now,
          etatTaxonCommentaire = input$etatTaxonCommentaire
        ))
      } else {
        newIDtaxon <- input$existing_IDtaxon
      }
  
      #CAS : nouveau nom scientifique sans ajout de taxons au territoire
      
      if (new_name) {
        newIDname <- dbGetQuery(
          con,
          "SELECT COALESCE(MAX(IDname), 0) + 1 AS id FROM nomenclature"
        )$id
        
        
        dbAppendTable(con, "nomenclature", data.frame(
          IDname = newIDname,
          IDtaxon = newIDtaxon,
          IDnameAccUsage = newIDname,
          statut = input$statut,
          taxonRank = input$taxonRank,
          genus_name = input$genus_name,
          specificEpithet_name = input$specificEpithet_name,
          infraspecificEpithet_name = NA_character_,
          nameAccordingTo = NA_character_,
          scientificNameAuthorshipSpecific =
            input$scientificNameAuthorshipSpecific
        ))
        
        # If the new name is accepted, update the previous name
        if (!new_taxon && input$statut == "accepté") {
          dbExecute(
            con,
            "UPDATE nomenclature
             SET statut = 'synonyme',
                 IDnameAccUsage = ?
             WHERE IDname = ? AND IDtaxon = ?",
            params = list(
              newIDname,
              input$existing_IDname,
              input$existing_IDtaxon
            )
          )
          
          # Redirect other records that referenced the old accepted name
          dbExecute(
            con,
            "UPDATE nomenclature
             SET IDnameAccUsage = ?
             WHERE IDnameAccUsage = ? AND IDtaxon <> ?",
            params = list(
              newIDname,
              input$existing_IDname,
              input$existing_IDtaxon
            )
          )
        }
        
        # Update the taxon's export name
        dbExecute(
          con,
          "UPDATE taxons SET IDnameForExport = ?, dateModif = ?
           WHERE IDtaxon = ?",
          params = list(newIDname, now, newIDtaxon)
        )
        
      }
      
      if (new_name) {
        check <- dbGetQuery(
          con,
          "SELECT n.IDname, n.IDtaxon, n.genus_name,
                  n.specificEpithet_name, n.statut,
                  t.IDnameForExport, t.dateModif, t.etatTaxon
           FROM nomenclature n
           LEFT JOIN taxons t ON n.IDtaxon = t.IDtaxon
           WHERE n.IDtaxon = ?  ORDER BY n.IDname",
          params = list(newIDtaxon)
        )
      } else {
        check <- dbGetQuery(
          con, "SELECT * FROM taxons WHERE IDtaxon = ?",
          params = list(newIDtaxon)
        )
      }
      
      dbCommit(con)
      result(paste(
        "Ajout réussi.",
        paste("IDtaxon :", newIDtaxon),
        if (new_name) paste("IDname :", newIDname),
        sep = "\n"
      ))
      verification(check)
      
    }, error = function(e) {
      try(dbRollback(con), silent = TRUE)
      result(paste("Erreur — aucune modification confirmée :",
                   e$message, sep = "\n\n"))
      verification(NULL)
    })
  })
  
  output$result <- renderText(result())
  output$verification <- renderTable(verification())
}

shinyApp(ui, server)

#IDtaxon 805; IDname 942
#353	411	accepté	species	Catoscopium	nigritum
#IDtaxon 184 ID name  213