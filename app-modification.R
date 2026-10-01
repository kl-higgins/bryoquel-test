library(shiny)
library(DBI)
library(duckdb)

# ---------------------------------------------------------
# Connect to DuckDB with WRITE access
# ---------------------------------------------------------

con <- dbConnect(
  duckdb::duckdb(),
  "bryoquelTest.duckdb"
)

onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})


# ---------------------------------------------------------
# User interface
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("BRYOQUEL — Ajout de données"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ===================================================
      # USER QUESTIONS
      # ===================================================
      
      h4("Type d'ajout"),
      
      radioButtons(
        "nouveau_taxon",
        "Nouveau taxon à ajouter au territoire du Québec-Labrador :",
        choices = c(
          "Oui" = "oui",
          "Non" = "non"
        ),
        selected = "oui"
      ),
      
      radioButtons(
        "nouveau_nom",
        "Nouveau nom scientifique à ajouter :",
        choices = c(
          "Oui" = "oui",
          "Non" = "non"
        ),
        selected = "oui"
      ),
      
      
      # ===================================================
      # EXISTING IDtaxon
      # ===================================================
      
      conditionalPanel(
        
        condition = "input.nouveau_taxon == 'non' && 
                     input.nouveau_nom == 'oui'",
        
        h4("Taxon existant"),
        
        numericInput(
          "existing_IDtaxon",
          "IDtaxon existant:",
          value = NA,
          min = 1,
          step = 1
        )
      ),
      
      
      # ===================================================
      # NOMENCLATURE
      # ===================================================
      
      conditionalPanel(
        
        condition = "input.nouveau_nom == 'oui'",
        
        h4("Nouvelle nomenclature"),
        
        textInput(
          "genus_name",
          "Genre:",
          value = ""
        ),
        
        textInput(
          "specificEpithet_name",
          "Épithète spécifique:",
          value = ""
        ),
        
        textInput(
          "scientificNameAuthorshipSpecific",
          "Autorité scientifique de l'espèce:",
          value = ""
        ),
        
        selectInput(
          "taxonRank",
          "Rang taxonomique:",
          choices = c(
            "species",
            "subspecies",
            "variety",
            "form"
          )
        ),
        
        selectInput(
          "statut",
          "Statut:",
          choices = c(
            "accepté",
            "complexe",
            "synonyme",
            "à déterminer"
          )
        )
        
      ),
      
      
      # ===================================================
      # TAXONS
      # ===================================================
      
      conditionalPanel(
        
        condition = "input.nouveau_taxon == 'oui'",
        
        h4("Nouveau taxon"),
        
        selectInput(
          "etatTaxon",
          "État du taxon:",
          choices = c(
            "actif", "inactif"
          )
        ),
        
        textInput(
          "etatTaxonCommentaire",
          "Commentaire:",
          value = ""
        )
        
      ),
      
      
      # ===================================================
      # SAVE
      # ===================================================
      
      br(),
      
      actionButton(
        "save_record",
        "Ajouter",
        class = "btn-primary"
      )
      
    ),
    
    
    # -----------------------------------------------------
    # Main panel
    # -----------------------------------------------------
    
    mainPanel(
      
      h4("Résultat"),
      
      verbatimTextOutput("result"),
      
      h4("Vérification"),
      
      tableOutput("verification")
      
    )
    
  )
)


# ---------------------------------------------------------
# Server
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  result <- reactiveVal("")
  verification <- reactiveVal(NULL)
  
  
  observeEvent(input$save_record, {
    
    tryCatch({
      
      # ===================================================
      # VALIDATION
      # ===================================================
      
      # -----------------------------------------------
      # No new taxon AND no new name
      # -----------------------------------------------
      
      if (
        input$nouveau_taxon == "non" &&
        input$nouveau_nom == "non"
      ) {
        
        stop(
          "Vous devez sélectionner « Oui » pour au moins \
un des deux types d'ajout."
        )
        
      }
      
      
      # -----------------------------------------------
      # New name = require nomenclature fields
      # -----------------------------------------------
      
      if (input$nouveau_nom == "oui") {
        
        if (
          !nzchar(input$genus_name) ||
          !nzchar(input$specificEpithet_name)
        ) {
          
          stop(
            "Le genre et l'épithète spécifique doivent \
être remplis."
          )
          
        }
        
      }
      
      
      # -----------------------------------------------
      # Existing taxon required when:
      # nouveau taxon = non
      # nouveau nom = oui
      # -----------------------------------------------
      
      if (
        input$nouveau_taxon == "non" &&
        input$nouveau_nom == "oui"
      ) {
        
        if (
          is.na(input$existing_IDtaxon) ||
          input$existing_IDtaxon <= 0
        ) {
          
          stop(
            "Vous devez entrer un IDtaxon existant."
          )
          
        }
        
        
        # Check that IDtaxon actually exists
        
        taxon_exists <- dbGetQuery(
          con,
          "SELECT COUNT(*) AS n
           FROM taxons
           WHERE IDtaxon = ?",
          params = list(input$existing_IDtaxon)
        )$n
        
        
        if (taxon_exists == 0) {
          
          stop(
            paste0(
              "L'IDtaxon ",
              input$existing_IDtaxon,
              " n'existe pas dans la table taxons."
            )
          )
          
        }
        
      }
      
      
      # ===================================================
      # START TRANSACTION
      # ===================================================
      
      dbBegin(con)
      
      now <- as.Date(Sys.time())
      
      
      # ===================================================
      # CASE 1
      #
      # Nouveau taxon = OUI
      # Nouveau nom = OUI
      #
      # Create new IDtaxon + new IDname
      # ===================================================
      
      if (
        input$nouveau_taxon == "oui" &&
        input$nouveau_nom == "oui"
      ) {
        
        # -------------------------------------------------
        # Create IDtaxon
        # -------------------------------------------------
        
        newIDtaxon <- dbGetQuery(
          con,
          "SELECT COALESCE(MAX(IDtaxon), 0) + 1 AS IDtaxon
           FROM taxons"
        )$IDtaxon
        
        
        # -------------------------------------------------
        # Add taxon
        # -------------------------------------------------
        
        newTaxon <- data.frame(
          
          IDtaxon = newIDtaxon,
          
          IDnameForExport = NA_integer_,
          
          etatTaxon = input$etatTaxon,
          
          dateModif = now,
          
          dateCreation = now,
          
          etatTaxonCommentaire =
            input$etatTaxonCommentaire,
          
          stringsAsFactors = FALSE
          
        )
        
        
        dbAppendTable(
          con,
          "taxons",
          newTaxon
        )
        
        
        # -------------------------------------------------
        # Create IDname
        # -------------------------------------------------
        
        newIDname <- dbGetQuery(
          con,
          "SELECT COALESCE(MAX(IDname), 0) + 1 AS IDname
           FROM nomenclature"
        )$IDname
        
        
        # -------------------------------------------------
        # Add nomenclature
        # -------------------------------------------------
        
        newName <- data.frame(
          
          IDname = newIDname,
          
          IDtaxon = newIDtaxon,
          
          IDnameAccUsage = newIDname,
          
          statut = input$statut,
          
          taxonRank = input$taxonRank,
          
          genus_name = input$genus_name,
          
          specificEpithet_name =
            input$specificEpithet_name,
          
          infraspecificEpithet_name =
            NA_character_,
          
          nameAccordingTo =
            NA_character_,
          
          scientificNameAuthorshipSpecific =
            input$scientificNameAuthorshipSpecific,
          
          stringsAsFactors = FALSE
          
        )
        
        
        dbAppendTable(
          con,
          "nomenclature",
          newName
        )
        
        
        # -------------------------------------------------
        # Set IDnameForExport
        # -------------------------------------------------
        
        dbExecute(
          con,
          "UPDATE taxons
           SET IDnameForExport = ?,
               dateModif = ?
           WHERE IDtaxon = ?",
          params = list(
            newIDname,
            now,
            newIDtaxon
          )
        )
        
        
        # -------------------------------------------------
        # Verification
        # -------------------------------------------------
        
        check <- dbGetQuery(
          con,
          "SELECT
             n.IDname,
             n.IDtaxon,
             n.genus_name,
             n.specificEpithet_name,
             n.statut,
             t.IDnameForExport,
             t.etatTaxon
           FROM nomenclature n
           LEFT JOIN taxons t
             ON n.IDtaxon = t.IDtaxon
           WHERE n.IDname = ?",
          params = list(newIDname)
        )
        
        
        dbCommit(con)
        
        
        result(
          paste0(
            "Nouveau taxon et nouveau nom ajoutés \
avec succès.\n\n",
            "IDtaxon: ", newIDtaxon, "\n",
            "IDname: ", newIDname
          )
        )
        
        verification(check)
        
      }
      
      # ===================================================
      # CASE 2
      #
      # Nouveau taxon = NON
      # Nouveau nom = OUI
      #
      # Existing IDtaxon + new IDname
      # Update IDnameForExport + dateModif
      # ===================================================
      
      else if (
        input$nouveau_taxon == "non" &&
        input$nouveau_nom == "oui"
      ) {
        
        existingIDtaxon <- input$existing_IDtaxon
        
        
        # -------------------------------------------------
        # Create new IDname
        # -------------------------------------------------
        
        newIDname <- dbGetQuery(
          con,
          "SELECT COALESCE(MAX(IDname), 0) + 1 AS IDname
     FROM nomenclature"
        )$IDname
        
        
        # -------------------------------------------------
        # Add nomenclature
        # -------------------------------------------------
        
        newName <- data.frame(
          
          IDname = newIDname,
          
          IDtaxon = existingIDtaxon,
          
          IDnameAccUsage = newIDname,
          
          statut = input$statut,
          
          taxonRank = input$taxonRank,
          
          genus_name = input$genus_name,
          
          specificEpithet_name =
            input$specificEpithet_name,
          
          infraspecificEpithet_name =
            NA_character_,
          
          nameAccordingTo =
            NA_character_,
          
          scientificNameAuthorshipSpecific =
            input$scientificNameAuthorshipSpecific,
          
          stringsAsFactors = FALSE
          
        )
        
        
        dbAppendTable(
          con,
          "nomenclature",
          newName
        )
        
        
        # -------------------------------------------------
        # Update IDnameForExport and dateModif
        # -------------------------------------------------
        
        dbExecute(
          con,
          "UPDATE taxons
     SET IDnameForExport = ?,
         dateModif = ?
     WHERE IDtaxon = ?",
          params = list(
            newIDname,
            now,
            existingIDtaxon
          )
        )
        
        
        # -------------------------------------------------
        # Verification
        # -------------------------------------------------
        
        check <- dbGetQuery(
          con,
          "SELECT
       n.IDname,
       n.IDtaxon,
       n.genus_name,
       n.specificEpithet_name,
       n.statut,
       t.IDnameForExport,
       t.dateModif,
       t.etatTaxon
     FROM nomenclature n
     LEFT JOIN taxons t
       ON n.IDtaxon = t.IDtaxon
     WHERE n.IDname = ?",
          params = list(newIDname)
        )
        
        
        # -------------------------------------------------
        # Commit transaction
        # -------------------------------------------------
        
        dbCommit(con)
        
        
        result(
          paste0(
            "Nouveau nom ajouté avec succès au taxon existant.\n\n",
            "IDtaxon: ", existingIDtaxon, "\n",
            "Nouveau IDname: ", newIDname,
            "\nIDnameForExport mis à jour: ", newIDname
          )
        )
        
        verification(check)
      }     
      
      # ===================================================
      # CASE 3
      #
      # Nouveau taxon = OUI
      # Nouveau nom = NON
      #
      # New IDtaxon only
      # ===================================================
      
      else if (
        input$nouveau_taxon == "oui" &&
        input$nouveau_nom == "non"
      ) {
        
        newIDtaxon <- dbGetQuery(
          con,
          "SELECT COALESCE(MAX(IDtaxon), 0) + 1 AS IDtaxon
           FROM taxons"
        )$IDtaxon
        
        
        newTaxon <- data.frame(
          
          IDtaxon = newIDtaxon,
          
          IDnameForExport = NA_integer_,
          
          etatTaxon = input$etatTaxon,
          
          dateModif = now,
          
          dateCreation = now,
          
          etatTaxonCommentaire =
            input$etatTaxonCommentaire,
          
          stringsAsFactors = FALSE
          
        )
        
        
        dbAppendTable(
          con,
          "taxons",
          newTaxon
        )
        
        
        check <- dbGetQuery(
          con,
          "SELECT *
           FROM taxons
           WHERE IDtaxon = ?",
          params = list(newIDtaxon)
        )
        
        
        dbCommit(con)
        
        
        result(
          paste0(
            "Nouveau taxon ajouté sans nouveau nom \
scientifique.\n\n",
            "IDtaxon: ", newIDtaxon
          )
        )
        
        verification(check)
        
      }
      
      
    }, error = function(e) {
      
      # ---------------------------------------------------
      # Roll back if anything failed
      # ---------------------------------------------------
      
      try(
        dbRollback(con),
        silent = TRUE
      )
      
      result(
        paste0(
          "Erreur — aucune modification enregistrée:\n\n",
          e$message
        )
      )
      
      verification(NULL)
      
    })
    
  })
  
  
  # -------------------------------------------------------
  # Display result
  # -------------------------------------------------------
  
  output$result <- renderText({
    result()
  })
  
  
  # -------------------------------------------------------
  # Display verification
  # -------------------------------------------------------
  
  output$verification <- renderTable({
    verification()
  })
  
}


shinyApp(ui, server)