


library(RSQLite)
library(DBI)
library(duckdb)
library(dplyr)
library(datamods)
library(shiny)

#"BRYOQUELInterfaceR$tables"
#créer le fichier .duckdb pour rassemblers les fichiers de tables
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = "C:/Users/Kellina/GeoFlora/projets/2024-002 gestion BRYOQUEL MELCC SQB/BRYOQUELInterfaceR/bryoquelTest.duckdb"
)


nomenclature <- read.csv("tables/nomenclature.csv", fileEncoding = "latin1")
occurrences  <- read.csv("tables/occurrences.csv", fileEncoding = "latin1")
taxons       <- read.csv("tables/taxons.csv", fileEncoding = "latin1")

#load with PRIMARY key as first column for readability
nomenclature <- nomenclature[, c("IDname", "IDtaxon", setdiff(names(nomenclature), c("IDname", "IDtaxon")))]
taxons <- taxons[, c("IDtaxon", setdiff(names(taxons), "IDtaxon") )]
occurrences <- occurrences[, c( "IDoccurrence", setdiff(names(occurrences), "IDoccurrence"))]


#set primary keys
dbCreateTable(con, "taxons", taxons, overwrite = TRUE,
              field.types = c(IDtaxon = "INTEGER PRIMARY KEY"))
dbAppendTable(con, "taxons", taxons)

dbCreateTable(con, "nomenclature", nomenclature,
              field.types = c(IDname = "INTEGER PRIMARY KEY", IDtaxon = "INTEGER REFERENCES taxons(IDtaxon)",
                              statut = "VARCHAR CHECK (statut IN ('accepté', 'complexe', 'synonyme', 'à déterminer'))",
                              taxonRank = "VARCHAR CHECK (taxonRank IN ('complexe', 'form', 'genus', 'species', 'subspecies', 'variety')",
                              IDnameAccUsage="INTEGER"
                              ))
dbAppendTable(con, "nomenclature", nomenclature)

dbCreateTable(con, "occurrences", occurrences,
              field.types = c(IDoccurrence = "INTEGER PRIMARY KEY", IDtaxon = "INTEGER REFERENCES nomenclature(IDtaxon)",
                              IDname="INTEGER REFERENCES nomenclature(IDname)"))
dbAppendTable(con, "occurrences", occurrences)

#create new lines
newIDtaxon <- dbGetQuery(con, "SELECT COALESCE(MAX(IDtaxon), 0) + 1 AS IDtaxon from taxons")$IDtaxon
now <- as.Date(Sys.time())

#Brachydontium trichodes
newTaxon <- data.table(IDtaxon = newIDtaxon, IDnameForExport = NA_integer_,
                       etatTaxon = "actif",
                       dateModif = now, dateCreation = now,
                       etatTaxonCommentaire = "Première mention de cette mousse pour le Québec récoltée le 2025-09-28")

dbAppendTable(con, "taxons", newTaxon)

newIDname <- dbGetQuery(con, "SELECT COALESCE(MAX(IDname), 0) + 1 AS IDname from nomenclature")$IDname
now <- as.Date(Sys.time())

newName<-data.table(IDname = newIDname, IDtaxon=newIDtaxon,
                    IDnameAccUsage = newIDname, 
                    statut = "accepté",
                    taxonRank="species",
                    genus_name="Brachydontium", specificEpithet_name = "trichodes", 
                    infraspecificEpithet_name=NA_character_, nameAccordingTo=NA_character_,
                    scientificNameAuthorshipSpecific="(F. Weber) Milde ")
dbAppendTable(con, "nomenclature", newName)


dbExecute(con, "UPDATE taxons SET IDnameForExport = ? WHERE IDtaxon = ?", params = list(newIDname, newIDtaxon))


#dbExecute(con, "UPDATE taxons SET IDnameForExport = ?, dateModif = ? WHERE IDtaxon = ?", params = list("Updated name", Sys.time(), 123))
#vérifier que IDname et IDtaxon existent
dbGetQuery(con, "SELECT IDname, IDtaxon FROM nomenclature WHERE IDname = ?", params = list(newIDname))

#valider les ajouts
dbGetQuery(
  con,
  "SELECT * FROM nomenclature WHERE genus_name = 'Brachydontium' LIMIT 10"
)

dbGetQuery(
  con,
  "SELECT * FROM taxons WHERE IDtaxon = 1123 LIMIT 10"
)


