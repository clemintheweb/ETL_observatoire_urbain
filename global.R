library(bslib)
library(leaflet)
library(leaflet.extras)
library(DT)
library(RMariaDB)
library(DBI)
library(sf)
library(dplyr)
library(glue)
library(shiny)
library(shinycssloaders)
library(shinyjqui)
library(geojsonio)
library(geojsonsf)
library(rsconnect)

# Fonction de connexion sécurisée à la BDD
get_db_conn <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "observatoire_urbain",
    host = "localhost",
    user = "attendance_admin",
    password = "paqlab"
  )
}