library(DT)
library(leaflet)
library(RColorBrewer)
library(RPostgres)
library(scales)
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(treemap)

# connection to the hotelsV database----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'hotels3',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25061,
  user = 'proj3',
  password = 'j0qm3rmlr1b7f8mf',
  sslmode = 'require'
)


# hotels table----
hot <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM hotels ORDER BY hotel_name'
)

att <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM attractions ORDER BY attr_name'
)

res <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM restaurants ORDER BY rest_name'
)

own <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM owners'
)

bok <- dbGetQuery(
  conn = con,
  statement = 'SELECT * FROM bookings ORDER BY hotel_name'
)

# distance between two coordinates - Haversine formula----
dist <- function(lat1, lng1, lat2, lng2) {
  r <- 6378.1 #radius of earth in km
  f1 <- lat1 * pi / 180
  f2 <- lat2 * pi / 180
  d1 <- (lat2 - lat1) * pi / 180
  d2 <- (lng2 - lng1) * pi / 180
  a <- sin(d1 / 2) ^ 2 + cos(f1) * cos(f2) * sin(d2 / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(round(r * c, 2))
}

# when exiting app, disconnect from the kpop database
onStop(
  function()
  {
    dbDisconnect(con)
  }
)