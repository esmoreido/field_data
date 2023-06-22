Sys.setlocale("LC_ALL","russian")
library(dplyr)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "hydromet",
                 host = "192.168.5.203", port = 5432,
                 user = "moreydo", password = "vnFkY9Vj")
dbListFields(con, "field_data")
