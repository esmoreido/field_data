library(dplyr)
library(lubridate)
library(RPostgreSQL)

mypaw <- {
  "vnFkY9Vj"
}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "hydromet",
                   host = "192.168.5.203", port = 5432,
                   user = "moreydo", password = mypaw)
rm(mypaw)
dbListTables(con)
dbListFields(con, 'field_data')

q <- "SELECT station, variable, count(*) from field_data GROUP BY station, variable"
res <- dbGetQuery(con, q)
res$variable[1]

rm <- "DELETE FROM field_data"
dbExecute(con, rm)
dbDisconnect(con)
