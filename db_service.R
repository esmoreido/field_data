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
dbGetQuery(con, "SHOW CLIENT_ENCODING")

st <- dbGetQuery(con, "SELECT DISTINCT station FROM field_data")
st
Encoding(st$station)
q <- paste0("SELECT datetime, variable, value FROM field_data WHERE variable IN ('Rain', 'TempOut') AND station IN ('", 
            st$station, "') ORDER BY datetime")
Encoding(q)
q

res <- dbGetQuery(con, q)


rm <- "DELETE FROM field_data"
dbExecute(con, rm)
dbDisconnect(con)


