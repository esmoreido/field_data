Sys.setlocale(category = "LC_ALL", locale = "ru_RU.UTF-8")
library(tidyverse)
library(dbplyr)
library(lubridate)
library(stringi)
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


st <- dbGetQuery(con, "SELECT * FROM field_data ORDER BY change DESC LIMIT 100")
st

rm <- paste0("DELETE FROM field_data")
rm
dbExecute(con, rm)

st_name <- "Ольмесхыр"
source('source/helpers_funs.R')
df <- read.weatherlink(filename = 'sample.txt', 
                       station_name = enc2utf8(st_name))
df$type <- '1' # 1 - метеостанция, 2 - логгер уровня и температуры, 3 - логгер электропроводности и температуры
df <- df %>%
  mutate(pres_mm = Bar * 0.75006150504341, 
         WindDir = as.integer(factor(WindDir, ordered = T)), 
         HiDir = as.integer(factor(HiDir, ordered = T)))


df <- df %>%
  pivot_longer(cols = !c(DateTime, station_name, type),
               names_to = 'variable', values_to = 'value')

# запрос на добавление данных из файлов
q <- gsub("[\r\n\t]", "",
          paste0(c("INSERT INTO field_data (station, datetime, variable,
                   value, type, change, source) VALUES ",
                   paste0("('", df$station_name,  "','",
                          df$DateTime,"','",
                          trimws(df$variable), "','",
                          df$value,"','", df$type,"','",
                          today(), "', 'shiny_app')",
                          collapse = ','), " ON CONFLICT DO NOTHING"),
                 collapse = ""))
q
q <- gsub("\'NA\'", "NULL", q)
res <- dbSendStatement(con, q)
dbGetRowsAffected(res)
dbGetQuery(con, "SELECT * FROM field_data")


dbListFields(con, "field_site")

df <- dbGetQuery(con, "SELECT * FROM field_site WHERE type = 1")
paste0("'", paste(df$name, df$id, sep = "' = '", collapse = "', '"), "'")

# загрузка информации по логгерам
library(sf)
meteo <- st_read('D:/YandexDisk/ИВПРАН/крым/данные/метеостанции/iwp_meteostation.shp', 
                 crs = 4326, options = "ENCODING=UTF-8")

q <- paste0("INSERT INTO field_site (name, type, lon, lat) VALUES ('", 
            paste(meteo$name, '1', meteo$lon, meteo$lat, sep = "','", collapse = "'), ('"), "') ON CONFLICT DO NOTHING")
q
qry <- dbSendQuery(con, q)
dbGetRowsAffected(qry)
dbGetQuery(con, "SELECT * FROM field_site")

hydro <- st_read('D:/YandexDisk/ИВПРАН/крым/данные/логгеры/iwp_loggers.shp', crs = 4326)
hydro <- hydro %>%
  mutate(namefull = paste(Watobj, Site, sep = ', '))
q <- paste0("INSERT INTO field_site (name, type, lon, lat) VALUES ('", 
            paste(hydro$namefull, '2', hydro$lon, hydro$lat, sep = "','", collapse = "'), ('"), "') ON CONFLICT DO NOTHING")
q
qry <- dbSendQuery(con, q)
dbGetRowsAffected(qry)
dbGetQuery(con, "SELECT * FROM field_site")


dbDisconnect(con)


