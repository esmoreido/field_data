Sys.setlocale(category = "LC_ALL", locale = "russian")
library(tidyverse)
library(dbplyr)
library(lubridate)
library(stringi)
library(RPostgreSQL)
# соединение ----
mypaw <- {
  "8IktF3go"
}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "hydromet",
                   host = "192.168.5.203", port = 5432,
                   user = "shiny_app", password = mypaw)
rm(mypaw)
dbListTables(con)
dbListFields(con, 'field_var_unit')
dbGetQuery(con, "SHOW CLIENT_ENCODING")

st <- dbGetQuery(con, "SELECT DISTINCT station FROM field_data")
st
Encoding(st$station)
q <- paste0("SELECT datetime, variable, value FROM field_data WHERE variable IN ('Rain', 'TempOut') AND station IN ('", 
            st$station, "') ORDER BY datetime")
Encoding(q)
q

res <- dbGetQuery(con, q)

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
df <- dbGetQuery(con, "SELECT * FROM gmvo_data WHERE index IN (70001,70002,70004,70007,70011,70842) AND variable_id = 2")

ggplot(df, aes(x=date, y=value, col=factor(index))) + geom_line()

dbListFields(con, "field_site")

df <- dbGetQuery(con, "SELECT * FROM field_data WHERE type = 1")
paste0("'", paste(df$name, df$id, sep = "' = '", collapse = "', '"), "'")



# загрузка информации по логгерам ----
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
dbGetQuery(con, "SELECT * FROM field_device")


# загрузка мета информации по устройствам ----

q <- "INSERT INTO field_device_type (id, type) VALUES (1, 'Метеостанция'), (2, 'Гидропост')"
dbExecute(con, enc2utf8(q))
dbGetQuery(con, "SELECT * FROM field_device_type")

# загрузка мета информации по переменным ----
var_names <- c("Temp_Out", "Hi_Temp", "Low_Temp", "Out_Hum", "Dew_Pt", 
               "Wind_Speed", "Wind_Dir", "Wind_Run", "Hi_Speed", "Hi_Dir", 
               "Wind_Chill", "Heat_Index", "THW_Index", "THSW_Index", "Bar", 
               "Rain", "Rain_Rate", "Solar_Rad", "Solar_Energy", "HiSolar_Rad", 
               "Heat_D-D", "Cool_D-D", "In_Temp", "In_Hum", "In_Dew", "In_Heat", 
               "In_EMC", "InAir_Density", "ET", "Soil1_Moist", "Soil2_Moist", 
               "Soil3_Moist", "Soil4_Moist", "Soil_Temp1", "Soil_Temp2", 
               "Soil_Temp3", "Soil_Temp4", "Wind_Samp", "Wind_Tx", 
               "ISS_Recept", "Arc_Int")
var_unit_en <- c("°С", "°С", "°С", "%", "°С", "m/s", "rumb", "m/s", "m/s", 
                 "rumb", "°С", "°С", "°С", "°С", "mBar", "mm", "mm/h", "W/m2", 
                 "lng", "W/m2", "°С", "°С", "°С", "%", "°С", "°С", "%", "kg/m3",
                 "mm", "%", "%", "%", "%", "°С", "°С", "°С", "°С", "num", "num",
                 "%", "num")
var_unit_ru <- c("°С", "°С", "°С", "%", "°С", "м/с", "румб", "м/с", "м/с", 
                 "румб", "°С", "°С", "°С", "°С", "мБар", "мм", "мм/ч", "В/м2", 
                 "лнг", "В/м2", "°С", "°С", "°С", "%", "°С", "°С", "%", "кг/м3", 
                 "мм", "%", "%", "%", "%", "°С", "°С", "°С", "°С", "шт", "шт", 
                 "%", "шт")
var_type <- c(rep('numeric', 6), 'character', rep('numeric', 2), 'character', 
                           rep('numeric', 31))
var_use <- c(rep(1, 13), 0, rep(1, 15), rep(0, 8),rep(1, 4))

var_df <- data.frame(var_names, var_unit_en, var_unit_ru, var_type, var_use)
var_df$var_device_unit <- 1

q <- gsub("[\r\n\t]", "",
          paste0(c("INSERT INTO field_var_unit (var_device_type, var_name, var_unit_en, 
                   var_unit_ru, var_type, var_use) VALUES ",
                   paste0("('", var_df$var_device_unit,  "','",
                          var_df$var_name,  "','",
                          var_df$var_unit_en,"','",
                          var_df$var_unit_ru,"','",
                          var_df$var_type,"','",
                          var_df$var_use, "')",
                          collapse = ','), " ON CONFLICT DO NOTHING"),
                 collapse = ""))
q
dbExecute(con, enc2utf8(q))
dbGetQuery(con, "SELECT * FROM field_var_unit")

# добавление данных с логгеров в типы данных ----
# q <- gsub("[\r\n\t]", "",
#           "INSERT INTO field_var_unit (var_device_type, var_name, var_unit_en, 
#                    var_unit_ru, var_type, var_use) VALUES
#           (2, 'water_temp_u20', '°С', '°С', 'numeric', 1),
#           (2, 'water_temp_u24', '°С', '°С', 'numeric', 1),
#           (2, 'water_pres_u20', 'kPa', 'кПа', 'numeric', 1),
#           (2, 'water_cond_u24', 'μS/cm', 'μС/см', 'numeric', 1)
#           ON CONFLICT DO NOTHING")
q <- gsub("[\r\n\t]", "",
          "INSERT INTO field_var_unit (var_device_type, var_name, var_unit_en, 
                   var_unit_ru, var_type, var_use) VALUES
          (1, 'Pres_mm', 'mm hg', 'мм рт.ст.', 'numeric', 1)
          ON CONFLICT DO NOTHING")
q
dbExecute(con, enc2utf8(q))
dbGetQuery(con, "SELECT * FROM field_var_unit")

var_name <- dbGetQuery(con, "SELECT id, var_name FROM field_var_unit WHERE var_device_type = 2 ORDER BY id")
hobo_data_type <- 1
if(hobo_data_type == 1){
  var_name <- var_name %>%
    filter(grepl('u20', var_name))
}else{
  var_name <- var_name %>%
    filter(grepl('u24', var_name))
}
coln <- c('N', 'datetime', var_name$var_name[1], var_name$var_name[2], 'cd', 'ca', 'hc', 'eof')
coln

# проверка выгрузки данных с объединением ----

q <- gsub("[\r\n\t]", "", 
     "SELECT field_data.datetime, field_site.name,  field_data.variable, field_data.change, field_data.source,  field_data.value  
      FROM field_data  
      LEFT JOIN field_site ON field_data.station::integer=field_site.id  
      WHERE field_data.variable IN ('Temp_Out')  
      AND field_data.station IN ('1', '2', '4')  
      ORDER BY datetime")
q
df <- dbGetQuery(con, q)

ggplot(df, aes(x = datetime, y = value, col=variable)) + geom_line() +
  facet_wrap(variable~name, ncol = 1, scales = 'free_y', strip.position = 'right') +
  scale_x_datetime(date_labels = "%d.%m.%y") +
  theme_light(base_size = 16) +
  theme(legend.position = 'top') + 
  labs(x='Дата', y='', col='')

# запросы на удаление по дате и источнику ----
dbListFields(con, 'field_data')
q <- gsub("[\r\n\t]", "", 
          "SELECT DISTINCT field_data.change, field_data.source, field_site.name, 
          count(value) as nval
      FROM field_data  
      LEFT JOIN field_site ON field_data.station::integer=field_site.id  
      GROUP BY field_data.change, field_data.source, field_site.name
      ORDER BY field_data.change")
q
df <- dbGetQuery(con, q)


# разъединение ----
dbDisconnect(con)


