Sys.setlocale(category = "LC_ALL", locale = "Russian")
library(ggplot2)
library(ggthemes)
library(lubridate)
library(reshape2)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(gridExtra)

read.weatherlink <- function(filename, station_name = paste('Метеостанция', today()), 
                             graph_title = 'График измеренных метеовеличин', 
                             date_breaks = '1 day', 
                             minor_date_breaks = '1 hour',
                             xls = F, baro = F, amdate = amdate, long = F){
  # считываем данные
  df <- read.csv(filename, sep = '\t', header = F,
                 check.names = F, stringsAsFactors = F,
                 col.names = c("Date",	"Time",	"TempOut",	"HiTemp",	"LowTemp",	"OutHum",	"DewPt.",	"WindSpeed",	"WindDir",	"WindRun",	"HiSpeed",	"HiDir",	"WindChill",	"HeatIndex",	"THWIndex",	"Bar  ",	"Rain",	"RainRate",	"HeatD-D ",	"CoolD-D ",	"In Temp",	"InHum",	"In Dew",	"In Heat",	"In EMC",	"In AirDensity",	"WindSamp",	"WindTx ",	"ISS Recept",	"Arc.Int."),
                 skip = 2, na.strings = '---',
                 colClasses = c('character', 'character', 
                                rep('numeric', 6), 'character', 'numeric', 'numeric', 'character', 
                                rep('numeric', 18)))
  if(amdate == T){
    # если формат даты-времени американский, меняем обозначения времени "а" и "р" на "AM" и "PM"
    df$Time <- gsub(pattern = "[a]$", x = df$Time, replacement = "AM")
    df$Time <- gsub(pattern = "[p]$", x = df$Time, replacement = "PM")
    
    # делаем дату-время из частей
    df <- df %>%
      mutate(DateTime = parse_date_time(paste(Date, Time), "%m/%d/%y %I:%M %p", tz = 'GMT')) %>%
      relocate(DateTime)
  }else{
    df <- df %>%
      mutate(DateTime = parse_date_time(paste(Date, Time), "%d.%m.%y %H:%M", tz = 'GMT')) %>%
      relocate(DateTime)
  }
  
  # убираем лишние пробелы в названиях
  colnames(df) <- trimws(colnames(df))
  
  # добавляем колонку с давлением в мм рт.ст.
  df <- df %>%
    mutate(pres_mm = Bar * 0.75006150504341) 
  
  # убираем старые дату и время
  df$station_name <- station_name
  df <- dplyr::select(df, -c(Date, Time))
  
  # экспорт в excel
  if(xls == T){
    write_xlsx(df, path = paste0(station_name, '.xlsx'))
  }
  # экспорт давления для барокомпенсации
  if(baro == T){
    baro_df <- df %>%
      dplyr::select('DateTime', 'pres_mm') %>%
      filter(!is.na(DateTime)) %>%
      mutate(Date = format(date(DateTime), format = "%m/%d%/%y"),
             Time = format(DateTime, format = "%H:%M:%S")) %>%
      dplyr::select(Date, Time, pres_mm)
    write.table(file = paste('baro', station_name,
                             min(na.omit(date(df$DateTime))), 
                             max(na.omit(date(df$DateTime))), 
                             '.txt', sep = '_', collapse = '-'), 
                quote = F, 
                row.names = F, x = baro_df, sep = ',', 
                col.names = c('Date', 'Time', 'pres (mm Hg)'))
  }
  
  if(long == T){
    df <- melt(df, id.vars = c('DateTime', 'station_name'))
    df$datatype <- 'weather'
  }
  return(df)
} 