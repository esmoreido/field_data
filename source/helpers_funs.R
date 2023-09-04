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

read.weatherlink <- function(){
  filename = input$file1$datapath
  station_name = input$station_name
  graph_title = 'График измеренных метеовеличин' 
  date_breaks = '1 day'
  minor_date_breaks = '1 hour'
  xls = F 
  baro = F
  amdate = input$amdate
  print(amdate)
  long = F
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
  
  # фильтр NA-дат 
  df <- df %>%
    filter(!is.na(DateTime))
  
  # убираем лишние пробелы в названиях
  colnames(df) <- trimws(colnames(df))
  
  # добавляем колонку с давлением в мм рт.ст., меняем номинальные переменные 
  # на рациональные
  df <- df %>%
    mutate(pres_mm = Bar * 0.75006150504341, 
           WindDir = as.integer(factor(WindDir, ordered = T)), 
           HiDir = as.integer(factor(HiDir, ordered = T)))
  
  # убираем старые дату и время
  df$station_name <- station_name
  df <- dplyr::select(df, -c(Date, Time))
  
  return(df)
} 