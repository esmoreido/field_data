Sys.setlocale(category = "LC_ALL", locale = "Russian")
library(ggplot2)
library(lubridate)
library(reshape2)
library(RPostgreSQL)
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
  amunit = input$amunit
  pres_mm = input$pres_mm
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
  
  # переводим из американских единиц в СИ
  if(amunit == T){
    df <- df %>%
      mutate(across(contains(c('Temp', 'Dew', 'Chill', 'Heat', 'Index')), 
                    ~ round((.x - 32)/1.8, 1)), .keep=c("unused"))
    df <- df %>%
      mutate(across(contains(c('Rain', 'Bar')), ~ .x * 25.4), .keep=c("unused"))
    if(pres_mm == T){
      df <- df %>%
        mutate(pres_mm = Bar)
    }
  }else{
    if(pres_mm == T){
      df <- df %>%
        mutate(pres_mm = Bar * 0.75006150504341)
    }
  }
  
  
  
  # меняем номинальные переменные на рациональные
  df <- df %>%
    mutate(WindDir = as.integer(factor(WindDir, ordered = T)), 
           HiDir = as.integer(factor(HiDir, ordered = T)))
  
  # убираем старые дату и время
  df$station_name <- station_name
  df <- dplyr::select(df, -c(Date, Time))
  
  return(df)
} 

# Получение из БД списка метеостанций для добавления в таблицу ----
get_weather_station_list_selectInput <- function(st_type = 1){
  renderUI({
    q <- paste0("SELECT DISTINCT name, id FROM field_site WHERE type = ", st_type)
    st_list <- dbGetQuery(con, q)
    st_choice <- as.list(st_list$id)
    names(st_choice) <- st_list$name
    selectInput('station_id',
                label = enc2native('Метеостанции'),
                choices = st_choice,
                selected = NULL, multiple = F)
  })
}

db_connect <- function(){
  mypaw <- {
    "vnFkY9Vj"
  }
  drv <- dbDriver("PostgreSQL")
  tryCatch({
    con <- dbConnect(drv, dbname = "hydromet",
                     host = "192.168.5.203", port = 5432,
                     user = "moreydo", password = mypaw) # заменить на новый логин и пароль для приложения
    print('Connected')
  },
  error = function(e){
    stop(safeError(e))
    output$qry <- renderText("Ошибка соединения с базой!")
  })
  rm(mypaw)
  return(con)
}