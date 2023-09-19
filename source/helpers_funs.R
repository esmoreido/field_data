Sys.setlocale(category = "LC_ALL", locale = "Russian")
library(ggplot2)
library(lubridate)
library(reshape2)
library(RPostgreSQL)
library(gridExtra)

# загрузка csv davis ----
read.weatherlink <- function(){
  filename = input$file1$datapath
  station_name = input$station_id
  graph_title = 'График измеренных метеовеличин' 
  date_breaks = '1 day'
  minor_date_breaks = '1 hour'
  xls = F 
  baro = F
  amdate = input$amdate
  amunit = input$amunit
  soil_data = input$soil_data
  pres_mm = input$pres_mm
  
  
  # names 
  if(soil_data == T){
    var_name_type <- dbGetQuery(con, "SELECT var_name, var_type FROM field_var_unit")
  }else{
    var_name_type <- dbGetQuery(con, "SELECT var_name, var_type FROM field_var_unit WHERE var_use = 1")
  }
  colnames_db <- c("Date",	"Time", var_name_type[[1]])
  colclasses_db <- c('character', 'character', var_name_type[[2]])
  print(colnames_db)
  print(colclasses_db)
  long = F
  # считываем данные 
  df <- read.csv(filename, sep = '\t', header = F,
                 check.names = F, stringsAsFactors = F, 
                 col.names = colnames_db,
                 skip = 2, na.strings = '---', 
                 colClasses = colclasses_db
  )
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
    mutate(across(contains('Dir'), ~as.integer(factor(., ordered = T))), .keep=c("unused"))
  # mutate(Wind_Dir = as.integer(factor(WindDir, ordered = T)), 
  #        Hi_Dir = as.integer(factor(HiDir, ordered = T)))
  
  # убираем старые дату и время
  df$station_name <- station_name
  df <- dplyr::select(df, -c(Date, Time))
  
  return(df)
} 

# получение из БД списка метеостанций для добавления в таблицу ----
get_weather_station_list_selectInput <- function(st_type = NULL, mult = F){
  renderUI({
    if(is.null(st_type)){
      q <- paste0("SELECT DISTINCT name, id FROM field_site")
    }else{
      q <- paste0("SELECT DISTINCT name, id FROM field_site WHERE type = ", st_type)
    }
    st_list <- dbGetQuery(con, q)
    st_choice <- as.list(st_list$id)
    names(st_choice) <- st_list$name
    selectInput('station_id', width = '350px',
                label = enc2native('Станции'),
                choices = st_choice,
                selected = NULL, multiple = mult)
  })
}

# получение из БД списка станций по которым есть данные в таблице ----
get_station_list_ui <- function(){
  renderUI({
      q <- gsub("[\r\n\t]", "",
                     paste0("SELECT DISTINCT fd.station, fs.name 
                            FROM field_data fd
                            LEFT JOIN field_site fs ON fd.station :: integer = fs.id
                            ORDER BY fs.name DESC"))
    st_list <- dbGetQuery(con, q)
    st_choice <- as.list(st_list$station)
    names(st_choice) <- st_list$name
    pickerInput(inputId = 'ui_stations',
                label ='Станции', width = '250px',
                choices=st_choice,
                selected = NULL,
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Отменить",
                               `select-all-text` = "Выбрать всё",
                               `none-selected-text` = "Выберите..."))
  })
}

# запрос на данные для графики ----
get_plot_vars <- function(){
  renderUI({
    var <- dbGetQuery(con, "SELECT var_name FROM field_var_unit")
    # print(var)
    pickerInput('pick_var',
                label ='Данные по оси X',
                choices=var$var_name,
                selected = NULL, 
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Отменить",
                               `select-all-text` = "Выбрать всё",
                               `none-selected-text` = "Выберите..."), 
                multiple = T)
  })
}

# функция соединения с базой данных ----
db_connect <- function(){
  mypaw <- {
    "8IktF3go"
  }
  drv <- dbDriver("PostgreSQL")
  tryCatch({
    con <- dbConnect(drv, dbname = "hydromet",
                     host = "192.168.5.203", port = 5432,
                     user = "shiny_app", password = mypaw) # заменить на новый логин и пароль для приложения
    print('Connected')
  },
  error = function(e){
    stop(safeError(e))
    output$qry <- renderText("Ошибка соединения с базой!")
  })
  rm(mypaw)
  return(con)
}

# запрос  на вставку в БД----
db_insert_weather <- function(){
  renderText({
    df <- input_df() %>%
      pivot_longer(cols = !c(DateTime, station_name),
                   names_to = 'variable', values_to = 'value')
    
    print(head(df))
    created_on <- now()
    data_source <- input$file1$name
    # print(data_source)
    type <- '1' # 1 - метеостанция, 2 - логгер уровня и температуры, 3 - логгер электропроводности и температуры
    n <- nrow(df)
    # print(n)
    # for (i in 1:100) {
    q <- gsub("[\r\n\t]", "",
              paste0(c("INSERT INTO field_data (station, datetime, variable,
                   value, type, change, source) VALUES ",
                       paste0("('", df$station_name,  "','",
                              df$DateTime,"','",
                              trimws(df$variable), "','",
                              df$value,"','", type,"','",
                              created_on, "', '", data_source, "')",
                              collapse = ','), " ON CONFLICT DO NOTHING"),
                     collapse = ""))
    
    # замена флага отсутствующих значений, чтобы запрос PostgreSQL не ругался
    q <- gsub("\'NA\'", "NULL", q)
    
    withProgress(expr = {
      qry <- dbSendStatement(con, q)}, message = "Добавление записей в таблицу, подождите...")
    res <- dbGetRowsAffected(qry)
    print(res)
    if(res > 0){
      return(paste("Для добавления подготовлено записей данных:", n , 
                   ". В таблицу добавлено записей данных: ", 
                   res, sep = "\n")) 
    }else{
      return("Новых данных для добавления \n в базу не обнаружено.")
    }
  })
}

