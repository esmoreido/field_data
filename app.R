library(shiny)
# library(shinyjs)
library(shinyWidgets)
library(stringi)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RPostgreSQL)
library(tidyverse)


# Основной контейнер приложения ----
ui <- navbarPage(title = "КрымДанные", footer = div(class = "footer", includeHTML("footer.html")), 
                 fluid = T, windowTitle = "КрымДанные", lang = "ru",
                 
                 # Панель перечня станций и постов ----
                 tabPanel(title = "Станции",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            
                            # App title 
                            titlePanel("Перечень станций и постов"),
                    
                              
                          
                 )),
                 # Панель загрузки данных ----
                 tabPanel(title = "Загрузка с метеостанции",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),

                            # App title 
                            titlePanel("Загрузка файлов с метеостанций"),
                            
                            # Боковая панель ----
                            sidebarLayout(
                              
                              # Добавление файла 
                              sidebarPanel(
                                
                                uiOutput('ui_st_import'),
                                
                                # Input: Select a file 
                                fileInput("file1", "Выбрать файл",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"),
                                          buttonLabel = "Выбрать...",
                                          placeholder = "Файл не выбран"),
                                checkboxInput(inputId = "amdate", 
                                              label = "Американский формат даты и времени мм/дд/гг 12 ч.", 
                                              value = F
                                             ),
                                checkboxInput(inputId = "amunit", 
                                              label = "Американские единицы (°F, in.)", 
                                              value = F
                                ), 
                                checkboxInput(inputId = "pres_mm", 
                                                 label = "мБар в мм.рт.ст.", 
                                                 value = F
                                ),
                                # Horizontal line 
                                tags$hr(),
                                
                                # Insert button 
                                actionButton("insert_df", "Загрузить"),
                                actionButton("reset1", "Очистить")
                                
                              ),
                              
                              # Основная панель таблицы ----
                              mainPanel(
                                
                                # Вывод таблицы и результатов добавления ----
                                div(dataTableOutput("contents"), style = "font-size:80%"),
                                h2(textOutput("qry", inline = T))
                              )
                              
                            )
                          )
                 ),
                 # Панель графика ----
                 tabPanel(title = "Просмотр",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            
                            # Заголовок
                            titlePanel("Просмотр данных"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput('ui_stations'),
                                uiOutput('ui_var'),
                                selectInput("group", "Группировка", 
                                            choices = c('Без группировки'='nogroup', 
                                                        'По переменным'='group_var', 
                                                        'По станциям'='group_stat')),
                                actionButton("plot_graph", "Создать"),
                                actionButton("reset2", "Очистить")
                              ),
                              mainPanel(
                                div(style='overflow: scroll', 
                                    plotOutput('plotdata')),
                                div(dataTableOutput("datatable"), style = "font-size:80%")
                              )
                            )
                          ))
)


# Сервер ----
server <- function(input, output, session) {
  source('source/helpers_funs.R', local = T, encoding = 'UTF-8')
  
  # mypaw <- {
  #   "vnFkY9Vj"
  # }
  # drv <- dbDriver("PostgreSQL")
  # Для панели загрузки данных из файлов ----
  # Соединение с базой ----
  con <- db_connect()
  # tryCatch({
  #   con <- dbConnect(drv, dbname = "hydromet",
  #                    host = "192.168.5.203", port = 5432,
  #                    user = "moreydo", password = mypaw)
  #   print('Connected')
  # },
  #   error = function(e){
  #     stop(safeError(e))
  #     output$qry <- renderText("Error connecting to database!")
  # })
  # rm(mypaw)
  
  # Перезагрузка приложения с кнопки ----
  observeEvent(c(input$reset1,input$reset2), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  # Получение из БД списка метеостанций для добавления в таблицу ----
  output$ui_st_import <- get_weather_station_list_selectInput(1)
  
  # Основная таблица данных ----
  input_df <- reactive({
    req(input$file1)
    validate(need(tools::file_ext(input$file1$datapath) == c("csv", "txt", "asc"), 
                  "Пожалуйста, загрузите текстовый файл (txt, csv, asc)"))
    
    tryCatch(
      {
        df <- read.weatherlink() # Внешняя функция чтения файла Davis
      },
      error = function(e) stop(safeError(e))
    )
  })
  
  # Вывод таблицы с загруженным файлом для просмотра ----
  output$contents <- renderDataTable(
    input_df(), 
    options = list(pageLength = 10, 
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # Загрузка по нажатию кнопки ----
  observeEvent(input$insert_df, {
    
    output$qry <- renderText({
      df <- input_df() %>%
        # dplyr::select(!c(Date, Time)) %>%
        pivot_longer(cols = !c(DateTime, station_name),
                     names_to = 'variable', values_to = 'value')
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
  })
  
  # Для панели графики ---- 
  # Получение из БД списка метеостанций для графики ----
  output$ui_stations <- get_weather_station_list_selectInput(1)
  
  # Получение из БД списка переменных ----
  output$ui_var <- renderUI({
    var <- dbGetQuery(con, "SELECT DISTINCT variable FROM field_data")
    pickerInput('pick_var',
                label ='Данные по оси X',
                choices=var$variable,
                selected = NULL, 
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Отменить",
                               `select-all-text` = "Выбрать всё",
                               `none-selected-text` = "Выберите..."), 
                multiple = T)
  })
  
  # Таблица для графика и вывода ----
  plot_df <- reactive({
    req(input$pick_station, input$pick_var)
    q <- paste0("SELECT datetime, station, variable, change, source, value FROM field_data WHERE variable IN (\'", 
                paste0(input$pick_var, collapse = '\', \''), "\') AND station IN ('",
                paste0(input$pick_station, collapse = '\', \''),"')  ORDER BY datetime")
    print(q)
    tryCatch({
      df <- dbGetQuery(con, q)
    },
    error = function(e) return(e)
    )
    
  })
  
  # График ----
  observeEvent(input$plot_graph, {
    output$plotdata <- renderPlot({
      withProgress(expr = {
        switch(input$group, 
               nogroup = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=variable)) + geom_line() +
                   facet_wrap(variable~station, ncol = 1, scales = 'free_y', strip.position = 'right') +
                   scale_x_datetime(date_labels = "%d.%m.%y") +
                   theme_light(base_size = 16) +
                   theme(legend.position = 'top') + 
                   labs(x='Дата', y='', col='')
                 },
               group_var = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=variable)) + geom_line() +
                   facet_wrap(.~station, ncol = 1, scales = 'free_y', strip.position = 'right') +
                   scale_x_datetime(date_labels = "%d.%m.%y") +
                   theme_light(base_size = 16) +
                   theme(legend.position = 'top') + 
                   labs(x='Дата', y='', col='')
               },
               group_stat = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=station)) + geom_line() +
                   facet_wrap(.~variable, ncol = 1, scales = 'free_y', strip.position = 'right') +
                   scale_x_datetime(date_labels = "%d.%m.%y") +
                   theme_light(base_size = 16) +
                   theme(legend.position = 'top') + 
                   labs(x='Дата', y='', col='')
               }
        )}, message = "Загрузка...")
    })
    # Вывод ----
    output$datatable <- renderDataTable(
      plot_df() %>%
        pivot_wider(id_cols = c('datetime', 'change', 'source'), names_from = c('station', 'variable'), values_from = 'value'),
      options = list(pageLength = 100, 
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  # Разрыв соединения с БД ----
  session$onSessionEnded(function() {
  #   lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
    dbDisconnect(con)
    print('Disconnected')
  })
  
}
# Выполнение приложения ----
shinyApp(ui, server)