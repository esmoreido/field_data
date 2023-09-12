library(shiny)
# library(shinyjs)
library(shinyWidgets)
library(stringi)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RPostgreSQL)
library(tidyverse)
# library(leaflet)


# Основной контейнер приложения ----
ui <- navbarPage(title = "КрымДанные", footer = div(class = "footer", includeHTML("footer.html")), 
                 fluid = T, windowTitle = "КрымДанные", lang = "ru",
                 
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
                                checkboxInput(inputId = "soil_data", 
                                              label = "Есть температура и влажность почвы", 
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
                          )
                 ),
                 
                 # Панель перечня станций и постов ----
                 tabPanel(title = "Станции",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            verticalLayout(
                              titlePanel("Расположение объектов наблюдательной сети"),
                              # leafletOutput("stations_map"),
                              wellPanel(
                                div(dataTableOutput("stations"), style = "font-size:80%")
                              )
                            )
                          )
                          
                          
                          
                 ),
)


# Сервер ----
server <- function(input, output, session) {
  source('source/helpers_funs.R', local = T, encoding = 'UTF-8')
  
  # соединение с БД
  con <- db_connect()
  
  # Перезагрузка приложения с кнопки ----
  observeEvent(c(input$reset1,input$reset2), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  # Получение из БД списка метеостанций для добавления в таблицу ----
  output$ui_st_import <- get_weather_station_list_selectInput(1, F)
  
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
    
    output$qry <- db_insert_weather()
    
  })
  
  # Для панели графики ---- 
  # Получение из БД списка станций для графики ----
  output$ui_stations <- get_weather_station_list_selectInput(NULL, T)
  
  # Получение из БД списка переменных ----
  output$ui_var <- get_plot_vars()
  
  # Таблица для графика и вывода ----
  plot_df <- reactive({
    req(input$station_id, input$pick_var)
    q <- gsub("[\r\n\t]", "", 
              paste0("SELECT field_data.datetime, field_site.name, 
                field_data.variable, field_data.change, field_data.source, 
                field_data.value 
                       FROM field_data 
                       LEFT JOIN field_site
                       ON field_data.station::integer=field_site.id
                       WHERE field_data.variable IN (\'", 
                     paste0(input$pick_var, collapse = '\', \''), "\') 
                AND field_data.station IN ('",
                     paste0(input$station_id, collapse = '\', \''),"')  ORDER BY datetime")
    )
    print(q)
    tryCatch({
      df <- dbGetQuery(con, q)
    },
    error = function(e) return(e)
    )
    return(df)
  })
  
  # График ----
  observeEvent(input$plot_graph, {
    output$plotdata <- renderPlot({
      withProgress(expr = {
        switch(input$group, 
               nogroup = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=variable)) + geom_line() +
                   facet_wrap(variable~name, ncol = 1, scales = 'free_y', strip.position = 'right') +
                   scale_x_datetime(date_labels = "%d.%m.%y") +
                   theme_light(base_size = 16) +
                   theme(legend.position = 'top') + 
                   labs(x='Дата', y='', col='')
               },
               group_var = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=variable)) + geom_line() +
                   facet_wrap(.~name, ncol = 1, scales = 'free_y', strip.position = 'right') +
                   scale_x_datetime(date_labels = "%d.%m.%y") +
                   theme_light(base_size = 16) +
                   theme(legend.position = 'top') + 
                   labs(x='Дата', y='', col='')
               },
               group_stat = {
                 ggplot(plot_df(), aes(x = datetime, y = value, col=name)) + geom_line() +
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
        pivot_wider(id_cols = c('datetime', 'change', 'source'), names_from = c('name', 'variable'), values_from = 'value'),
      options = list(pageLength = 100, 
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  
  # Карта ----
  stations_df <- reactive({
    pts <- dbGetQuery(con, "SELECT * FROM field_site")
    # print(pts)
    return(pts)
  })
  
  output$stations_table <- renderDataTable(stations_df(), 
                                           options = list(pageLength = 25,
                                                          language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  output$stations_map <- renderLeaflet({
    typepal <- colorFactor(palette = c('red', 'blue'), domain = stations_df()$type)
    leaflet(stations_df()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, fillOpacity = 1,
                       label = ~name, labelOptions = labelOptions(noHide = T), 
                       fillColor = ~typepal(type), 
                       stroke = F, 
                       clusterOptions = markerClusterOptions()) %>%
      addLegend(colors = c('red', 'blue'), values = ~type, title = '', opacity = 1,
                labels = c('Метеостанции', 'Гидропосты'))
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