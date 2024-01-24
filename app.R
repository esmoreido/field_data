library(shiny)
library(shinyjs)
library(shinyWidgets)
library(stringi)
library(dplyr)
# library(ggplot2)
library(dygraphs)
library(lubridate)
library(RPostgreSQL)
library(tidyverse)
library(DT)
library(readxl)
library(htmltools)
library(xts)
# library(reshape2)
# library(leaflet)
stl <- "display:inline-block; vertical-align:top"


# UI Основной контейнер приложения ----
ui <- navbarPage(id = 'mainpanel', title = "КрымДанные", footer = div(class = "footer", includeHTML("footer.html")), 
                 fluid = T, windowTitle = "КрымДанные", lang = "ru",
                 
                 # Панель загрузки данных с метеостанций ----
                 tabPanel(title = "Загрузка с метеостанции",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            
                            # App title 
                            titlePanel("Загрузка файлов с метеостанций"),
                                wellPanel(id = 'inputFile',
                                          # Input: Select a file 
                                div(style = stl, fileInput("file1", "Выбрать файл",
                                          multiple = FALSE, width = '350px',
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"),
                                          buttonLabel = "Выбрать...",
                                          placeholder = "Файл не выбран")),
                                div(style = stl, uiOutput('ui_st_import')),
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
                                actionButton("reset1", "Очистить"),
                                h2(textOutput("qry", inline = T))
                                ),
                            wellPanel(id = 'mainTable', style = "overflow-y:scroll; max-height: 600px",
                                # Вывод таблицы и результатов добавления ----
                                div(dataTableOutput("contents"), style = "font-size:80%")
                                )
                              
                            
                          )
                 ),
                 # Панель загрузки данных с логгеров HOBO ----
                 tabPanel(title = "Загрузка с логгеров HOBO",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            
                            # App title 
                            titlePanel("Загрузка с логгеров HOBO"),
                            wellPanel(id = 'inputFileHobo',
                                      div(style = stl, 
                                          fileInput("file2", "Выбрать файл",
                                                                 multiple = FALSE, width = '350px',
                                                                 accept = c("text/csv",".xlsx",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv",
                                                                            ".xlsx"),
                                                                 buttonLabel = "Выбрать...",
                                                                 placeholder = "Файл не выбран")),
                                      div(style = stl, 
                                          uiOutput('ui_hobo_import')),
                                      div(radioButtons(inputId = "hobo_data_type",
                                                    label = "Вид логгера", inline = T,
                                                   choices = c('Давление'='1', 'Электропроводность'='2'))),
                                      div(radioButtons(inputId = "hobo_xls",
                                                                    label = "Формат файла", inline = T,
                                                                    choices = c('txt/csv'='2', 'excel'='1'))),
                                      div(style = stl, checkboxInput(inputId = 'ninecol',  value = 0, 
                                                                     label = 'В файле 9 колонок данных')),
                                      div(style = stl, checkboxInput(inputId = 'hobo_header',  value = 0, 
                                                                     label = 'Есть первая строка с номером (Plot Title: XXXXXXX)')),
                            # Horizontal line 
                            tags$hr(),
                                      
                                      # Insert button 
                                      actionButton("insert_hobo_df", "Загрузить"),
                                      actionButton("reset4", "Очистить"),
                                      h2(textOutput("qry_hobo", inline = T))
                            ),
                            wellPanel(id = 'mainTable', 
                                      # Вывод таблицы и результатов добавления c HOBO ----
                                      div(style = "overflow-y:scroll; max-height: 600px",
                                          DTOutput("contents_hobo"), 
                                          style = "font-size:80%")
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
                            wellPanel(
                                div(style = stl, uiOutput('ui_stations')),
                                div(style = stl, uiOutput('ui_var')),
                                div(style = stl, 
                                    selectInput("group", width = '250px', selectize = T, "Группировка", 
                                            choices = c('Без группировки'='nogroup', 
                                                        'По переменным'='group_var', 
                                                        'По станциям'='group_stat'))),
                                div(style = "display:block;", 
                                    actionButton("plot_graph", "Создать"),
                                    downloadButton('download',"Скачать таблицу"),
                                actionButton("reset2", "Очистить"))
                              ),
                              wellPanel(
                                # div(style='overflow: scroll; max-height: 600px', 
                                #     plotOutput('plotdata')),
                                div(style='overflow: scroll', 
                                    htmlOutput('plotdata')),
                                div(dataTableOutput("datatable"), style = "font-size:80%; overflow-y:scroll; max-height: 600px")
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
                                div(dataTableOutput("st_table"), style = "font-size:80%")
                              )
                            )
                          )
                          
                          
                          
                 ),
                 # Панель удаления записей ----
                 tabPanel(id = 'deltab', title = "Редактирование",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            verticalLayout(
                              titlePanel("Редактирование записей"),
                              tags$p('Выберите строку таблицы для удаления записей и нажмите \"Удалить\", после этого нажмите \"Обновить\" и вернитесь на эту вкладку'),
                              # wellPanel(
                                div(DT::dataTableOutput('delete_table'),
                                    style = "font-size:80%"),
                                # verbatimTextOutput('delete_table_selection'),
                              div(actionButton("delete_records_source", "Удалить"),
                              actionButton("reset3", "Обновить")),
                              textOutput("deleted_records", inline = T)
                              # )
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
  observeEvent(c(input$reset1,input$reset2,input$reset3, input$reset4), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  # Получение из БД списка метеостанций для добавления в таблицу ----
  output$ui_st_import <- get_weather_station_list_selectInput(1, F, 'station_id')
  
  # Таблица данных с метеостанции ----
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
    return(df)
  })
  
  # Вывод таблицы с загруженным файлом для просмотра ----
  output$contents <- renderDT(
    input_df(), 
    options = list(pageLength = 10, 
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # Загрузка метео по нажатию кнопки ----
  observeEvent(input$insert_df, {
    
    output$qry <- db_insert_weather()
    
  })
  
  # Получение из БД списка логгеров для добавления в таблицу ----
  output$ui_hobo_import <- get_weather_station_list_selectInput(2, F, 'station_hobo_id')
  
  # Таблица данных с логгера ----
  input_df_hobo <- reactive({
    req(input$file2)
    validate(need(tools::file_ext(input$file2$datapath) == c("csv", "txt", "asc", "xlsx"), 
                  "Пожалуйста, загрузите текстовый файл (txt, csv, asc) или файл Excel (xlsx)"))
    
    tryCatch(
      {
        df <- read.hobo() # Внешняя функция чтения файла hobo
      },
      error = function(e) stop(safeError(e))
    )
  })
  
  # Вывод таблицы с загруженным файлом HOBO для просмотра ----
  output$contents_hobo <- renderDT(
    input_df_hobo(), 
    options = list(pageLength = 10,
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  # Загрузка HOBO по нажатию кнопки ----
  observeEvent(input$insert_hobo_df, {
    
    output$qry_hobo <- db_insert_hobo()
    
  })
  
  # Для панели графики ---- 
  # Получение из БД списка станций для графики ----
  output$ui_stations <- get_station_list_ui()
  
  # Получение из БД списка переменных ----
  output$ui_var <- get_plot_vars()
  
  # Таблица для графика и вывода ----
  plot_df <- reactive({
    req(input$ui_stations, input$pick_var)
    print(input$pick_var)
    print(input$ui_stations)
    q <- gsub("[\r\n\t]", "", 
              paste0("SELECT field_data.datetime, field_site.name, 
                field_var_unit.var_name, field_data.value 
                       FROM field_data 
                       LEFT JOIN field_site
                       ON field_data.station::integer=field_site.id
                       LEFT JOIN field_var_unit 
                       ON field_data.variable::integer = field_var_unit.id
                       WHERE field_data.variable IN (\'", 
                     paste0(input$pick_var, collapse = '\', \''), "\') 
                AND field_data.station IN ('",
                     paste0(input$ui_stations, collapse = '\', \''),"')  ORDER BY datetime")
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
    # output$plotdata <- renderPlot({
    #   withProgress(expr = {
    #     switch(input$group, 
    #            nogroup = {
    #              ggplot(plot_df(), aes(x = datetime, y = value, col=var_name)) + geom_line() +
    #                facet_wrap(var_name~name, ncol = 1, scales = 'free_y', strip.position = 'right') +
    #                scale_x_datetime(date_labels = "%d.%m.%y") +
    #                theme_light(base_size = 16) +
    #                theme(legend.position = 'top') + 
    #                labs(x='Дата', y='', col='')
    #            },
    #            group_var = {
    #              ggplot(plot_df(), aes(x = datetime, y = value, col=var_name)) + geom_line() +
    #                facet_wrap(.~name, ncol = 1, scales = 'free_y', strip.position = 'right') +
    #                scale_x_datetime(date_labels = "%d.%m.%y") +
    #                theme_light(base_size = 16) +
    #                theme(legend.position = 'top') + 
    #                labs(x='Дата', y='', col='')
    #            },
    #            group_stat = {
    #              ggplot(plot_df(), aes(x = datetime, y = value, col=name)) + geom_line() +
    #                facet_wrap(.~var_name, ncol = 1, scales = 'free_y', strip.position = 'right') +
    #                scale_x_datetime(date_labels = "%d.%m.%y") +
    #                theme_light(base_size = 16) +
    #                theme(legend.position = 'top') + 
    #                labs(x='Дата', y='', col='')
    #            }
    #     )}, message = "Загрузка...")
    # })
    
    output$plotdata <- renderUI({
      withProgress(expr = {
        df <- dcast(plot_df(), datetime~name+var_name, value.var = 'value')
        ts <- as.xts(df[,-1], 
                     order.by = as.POSIXct(df$datetime, 
                                           format = "%Y-%m-%d %H:%M:%S"))
        lst <- lapply(ts, function (x) dygraph(x, main = colnames(x), 
                                               group = 'plots', 
                                               width = 'auto', 
                                               height = 200) %>% 
                        dyRangeSelector() %>%
                        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")))
        # print(lst)
        res <- htmltools::tagList(lst)
        return(res)
        }, message = "Загрузка...")
    })
    # Вывод ----
    output$datatable <- renderDataTable(
      plot_df() %>%
        pivot_wider(id_cols = 'datetime', names_from = c('name', 'var_name'), values_from = 'value'),
      options = list(pageLength = 100, 
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  # Файл для скачивания ----
  output$download <- downloadHandler(
    filename = function(){"krymdata_output.csv"}, 
    content = function(fname){
      write.csv(pivot_wider(plot_df(), id_cols = c('datetime', 'source'), names_from = c('name', 'var_name'), values_from = 'value'), fname, sep = ";", quote = F, row.names = F, na = '-32968')
    }
  )
  # Карта и таблица станций ----
  stations_df <- reactive({
    q <- gsub("[\r\n\t]", "", 
              "SELECT fs.name, ft.type, fs.lon, fs.lat, fs.elev, fs.date_open, fs.date_closed
      FROM field_site fs
      LEFT JOIN field_device_type ft ON fs.type = ft.id
              ORDER BY ft.type")
    pts <- dbGetQuery(con, q)
    return(pts)
  })

  # Таблица с перечнем станций ----
  output$st_table <- renderDT(stations_df(), 
                              colnames = c('Название', 'Тип', 'Долгота, °', 
                                           'Широта, °', 'Высота, м', 
                                           'Дата открытия', 'Дата закрытия'),
                                  options = list(pageLength = 25,
                                                 language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  
  
  # Виджет с картой расположения станций (пока не работает на сервере) ----
  # output$stations_map <- renderLeaflet({
  #   typepal <- colorFactor(palette = c('red', 'blue'), domain = stations_df()$type)
  #   leaflet(stations_df()) %>%
  #     addTiles() %>%
  #     addCircleMarkers(lng = ~lon, lat = ~lat, fillOpacity = 1,
  #                      label = ~name, labelOptions = labelOptions(noHide = T),
  #                      fillColor = ~typepal(type),
  #                      stroke = F,
  #                      clusterOptions = markerClusterOptions()) %>%
  #     addLegend(colors = c('red', 'blue'), values = ~type, title = '', opacity = 1,
  #               labels = c('Метеостанции', 'Гидропосты'))
  # })
  
  
  
  stations_source <- reactive({
    q <- gsub("[\r\n\t]", "", 
              "SELECT DISTINCT fd.change, fd.source, fs.name, count(fd.value) as nv
                FROM field_data fd
                LEFT JOIN field_site fs ON fd.station::integer=fs.id
                GROUP BY fd.change, fd.source, fs.name
                ORDER BY fd.change")
    pts <- dbGetQuery(con, q)
    pts <- pts %>%
      arrange(desc(change))
    return(pts)
  })
  # Таблица с перечнем станций и источников для удаления ----
  output$delete_table <- renderDT(stations_source(), server = FALSE, 
                                  selection = 'single',
                                  colnames = c('Дата добавления', 'Источник', 'Станция', 'Кол-во записей'),
                                           options = list(pageLength = 25,
                                                          language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))

  # Удаление данных по нажатию кнопки ----
  observeEvent(input$delete_records_source, {
    q <- paste0("DELETE FROM field_data WHERE source = \'", 
                stations_source()[input$delete_table_rows_selected,'source'],
                "\' AND change = \'", 
                stations_source()[input$delete_table_rows_selected,'change'],
                "\'")
    qry <- dbSendStatement(con, q)
    res <- dbGetRowsAffected(qry)
    print(res)
    output$deleted_records <- renderText(paste0("Из базы удалено ", res, " записей."))
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