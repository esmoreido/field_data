library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
# library(leaflet)
library(dygraphs)
library(htmltools)
library(RPostgreSQL)
library(DT)
library(xts)

options(shiny.trace=FALSE)
options(shiny.fullstacktrace=FALSE)

stl <- "display:inline-block; vertical-align:top"

ui <- dashboardPage(skin = 'red', 
  dashboardHeader(title = "Field2DB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Обзор", tabName = "dashboard", icon = icon("map-location-dot")), # fontawesome.com
      menuItem("Загрузка", tabName = "upload", icon = icon("arrow-up-from-bracket")),
      menuItem("Просмотр", tabName = "explore", icon = icon("chart-line")),
      menuItem("Обслуживание", tabName = "edit", icon = icon("cog"))
      # tags$div(class = 'sticky_footer', 
      #          tags$a(href="href=\"https://www.iwp.ru/about/structure/otdel-gidrologii-rechnykh-basseynov/laboratoriya-gidrologii-navodneniy/\"", 
      #                 HTML("&copy; 2023-24 Лаборатория 
      #                 гидроинформатики
      #                 ИВП РАН"))
      # )
    )
  ),
  dashboardBody(
    includeCSS("www/field_data.css"),
    tabItems(
      # дэшборд ----
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("num_stat"),
                valueBoxOutput("dbsize"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(title = 'Карта расположения станций и постов', 
                    solidHeader = TRUE, height = 'auto',
                    # leafletOutput('stations_map')
                    htmlOutput("stations_iframe_map")
                    ),
                box(title = "Перечень станций", solidHeader = TRUE,
                    dataTableOutput("st_table"))
              ),
      ),
      # загрузка ----
      tabItem("upload",
              tabsetPanel(
                # метеостанции ----
                tabPanel(title = "Загрузка с метеостанции", icon = icon("cloud-sun-rain"),
                         fluidPage(
                           shinyjs::useShinyjs(),
                           shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                           titlePanel("Загрузка файлов с метеостанций"),
                           wellPanel(id = 'inputFile', 
                                     div(style = stl, fileInput("file1", "Выбрать файл",
                                                                multiple = FALSE, width = '350px',
                                                                accept = c("text/csv",
                                                                           "text/comma-separated-values,text/plain",
                                                                           ".csv"),
                                                                buttonLabel = "Выбрать...",
                                                                placeholder = "Файл не выбран")),
                                     div(style = stl, uiOutput('ui_st_import')),
                                     checkboxInput(inputId = "amdate", 
                                                   label = "Формат даты и времени мм/дд/гг 12 ч.", 
                                                   value = F
                                     ),
                                     checkboxInput(inputId = "amunit", 
                                                   label = "Неметрические единицы (°F, in.)", 
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
                                     actionButton("insert_df", "Загрузить", icon = icon("arrow-up-from-bracket")),
                                     actionButton("reset1", "Очистить",icon = icon("broom")),
                                     h2(textOutput("qry", inline = T))
                           ),
                           wellPanel(id = 'mainTable', style = "overflow-y:scroll; max-height: 600px",
                                     # Вывод таблицы и результатов добавления ----
                                     div(h3(textOutput('weather_warning'))),
                                     div(dataTableOutput("contents"), style = "font-size:80%")
                           )
                           
                           
                         )),
                # гидропосты ----
                tabPanel(
                  title = "Загрузка с логгеров HOBO", icon = icon("water"),
                  fluidPage(
                    shinyjs::useShinyjs(),
                    shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
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
                              div(checkboxInput(inputId = 'ninecol',  value = 0, 
                                                label = 'В файле 9 колонок данных')),
                              div(style = stl, checkboxInput(inputId = 'hobo_header',  value = 0, 
                                                             label = 'Есть первая строка с номером (Plot Title: XXXXXXX)')),
                              # Horizontal line 
                              tags$hr(),
                              
                              # Insert button 
                              actionButton("insert_hobo_df", "Загрузить", icon = icon("arrow-up-from-bracket")),
                              actionButton("reset4", "Очистить"),
                              h2(textOutput("qry_hobo", inline = T))
                    ),
                    wellPanel(id = 'mainTable', 
                              h3(textOutput('hobo_warning')),
                              # Вывод таблицы и результатов добавления c HOBO ----
                              div(style = "overflow-y:scroll; max-height: 600px",
                                  DTOutput("contents_hobo"), 
                                  style = "font-size:80%")
                    )
                    
                    
                  )
                ))
      ),
      # просмотр ----
      tabItem("explore",
              fluidPage(
                shinyjs::useShinyjs(),
                shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                
                # Заголовок
                titlePanel("Просмотр данных"),
                wellPanel(
                  div(style = stl, uiOutput('ui_stations')),
                  div(style = stl, uiOutput('ui_var')),
                  div(style = stl, selectInput('aggregate', 'Осреднение', width = '250px',
                                               choices = c('Без осреднения'='none', '1 час'='1 hour', 
                                                           '6 часов'='6 hours', '12 часов'='12 hours',
                                                           '1 день'='1 day', '1 неделя'='1 week', 
                                                           '1 месяц'='1 month', '1 год'='1 year'))),
                  div(style = "display:block;", 
                      actionButton("plot_graph", "Создать", icon = icon("chart-line")),
                      downloadButton('download',"Скачать таблицу"),
                      actionButton("reset2", "Очистить", icon = icon("broom")))
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
      # обслуживание ----
      tabItem("edit",
              fluidPage(
                shinyjs::useShinyjs(),
                shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                verticalLayout(
                  titlePanel("Редактирование записей"),
                  tags$p('Выберите строку таблицы для удаления записей и нажмите \"Удалить\", после этого нажмите \"Обновить\" и вернитесь на эту страницу.'),
                  # wellPanel(
                  div(DT::dataTableOutput('delete_table'),
                      style = "font-size:80%"),
                  # verbatimTextOutput('delete_table_selection'),
                  div(actionButton("delete_records_source", "Удалить", icon = icon('broom')),
                      actionButton("reset3", "Обновить", icon = icon('arrows-rotate'))),
                  textOutput("deleted_records", inline = T)
                  # )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  source('source//helpers_funs.R', local = T, encoding = 'UTF-8')
  
  # Перезагрузка приложения с кнопки ----
  observeEvent(c(input$reset1,input$reset2,input$reset3, input$reset4), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  # Соединение с БД ----
  con <- db_connect('c:/Users/morey/Documents/R/field_data/pwd.txt')
  
  # Таблица станций ----
  stations_df <- reactive({
    q <- gsub("[\r\n\t]", "", 
              "SELECT fs.name, ft.type, fs.lon, fs.lat, fs.elev, fs.date_open
      FROM field_site fs
      LEFT JOIN field_device_type ft ON fs.type = ft.id
              ORDER BY ft.type")
    pts <- dbGetQuery(con, q)
    return(pts)
  })
  
  # Верхние картинки ----
  output$num_stat <- renderValueBox({
    valueBox(
      value = nrow(stations_df()),
      subtitle = "Количество станций в БД",
      icon = icon("area-chart"),
      # color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
  
  output$dbsize <- renderValueBox({
    valueBox(
      value = getDbStats(con)[[1]],
      subtitle = "Текущий объем БД",
      icon = icon("download")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = getDbStats(con)[[2]],
      "Уникальных записей в БД",
      icon = icon("users")
    )
  })
  
  
  # Таблица с перечнем станций ----
  output$st_table <- renderDT(stations_df(), 
                              colnames = c('Название', 'Тип', 'Долгота, °', 
                                           'Широта, °', 'Высота, м', 
                                           'Дата открытия'),
                              options = list(pageLength = 25,
                                             language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json")))
  
 # Карта расположения станций (пока не работает на сервере) ----
  
  # output$stations_map <- renderLeaflet({
  #   wsh <- rgdal::readOGR('source/srtm_basp.shp')
  #   typepal <- colorFactor(palette = c('red', 'blue'), domain = stations_df()$type)
  #   leaflet(stations_df()) %>%
  #     addTiles() %>%
  #     addCircleMarkers(lng = ~lon, lat = ~lat, fillOpacity = 1,
  #                      label = ~name, labelOptions = labelOptions(noHide = T),
  #                      fillColor = ~typepal(type),
  #                      stroke = F, clusterOptions = 1) %>%
  #     addPolygons(data = wsh) %>%
  #     leaflet::addLegend(colors = c('blue','red'), values = ~type, title = '', opacity = 1,
  #                        labels = c('Метеостанции', 'Гидропосты'))
  #     
  # })

  # Карта расположения станций с nextgis.com ----
  output$stations_iframe_map <- renderUI({
    tags$iframe(src ="https://wshydro.nextgis.com/resource/108/display/tiny?angle=0&zoom=10&styles=142%2C140%2C413%2C674&linkMainMap=true&events=false&panel=none&controls=id&panels=layers%2Cidentify&base=basemap_0&lon=34.0000&lat=45.0087",
                          style="overflow:hidden;height:500px;width:700px", height="500", width="700")
  })
  
  # Панель загрузки данных ----
  
  # Метеоданные ----
  
  # Получение из БД списка метеостанций для добавления в таблицу ----
  output$ui_st_import <- get_weather_station_list_selectInput(1, F, 'station_id')
  
  # Таблица данных с метеостанции ----
  input_df <- reactive({
    req(input$file1)
    output$weather_warning <- validate(need(tools::file_ext(input$file1$datapath) == c("csv", "txt", "asc"), 
                                            "Пожалуйста, загрузите текстовый файл (txt, csv, asc)"),
                                       need(input$station_id != '', 'Выберите название станции!')
    )
    
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
  
  # Гидроданные ----
  # Получение из БД списка логгеров для добавления в таблицу ----
  output$ui_hobo_import <- get_weather_station_list_selectInput(2, F, 'station_hobo_id')
  
  # Таблица данных с логгера ----
  input_df_hobo <- reactive({
    req(input$file2)
    
    output$hobo_warning <- validate(need(tools::file_ext(input$file2$datapath) == c("csv", "txt", "asc"), 
                                         "Пожалуйста, загрузите текстовый файл (txt, csv, asc)"),
                                    need(input$station_hobo_id != '', 'Выберите название станции!')
    )
    
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
    req(input$ui_stations, input$pick_var, input$aggregate)
    # print(input$pick_var)
    # print(input$ui_stations)
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
    
    # аггрегация данных ----
    print(input$aggregate)
    if(input$aggregate != 'none'){
      df <- df %>%
        group_by(name, var_name, datetime = lubridate::floor_date(datetime, input$aggregate)) %>%
        summarise(value = case_when(
          any(var_name == 'Rain') ~ sum(value),
          any(var_name != 'Rain') ~ mean(value),
          .default = mean(value))) %>%
        pivot_wider(id_cols = datetime,  
                    names_from = c('name', 'var_name'), names_sep = '_', 
                    values_from = 'value') %>%
        mutate(across(where(is.numeric), round, 3))
    }else{
      df <- df %>%
        pivot_wider(id_cols = datetime,  
                    names_from = c('name', 'var_name'), names_sep = '_', 
                    values_from = 'value') %>%
        mutate(across(where(is.numeric), round, 3))
    }
    print(head(df))
    return(df)
    # print(head(df))
  })
  
  
  
  # График ----
  observeEvent(input$plot_graph, {
    output$plotdata <- renderUI({
      withProgress(expr = {
        df <- plot_df()
        # tryCatch({
        ts <- as.xts(df[,-1], 
                     order.by = as.POSIXct(df$datetime))
        # }, warning = function(war){
        #   print(paste("Предупреждение:  ", war))
        # }, error = function(e){
        #   print(paste("Ошибка:  ", e))
        #   stop(safeError(e))
        # })
        lst <- lapply(ts, function (x) dygraph(x, main = colnames(x), 
                                               group = 'plots', 
                                               width = 'auto', 
                                               height = 200) %>% 
                        dyRangeSelector() %>%
                        dyAxis("x", label = "Дата", 
                               valueFormatter = jsValueFormatter(),
                               rangePad=5) |>
                        dyAxis("y", drawGrid = FALSE) %>%
                        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")))
        # print(lst)
        res <- htmltools::tagList(lst)
        return(res)
      }, message = "Загрузка...")
    })
    # Вывод ----
    output$datatable <- renderDataTable(
      plot_df(),
      options = list(pageLength = 100, 
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  # Файл для скачивания ----
  output$download <- downloadHandler(
    filename = function(){"krymdata_output.csv"}, 
    content = function(fname){
      write.table(plot_df(), 
                fname, sep = ";", quote = F, row.names = F, na = '-32968')
    }
  )
  
  # Таблица с перечнем станций и источников для удаления ----
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
  
  output$delete_table <- renderDT(stations_source(), server = FALSE, 
                                  selection = 'single',
                                  colnames = c('Дата добавления', 'Источник', 'Станция', 'Добавлено записей'),
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

shinyApp(ui, server)