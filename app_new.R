library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dygraphs)
library(htmltools)
library(RPostgreSQL)
library(DT)
library(xts)
library(terra)
dotenv::load_dot_env()


stl <- "display:inline-block; vertical-align:top"

ui <- dashboardPage(skin = 'red', 
                    dashboardHeader(title = "IvankovoNIS"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Обзор", tabName = "dashboard", icon = icon("map-location-dot")), # fontawesome.com
                        # menuItem("Загрузка", tabName = "upload", icon = icon("arrow-up-from-bracket")),
                        menuItem("Просмотр", tabName = "explore", icon = icon("chart-line")),
                        # menuItem("Обслуживание", tabName = "edit", icon = icon("cog"))
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
                                      leafletOutput('stations_map')),
                                  box(title = "Перечень станций", solidHeader = TRUE,
                                      dataTableOutput("st_table"))
                                ),
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
                                                                 choices = c('Без осреднения'='none', '1 месяц'='1 month', 
                                                                             '2 месяца'='2 months', '3 месяца'='3 months',
                                                                             '6 месяцев'='6 months', '1 год'='1 year'))),
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
                      )
                    )
)

server <- function(input, output, session) {
  # source('source/helpers_funs.R', local = T, encoding = 'UTF-8')
  
  # Перезагрузка приложения с кнопки ----
  observeEvent(c(input$reset1,input$reset2,input$reset3, input$reset4), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  # Соединение с БД ----
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS")
  )
  
  # Таблица станций ----
  stations_df <- reactive({
    q <- gsub("[\r\n\t]", "", 
              "SELECT name_station, X, Y FROM stations")
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
      subtitle = "Текущий объём БД",
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
                              colnames = c('Название', 'Долгота, °', 
                                           'Широта, °'),
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
  #                        labels = c('Условия отбора проб', 'Физико-химические характеристики'))
  #   
  # })
  
  
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
    # q <- gsub("[\r\n\t]", "", 
    #           paste0("SELECT field_data.datetime, field_site.name, 
    #             field_var_unit.var_name, field_data.value 
    #                    FROM field_data 
    #                    LEFT JOIN field_site
    #                    ON field_data.station::integer=field_site.id
    #                    LEFT JOIN field_var_unit 
    #                    ON field_data.variable::integer = field_var_unit.id
    #                    WHERE field_data.variable IN (\'", 
    #                  paste0(input$pick_var, collapse = '\', \''), "\') 
    #             AND field_data.station IN ('",
    #                  paste0(input$ui_stations, collapse = '\', \''),"')  ORDER BY datetime")
    q <- gsub("[\r\n\t]", "",
                 paste0("SELECT stations.name_station, values_types.value_type, samples.date, values.value
                        FROM values
                        JOIN values_types ON values.ID_value = values_types.ID_value
                        JOIN samples ON values.ID_condition = samples.ID_condition
                        JOIN stations ON samples.ID_station = stations.ID_station
                        JOIN sample_conditions ON values.ID_condition = sample_conditions.ID_condition
                       WHERE values_types.value_type IN (\'",
                     paste0(input$pick_var, collapse = '\', \''), "\')
                AND stations.name_station IN ('",
                     paste0(input$ui_stations, collapse = '\', \''),"')  ORDER BY date")

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
          .default = mean(value))) %>%
        pivot_wider(id_cols = datetime,  
                    names_from = c('name_station', 'value_type'), names_sep = '_', 
                    values_from = 'value') %>%
        mutate(across(where(is.numeric), round, 3))
    } else {
      df <- df %>%
        pivot_wider(id_cols = datetime,  
                    names_from = c('name_station', 'value_type'), names_sep = '_', 
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
                  fname, sep = ";", quote = F, row.names = F, na = '-')
    }
  )
  
  # Разрыв соединения с БД ----
  session$onSessionEnded(function() {
    #   lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
    dbDisconnect(con)
    print('Disconnected')
  })
}

shinyApp(ui, server)