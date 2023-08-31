library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RPostgreSQL)
library(tidyverse)


# Define UI for data upload app ----
ui <- navbarPage(title = "КрымДанные", footer = div(class = "footer", includeHTML("footer.html")), 
                 fluid = T, windowTitle = "КрымДанные", lang = "ru",
                 
                 # Метеостанции ----
                 tabPanel(title = "Загрузка с метеостанции",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),

                            # App title ----
                            titlePanel("Загрузка файлов с метеостанций"),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Добавление файла ----
                              sidebarPanel(
                                
                                selectInput("station_name", "Название метеостанции", 
                                            choices = c('Ольмесхыр', 'Многоречье', 'Кизилкобинка', 'Караби')), # 'Чатырдаг', 
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                
                                # Input: Select a file ----
                                fileInput("file1", "Выбрать файл",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"),
                                          buttonLabel = "Выбрать...",
                                          placeholder = "Файл не выбран"),
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Insert button ----
                                actionButton("insert_df", "Загрузить"),
                                actionButton("reset1", "Очистить")
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                dataTableOutput("contents"),
                                h2(textOutput("qry"))
                              )
                              
                            )
                          )
                 ),
                 tabPanel(title = "Просмотр",
                          fluidPage(
                            shinyjs::useShinyjs(),
                            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
                            
                            # App title ----
                            titlePanel("Просмотр данных"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput('ui_stations', ),
                                uiOutput('ui_var'),
                                actionButton("plot_graph", "Создать"),
                                actionButton("reset2", "Очистить")
                              ),
                              mainPanel(
                                div(style='overflow-y: scroll', 
                                    plotOutput('plotdata')),
                                dataTableOutput("datatable")
                              )
                            )
                          ))
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  source('source/helpers_funs.R', local = T)
  
  mypaw <- {
    "vnFkY9Vj"
  }
  drv <- dbDriver("PostgreSQL")
  tryCatch({
    con <- dbConnect(drv, dbname = "hydromet",
                     host = "192.168.5.203", port = 5432,
                     user = "moreydo", password = mypaw)
    print('Connected')
  },
  error = function(e) {
    # return a safeError if a parsing error occurs
    output$qry <- renderText("Error connecting to database!")
    stop(safeError(e))
  })
  rm(mypaw)
  
  observeEvent(c(input$reset1,input$reset2), {
    shinyjs::js$refresh_page()
  }, ignoreNULL = T, ignoreInit = T)  
  
  input_df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    validate(need(tools::file_ext(input$file1$datapath) == c("csv", "txt", "asc"), 
                  "Пожалуйста, загрузите текстовый файл (txt, csv, asc)"))
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath, sep = '\t', header = F,
                       check.names = F, stringsAsFactors = F,
                       col.names = c("Date",	"Time",	"TempOut",	"HiTemp",	"LowTemp",	"OutHum",	"DewPt",	"WindSpeed",	"WindDir",	"WindRun",	"HiSpeed",	"HiDir",	"WindChill",	"HeatIndex",	"THWIndex",	"Bar",	"Rain",	"RainRate",	"HeatD-D",	"CoolD-D",	"InTemp",	"InHum",	"InDew",	"InHeat",	"InEMC",	"InAirDensity",	"WindSamp",	"WindTx ",	"ISSRecept",	"ArcInt"),
                       skip = 2, na.strings = '---',
                       colClasses = c('character', 'character', 
                                      rep('numeric', 6), 'character', 'numeric', 'numeric', 'character', 
                                      rep('numeric', 18)))
        df <- df %>%
          mutate(datetime = parse_date_time(paste(Date, Time), orders = "%d.%m.%y %H:%M", tz = 'GMT'),
                 station_name = input$station_name) %>%
          filter(!is.na(datetime)) %>%
          relocate(station_name, datetime) %>%
          mutate(pres_mm = Bar * 0.75006150504341, 
                 WindDir = as.integer(factor(WindDir, ordered = T)), 
                 HiDir = as.integer(factor(HiDir, ordered = T)))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$contents <- renderDataTable(
    input_df(), 
    options = list(pageLength = 10, 
                   language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
  )
  
  observeEvent(input$insert_df, {
    
    output$qry <- renderText({
      # количество исходных строк
      n_init <- nrow(input_df())
      # количество исходных переменных как количество столбцов минус название, датавремя, дата, время
      nvar <- ncol(input_df()) - 4
      df <- input_df() %>%
        dplyr::select(!c(Date, Time)) %>%
        pivot_longer(cols = !c(datetime, station_name),
                     names_to = 'variable', values_to = 'value')
      created_on <- now()
      data_source <- 'shiny_app'
      type <- '1' # 1 - метеостанция, 2 - логгер уровня и температуры, 3 - логгер электропроводности и температуры
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Загрузка", value = 0)
      n <- nrow(df)
      # print(n)
      for (i in 1:n) {
        q <- paste0("INSERT INTO field_data (station, datetime, variable, value, type, change, source) VALUES ('", df$station_name[i],  "','", df$datetime[i],"','", trimws(df$variable[i]), "',", df$value[i],",", type,",'", created_on, "', '", data_source, "') ON CONFLICT DO NOTHING")
        # print(q)
        
        progress$inc(1/n, detail = paste("Обрабатывается запись", i, " из ", n))
        tryCatch({
          # dbExecute(con, q)
          qry <- dbSendStatement(con, q)
        },
        error = function(e) return(e)
        )
        
      }
      # return(paste("В таблицу добавлено ", n, " записей данных с метеостанции ", input$station_name, 
      #              '(Количество исходных записей (', n_init, ') умноженное на число переменных (', nvar, ')'))
      print(qry)
      res <- dbGetRowsAffected(qry)
      print(res)
      return(paste("В таблицу добавлено ", res, " записей данных"))
    })
  })
  
  output$ui_stations <- renderUI({
    st_list <- dbGetQuery(con, "SELECT DISTINCT station FROM field_data")
    selectInput('pick_station',
                label ='Метеостанции',
                choices=st_list$station,
                selected = NULL, multiple = TRUE)
  })
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
  
  plot_df <- reactive({
    req(input$pick_station, input$pick_var)
    q <- paste0("SELECT datetime, station, variable, change, value FROM field_data WHERE variable IN (\'", 
                paste0(input$pick_var, collapse = '\', \''), "\') AND station IN ('",
                paste0(input$pick_station, collapse = '\', \''),"')  ORDER BY datetime")
    print(q)
    # q <- enc2utf8(q)
    tryCatch({
      df <- dbGetQuery(con, q)
    },
    error = function(e) return(e)
    )
    
  })
  
  observeEvent(input$plot_graph, {
    output$plotdata <- renderPlot({
      withProgress(expr = {
        ggplot(plot_df(), aes(x = datetime, y = value, col=variable)) + geom_line() +
          facet_wrap(variable~station, ncol = 1, scales = 'free_y', strip.position = 'right') +
          scale_x_datetime(date_labels = "%d.%m.%y") +
          # theme_light(base_size = 16) + 
          theme(legend.position = 'top') + 
          labs(x='Дата', y='', col='')}, message = "Загрузка...")
    })
    output$datatable <- renderDataTable(
      plot_df() %>%
        pivot_wider(id_cols = c('datetime', 'change'), names_from = c('station', 'variable'), values_from = 'value'),
      options = list(pageLength = 100, 
                     language = list(url = "https://cdn.datatables.net/plug-ins/1.10.19/i18n/Russian.json"))
    )
  })
  
  session$onSessionEnded(function() {
    lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
    print('Disconnected')
  })
}
# Create Shiny app ----
shinyApp(ui, server)