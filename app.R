library(shiny)
library(shinyjs)
library(dplyr)
library(lubridate)
library(RPostgreSQL)
library(tidyverse)

mypaw <- {
  "vnFkY9Vj"
}
drv <- dbDriver("PostgreSQL")
tryCatch({
  con <- dbConnect(drv, dbname = "hydromet",
                   host = "192.168.5.203", port = 5432,
                   user = "moreydo", password = mypaw)
},
error = function(e) {
  # return a safeError if a parsing error occurs
  output$qry <- renderText("Error connecting to database!")
  stop(safeError(e))
})
rm(mypaw)



# Define UI for data upload app ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
  # App title ----
  titlePanel("Загрузка файлов с метеостанций"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
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
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      
      # Insert button ----
      actionButton("insert_df", "Загрузить"),
      actionButton("reset", "Очистить")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      dataTableOutput("contents"),
      h2(textOutput("qry"))
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  observeEvent(input$reset, {
    shinyjs::js$refresh_page()
  })  
  
  input_df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
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
                 HiDir = as.integer(factor(HiDir, ordered = T))) # %>%
          # select(!c(Date, Time))
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
    
        df <- pivot_longer(input_df(), cols = !c(datetime, station_name),
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
          print(n)
          for (i in 1:n) {
          q <- paste0("INSERT INTO field_data (station, datetime, variable, value, type, change, source) VALUES ('", df$station_name[i],  "','", df$datetime[i],"','", trimws(df$variable[i]), "',", df$value[i],",", type,",'", created_on, "', '", data_source, "') ON CONFLICT DO NOTHING")
          print(q)
          
          progress$inc(1/n, detail = paste("Обрабатывается запись", i, " из ", n))
          tryCatch({
            dbExecute(con, q)
          },
            error = function(e) print(e)
          )
          
          }
          return(paste("В таблицу добавлено ", n, " записей данных с метеостанции ", input$station_name))
      })
  
    })
  lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(con = x)})
}
# Create Shiny app ----
shinyApp(ui, server)