# Install required packages if needed
# install.packages(c("shiny", "dplyr", "ggplot2"))

library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(ggplot2)
library(plotly)
library(climaemet)
library(openair)
library(DT)

md <- c("Data Type", "Record name","Start time","Duration (H:M:S)",
        "Location description","Location address","Location coordinates","Notes")

## functions
read_raw <- function(file){
  
  # read the file to get the name of variables 
  vnames <- readr::read_csv(file, skip = 3, n_max=1)
  
  custom_locale <- locale(decimal_mark = ",")
  raw <- readr::read_csv(file, skip = 5, 
                         col_names = names(vnames), 
                         locale = custom_locale)
  return(raw)
  
}

format_climate <- function(raw, md){ 
  
  data <- raw |> 
    dplyr::select(!all_of(md)) |> 
    clean_names() |> 
    mutate(datetime = 
             lubridate::ymd_hms(
               gsub("p\\. m\\.", "pm", formatted_date_time, ignore.case = TRUE))
    ) |> 
    relocate(datetime) |> 
    dplyr::select(-formatted_date_time)
  
  return(data)
  
}

get_duration <- function(raw){
  
  out <- raw |> 
    dplyr::select(`Record name`,`Start time`,`Duration (H:M:S)`) |> 
    na.omit()
  
  # Remove the period after the month abbreviation
  datetime_str <- gsub("\\.", "", out$`Start time`)
  
  # Convert to datetime object
  datetime <- dmy_hms(datetime_str, tz = "UTC")
  
  d <- list(datetime = datetime, duration = out$`Duration (H:M:S)`) 
  return(d)
}

# Define the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    title =
      div(
        "Weather conditions during UAV flying timer",
        p(),
        img(src = "logoserpam.jpg", height = "150", " SERPAM-EEZ"),
        tags$a(
          href = "https://lifewatcheric-sumhal.csic.es/",
          target = "_blank",
          tags$img(src = "logosumhal.jpg", height = "70", "Proyecto SUMHAL")
        )
      ),
    windowTitle = "Weather viewer"
  ),
  
  
  
  
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("file", "Upload CSV file"),
      fluidRow(
        column(12, textOutput("result_output"))
      )
    ),
    mainPanel(width = 9,
      tabsetPanel(type = "tabs",
                tabPanel("Temperature", plotlyOutput("plot_temperature")), 
                tabPanel("Relative Humidty", plotlyOutput("plot_humidity")), 
                # tabPanel("WindA", plotOutput("plot_windA")), 
                tabPanel("Wind", plotOutput("plot_wind")),
                tabPanel("Table", dataTableOutput("table"))
        
      )
    )
  )
)


# Define the server
server <- function(input, output, session) {
  
  raw <- reactive({
    req(input$file)
    read_raw(input$file$datapath)
  })
  
  data <- reactive({
    format_climate(raw(), md)
  })
  
  
  output$plot_temperature <- renderPlotly({
  
    ggplotly(
      data() |> dplyr::select(datetime, temperature, heat_index) |> 
        pivot_longer(-datetime, names_to = "variables") |> 
        ggplot(aes(x=datetime, y=value, colour = variables)) + 
        geom_point() + geom_path() +
        theme_bw() +
        theme(legend.position = "bottom") + 
        xlab("Time") + 
        ylab("ºC") 
      
    ) |> 
      layout(legend = list(orientation = "h"))

  })
    
  
  output$plot_humidity <- renderPlotly({
    
    ggplotly(
      ggplot(data(), aes(x=datetime, y=relative_humidity)) + 
        geom_point() + geom_path() +
        theme_bw() +
        # scale_x_datetime(date_breaks = "5 min", date_labels = "%H:%M") +
        xlab("Time") + 
        ylab("Relative humidity (%)")
      
    )
    
  })
  
  output$plot_wind <- renderPlot({
    windRose(data(), 
             ws = "wind_speed", 
             wd = "compass_magnetic_direction")
    
  })
  
  # output$plot_windA <- renderPlot({
  #   
  #     ggwindrose(speed = data()$wind_speed, 
  #                direction = data()$compass_magnetic_direction, 
  #                speed_cuts = seq(0,30,3))
  #   
  # })
  # 
  
  output$table <- renderDT(server = FALSE, {
    DT::datatable(
      data(),
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Current Page", filename = "page",
               exportOptions = list(
                 modifier = list(page = "current")
               )
          ),
          list(extend = "csv", text = "Download Full Results", filename = "data",
               exportOptions = list(
                 modifier = list(page = "all")
               )))))
  })
  
  output$result_output <- renderText({
    o <- get_duration(raw()) 
    
    HTML(paste('Date:', o$datetime, 
               '<br> Duration:', o$duration))
  })
} 

  

# Run the app
shinyApp(ui, server)
