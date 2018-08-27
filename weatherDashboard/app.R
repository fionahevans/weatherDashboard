library(shiny)
library(shinyjs)
library(ppd)
library(httr)
library(jsonlite)
library(shinydashboard)
library(shinycssloaders)
library(highcharter)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Daily Operations"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Weather", tabName = "weather", icon = icon("tint")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # tabItem(tabName = "dashboard",
      #         h2("Dashboard")
      # ),
      tabItem(tabName = "weather",
              tabsetPanel(
                id = "weatherTabs",
                tabPanel(
                  title = "Weather snapshot",
                  value = "weatherSnap",
                  # Boxes need to be put in a row (or column)
                  fluidRow(
                    column(
                      width = 12,
                      h3(textOutput("station_name") , align="center")
                    )
                  ),
                  fluidRow(
                    #div(id = "loading", height=400, p("Loading data...")),
                    column(
                      width = 8,
                      highchartOutput("rtd_hc") #%>% withSpinner(color="#0dc5c1")
                    ),
                    column(
                      width = 4,
                      highchartOutput("py_hc") 
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      highchartOutput("sw_hc") #%>% withSpinner(color="#0dc5c1")
                    )
                    
                  )
                ), # end tabPanel
                
                tabPanel(
                  title = "Weather inputs",
                  value = "weatherParms",
                  fluidRow(
                    column(
                      width=12,
                      h3("Data"),
                      p("This weather snapshot is produced using data provided by the WA Department of Primary 
  Industries and Regional Development (DPIRD) via API."),
                      h3("Summer and seasonal rainfall")
                    )
                  ),
                  fluidRow(
                    column(
                      width=4, 
                      selectInput(
                        "station", "Weather station", 
                        c("Merredin (BOM: 10092)"), selected="Merredin (BOM: 10092)"),
                      dateInput(
                        "summerStart", label = "Summer start date", 
                        value = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")) - 1, "1101"), "%Y%m%d"), 
                        min = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")) - 1, "1101"), "%Y%m%d")
                      ),
                      dateRangeInput(
                        "seasonDates", label = "Growing season start and end dates", 
                        start = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")), "0401"), "%Y%m%d"), 
                        end = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")), "1031"), "%Y%m%d"), 
                        min = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")), "0301"), "%Y%m%d"),
                        max = as.Date(paste0(as.numeric(format(Sys.time(), "%Y")), "1031"), "%Y%m%d")
                      )
                    ),
                    column(
                      width=8,
                      includeHTML("rtd.html")
                    )
                  ),
                  fluidRow(
                    column(
                      width=12,
                      h3("Potential Yield (French & Schultz)")
                    )
                  ),
                  fluidRow(
                    column(
                      width=4,
                      sliderInput("wue", label="Water Use Efficiency (WUE)", min=8, max=22, value=15),
                      sliderInput("evap", label="Evaporation", min=80, max=120, value=110)
                    ),
                    column(
                      width=8,
                      includeHTML("py.html")
                    )
                  ),
                  fluidRow(
                    column(
                      width=12,
                      h3("Soil water")
                    )
                  ),
                  fluidRow(
                    column(
                        width=4,
                        selectInput("soil", "Soil type", c("gravel", "shallow-soil", 
                                                           "sand", "sandy-earth", 
                                                           "shallow-sandy-duplex", 
                                                           "deep-sand-duplex", 
                                                           "shallow-loamy-duplex", 
                                                           "deep-loamy-duplex", 
                                                           "loamy-earth", "clay"),
                                    selected="shallow-sandy-duplex")
                      ),
                    column(
                        width=8,
                        includeHTML("sw.html")
                    )
                  ) # end fluidRow
                )
              ) # end tabsetPanel
              
      ) # end tabItem
    ) # end tabItems
  ) # end dashboardBody
)

server <- function(input, output, session) { 
  
  science.apiKey <- 'iDUtU8TgAZIiCwKYkbXv9xr2Y4W8OxPC'
  science <- 'https://api.dpird.wa.gov.au/v2/science/rainfall/'
  
  # Get weather stations from API
  stations <- getSCIENCEstations(science.apiKey)
  stations$name <- paste0(stations$stationName, " (", stations$owner, ": ", stations$stationCode, ")")
  
  # Reactive data
  station.name <- reactiveValues()
  rtd <- reactiveValues()
  rtd.summary <- reactiveValues()
  rtd.show <- reactiveValues()
  py <- reactiveValues()
  sw <- reactiveValues()
  breakOfSeason <- reactiveValues()
  
  # Fill selectInput with stations
  observe({
    updateSelectInput(session, "station", choices = stations$name, selected="Merredin (BOM: 10092)")
  })
  
  # Get RTD data when inputs are updated
  observeEvent(c(input$station, input$summerStart, input$soil), {
   
    forecastDate <- format(today, "%Y-%m-%d")
    
    station.name <- stations[which(stations$name == input$station), "stationName"]
    station.id <- stations[which(stations$name == input$station), "stationCode"]
    
    ret <- ppd::getRTD(station.id, forecastDate, input$summerStart, input$seasonDates[1],
                       input$seasonDates[2], science.apiKey)
    
    rtd$data <- ret$data
    rtd.summary$data <- as.data.frame(ret$summary)
    calculatePY()
    cat("Rainfall to date data updated.\n")
  })
  
  # Get soil water data when inputs are updated
  observeEvent(c(input$station, input$summerStart, input$soil), {
    forecastDate <- format(today, "%Y-%m-%d")
    
    station.name <- stations[which(stations$name == input$station), "stationName"]
    station.id <- stations[which(stations$name == input$station), "stationCode"]
    
    output$station_name <- renderText({ 
      station.name
    })
    
    ret <- ppd::getSoilWater(station.id, startDate=input$summerStart, 
                             endDate=forecastDate, apiKey=science.apiKey)
    
    # Reshape data frame to cover growing season only
    #data <- subset(ret$data, date >= input$seasonDates[1])
    data <- ret$data
    days <- seq(from=data$date[nrow(data)]+1, to=input$seasonDates[2], by="1 day")
    vec <- rep(NA, length(days))
    tmp <- data.frame(date=days, rainfall=vec, fallow=vec, crop=vec)
    data <- rbind(data, tmp)
    
    #print(days)
    
    sw$data <- data
    breakOfSeason$data <- ret$breakOfSeason
    
    print(dim(rtd))
    print(dim(data))
    cat("Soil water data updated.\n")
  })
  
  calculatePY <- function(){
    
    Wavail = list(
      decile1 = rtd.summary$data$summer.cumulativeRainfall[1] * 0.3 +
                rtd.summary$data$season.cumulativeRainfall[1]  +
                rtd$data$proj1[nrow(rtd$data)],
      decile5 = rtd.summary$data$summer.cumulativeRainfall[1] * 0.3 +
                rtd.summary$data$season.cumulativeRainfall[1]  +
                rtd$data$proj5[nrow(rtd$data)],
      decile9 = rtd.summary$data$summer.cumulativeRainfall[1] * 0.3 +
                rtd.summary$data$season.cumulativeRainfall[1]  +
                rtd$data$proj9[nrow(rtd$data)])
                          
    py$data <- data.frame(
                   name=c("Decile 1", "Decile 5", "Decile 9"),
                   py=c(round((Wavail$decile1-input$evap) * input$wue / 1000, 1), 
                        round((Wavail$decile5-input$evap) * input$wue / 1000, 1), 
                        round((Wavail$decile9-input$evap) * input$wue / 1000, 1)))
    cat("Potential yield data updated.\n")
  }

 # Calculate PY when inputs are updated observeEvent
 observeEvent(c(input$wue, input$evap), {
   calculatePY()
 })
  
 
    
  # Render rainfall to date chart
  output$rtd_hc <- renderHighchart({
    
    data <- rtd$data
      
      highchart() %>%
        #hc_title(text = paste("Rainfall to date:", station.name)) %>%
        hc_tooltip(
          shared = TRUE) %>%
        
        hc_add_series(
          round(data$decile1, 1),
          color = "#c3b091",
          name = paste("Decile 1")) %>%
        hc_add_series(
          round(data$decile5, 1), 
          color = "#c3b091",
          name = paste("Decile 5")) %>%
        hc_add_series(
          round(data$decile9,  1),
          color = "#c3b091",
          name = paste("Decile 9")) %>%
        hc_add_series(
          round(max(data$cumulativeRainfall, na.rm=T)+data$proj1, 1),
          color = "#0dc5c1",
          name = paste("Projected decile 1")) %>%
        hc_add_series(
          round(max(data$cumulativeRainfall, na.rm=T)+data$proj5, 1),
          color = "#0dc5c1",
          name = paste("Projected decile 5")) %>%
        hc_add_series(
          round(max(data$cumulativeRainfall, na.rm=T)+data$proj9, 1),
          color = "#0dc5c1",
          name = paste("Projected decile 9")) %>%
        hc_add_series(
          data$cumulativeRainfall, 
          color = "purple",
          name = paste("Cumulative rainfall")) %>%
        # hc_xAxis(
        #   type = "datetime",
        #   categories = data$date,
        #   dateTimeLabelFormats = list(day = "%d %b"),
        #   tickInterval  = 14 * 24 * 3600 * 1000
        # ) %>%
        hc_yAxis(
          title = list(text = "Cumulative rainfall (mm)")
        )
  })

  
 
  # Render potential yield chart
  output$py_hc <- renderHighchart({
    
    data <- py$data
      
      highchart() %>%
        #hc_title(text = paste("Potential Yield:")) %>%
        hc_tooltip(
          shared = TRUE) %>%
        hc_add_series(
          data, hcaes(x=name, y=py),
          type = "bar", 
          color = "#0dc5c1",
          name = ""
        ) %>%
        hc_yAxis(
          title = list(text = "Potential yield (t/ha)")
        ) %>%
        hc_xAxis(
          categories = data$name
        ) %>%
        hc_legend(list(enabled = FALSE))
    })
  
  # Render soil water chart
  output$sw_hc <- renderHighchart({
    
    data <- sw$data
    
    highchart() %>%
      #hc_title(text = paste("Rainfall to date:", station.name)) %>%
      hc_tooltip(
        shared = TRUE) %>%
      hc_add_series(
        data$fallow, 
        type = "spline",
        color = "#706a48",
        name = paste("Soil water (fallow)")) %>%
      hc_add_series(
        data$crop, 
        type = "spline",
        color = "#a6c143",
        name = paste("Soil water (crop)")) %>%
      hc_add_series(
        data$rainfall, 
        type = "column",
        color = "#265e96",
        name = paste("Rainfall")) %>%
      # hc_xAxis(
      #   type = "datetime",
      #   categories = data$date,
      #   dateTimeLabelFormats = list(day = "%d %b"),
      #   tickInterval  = 14 * 24 * 3600 * 1000,
      #   plotLines = list(color = "gray", dashstyle = "shortdash", width = 2,
      #                    label = list(text = "Break of season"))
      # ) %>%
      hc_yAxis(
        title = list(text = "Soil water (mm)")
      )
  })  
  
}

shinyApp(ui, server)