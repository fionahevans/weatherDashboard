
# Get weather data and render RTD using API when inputs change
# observeEvent(c(input$station, input$summerStart, input$seasonDates), {
#   hide(id = "rtd_hc")
#   hide(id = "py_hc")
#   
#   renderRTD2()
#   renderPotentialYield()
#   
#   show(id = "rtd_hc")
#   show(id = "py_hc")
# })


renderRTD <- function() {
  
  
  # Render rainfall to date chart
  output$rtd_hc <- renderHighchart({
    
    highchart() %>%
      hc_title(text = paste("Rainfall to date:", station.name)) %>%
      hc_tooltip(
        shared = TRUE) %>%
      
      hc_add_series(
        rtd$data, "line", hcaes(date, decile1), 
        color = "#c3b091",
        name = paste("Decile 1")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(date, decile5), 
        color = "#c3b091",
        name = paste("Decile 5")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(date, decile9), 
        color = "#c3b091",
        name = paste("Decile 9")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(date, max(cumulativeRainfall, na.rm=T)+proj1), 
        color = "#0dc5c1",
        name = paste("Projected decile 1")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(date, max(cumulativeRainfall, na.rm=T)+proj5), 
        color = "#0dc5c1",
        name = paste("Projected decile 5")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(date, max(cumulativeRainfall, na.rm=T)+proj9), 
        color = "#0dc5c1",
        name = paste("Projected decile 9")) %>%
      hc_add_series(
        rtd$data, "line", hcaes(x=date, y=cumulativeRainfall), 
        color = "purple",
        name = paste("Cumulative rainfall")) %>%
      hc_xAxis(
        type = "datetime",
        dateTimeLabelFormats = list(day = "%d %b"),
        tickInterval  = 14 * 24 * 3600 * 1000
      ) %>%
      hc_yAxis(
        title = list(text = "Cumulative rainfall (mm)")
      )
  })
  
}
