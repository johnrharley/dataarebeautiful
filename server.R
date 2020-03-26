# server details for shiny code

server <- function(input, output) {
  
  output$testingPlot <- renderPlotly({
    testingInteractice <- counts %>%
      gather(Date, Tests, -Lab, factor_key = TRUE) %>%
      mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
      filter(Lab %in% c("ASPHL cumulative", "Commercial cumulative")) %>%
      ggplot(aes(x=Date, y=Tests, color=Lab)) +
      geom_point(size=2) +
      geom_line(alpha=0.3) +
      theme_classic(base_size=12) +
      labs(x="Date", y="Cumulative Tests", title="COVID-19 Testing in Alaska") +
      theme(legend.position = "top")
    ggplotly(testingInteractice) %>%
      layout(legend = list(
        orientation = "h"
      ))
  })
 
  
  output$covid_map <- renderLeaflet({
    leaflet(city_data) %>%
      addTiles() %>%
      addCircleMarkers(lat=~lat, lng=~lon, radius = ~Total, 
                       popup = paste("Region:", cities$Region, "<br>",
                                     "Travel-Related:", cities$`Travel-Related`, "<br>",
                                     "Non-Travel:", cities$`Non-Travel`, "<br>",
                                     "Total:", cities$Total, "<br>")) %>%
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
      addControl(title, position = "topright", className="map-title") %>%
      addControl(subtitle, position = "topright", className="map-subtitle")
  })


  output$casesPlot <- renderPlotly({
      casesPlot <- ggplot(cases, aes(x=Date, y=positiveCases)) +
      geom_point(color="black", alpha=0.6, size=2) +
      theme_bw() +
      geom_line(color="grey60", alpha=0.6) +
      geom_smooth(method="lm", formula = y ~ poly(x,2), se=F, size=2, color="#2a82a8", alpha=0.7) +
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
        labs(x="", y="Confirmed positive cases", title="COVID-19 Cases in Alaska")
    ggplotly(casesPlot)
  })
  
  
  output$casesPlotLog <- renderPlotly({
    casesLog <- cases %>%
      filter(positiveCases > 0)
    casesPlotLog <- ggplot(casesLog, aes(x=Date, y=positiveCases)) +
      geom_point(color="black", alpha=0.6, size=2) +
      theme_bw() +
      scale_y_log10() +
      geom_line(color="grey60", alpha=0.6) +
      geom_smooth(method="lm", formula = y~x, se=F, size=2, color="#2a82a8", alpha=0.7) +
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
      labs(x="", y="Confirmed positive cases", title="COVID-19 Cases in Alaska")
    ggplotly(casesPlotLog)
  })
  
  link <- a("Covid Monitoring", href="http://www.dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/monitoring.aspx")  
  
  output$tab <- renderUI({
    tagList("DHHS:", link)
    
  })
  
  gitLink <- a("johnrharley", href = "https://github.com/johnrharley/dataarebeautiful")
  
  output$git <- renderUI({
    tagList("Github: ", gitLink)
  })  
}


