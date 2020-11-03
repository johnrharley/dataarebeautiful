library(crosstalk)
library(shiny)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(shinythemes)
library(leaflet)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("Diurnal temperature flux at select U.S. cities"),
    mainPanel(
        tabsetPanel(
            tabPanel("Temperature Plot", plotlyOutput('distPlot')),
            tabPanel("Map of Stations", leafletOutput('map')))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # read in data files
    df1 <- read.csv("/data/2290507.csv")
    df2 <- read.csv("2292048.csv", stringsAsFactors = FALSE) 
    
    # convert dates
    df1 %>%
        mutate(Date = as.Date(DATE)) %>%
        mutate(Year = lubridate::year(Date), 
               Month = lubridate::month(Date),
               yday = lubridate::yday(Date)) %>%
        select(-DATE) -> df1
    
    # clean up datasets
    df2 %>%
        mutate(Date = as.Date(as.character(DATE), format="%Y%m%d")) %>%
        rename(NAME = STATION_NAME) %>%
        mutate(Year = lubridate::year(Date), 
               Month = lubridate::month(Date),
               yday = lubridate::yday(Date)) %>%
        mutate(ELEVATION = as.numeric(ELEVATION), LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
        filter(!is.na(LATITUDE)) %>%
        select(-DATE) -> df2
    
    temps <- bind_rows(df1, df2)
    
    # remove stations with lots of missing data
    temps %>%
        filter(NAME != "SANTA FE, NM US" & NAME != "LEADVILLE LAKE CO AIRPORT CO US" & NAME != "DUTCH HARBOR AK US") %>%
        filter(TMIN > -100 & TMAX < 150) %>%
        select(NAME, ELEVATION, LATITUDE, LONGITUDE, TMIN, TMAX, Date, Year, Month, yday) %>%
        filter(complete.cases(.)) -> temps
    
    # only take into account last 25 years and calculate temp difference
    temps %>%
        filter(Year > 1995) %>%
        select(NAME, Year, Date, TMIN, TMAX, yday, Month) %>%
        mutate(tmpDiff =  TMAX-TMIN) %>%
        separate(NAME, sep="INTERNATIONAL",into=c("NAME", NA)) %>%
        group_by(NAME, Month) %>%
        summarise(meanDiff = mean(tmpDiff, na.rm = TRUE)) %>%
        mutate(label = if_else(Month == max(Month), as.character(NAME), NA_character_)) -> city_data
    
    # create interactive plot
    trendsShared <- SharedData$new(city_data, ~NAME)
    
    ggplot(trendsShared, aes(x=Month, y=meanDiff, color=NAME)) +
        geom_line() +
        theme_ft_rc() +
        guides(color=FALSE) +
        theme(legend.position='none') +
        scale_x_continuous(breaks = 1:12, labels=month.abb, expand = c(0,1)) +
        labs(y="Average temperature difference (F)", title = "Max - min temperatures (averaged monthly)", subtitle = "Averaged monthly") -> city_plot


    output$distPlot <- renderPlotly({
    # temperature flux graph
        ggplotly(city_plot, tooltip = "NAME") %>%
            highlight(on = "plotly_hover", color="red") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                xaxis = list(
                    tickvals = list(1,2,3,4,5,6,7,8,9,10,11,12),
                    showticklabels = TRUE
                ))
    })
        
    temps %>%
        distinct(LATITUDE, LONGITUDE, NAME) %>%
        group_by(NAME) %>%
        summarise_all(list(mean)) -> points
    
    # quick leaflet map
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addCircleMarkers(data = points, color = "blue",  popup = ~as.character(NAME), radius = 10)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
