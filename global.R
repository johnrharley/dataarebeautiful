# setup
library(leaflet, verbose = FALSE)
library(htmltools, verbose = FALSE)
library(htmlwidgets, verbose = FALSE)
library(tidyverse, verbose = FALSE)
library(rvest, verbose = FALSE)
library(plotly, verbose = FALSE)
library(ggmap, verbose = FALSE)
library(shinythemes, verbose = FALSE)
library(shiny, verbose = FALSE)
library(aws.s3, verbose=FALSE)

# scrape the dhss website
url <- "http://www.dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/monitoring.aspx"

url  %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField"]/div[2]/table') %>%
  html_table(header = TRUE, trim=TRUE) %>%
  .[[1]] -> covid_cases

colnames(covid_cases) <- c("Region", "Travel-Related", "Non-Travel", "Close-Contact", "Under-Investigation", "Total")

# sometimes there are some blank space characters in the table, they need to be removed
if(is.character(covid_cases$`Non-Travel`)) {
covid_cases %>%
  mutate(`Non-Travel` = as.numeric(gsub(`Non-Travel`, 
            pattern = "[^[:alnum:][:blank:]?&/\\-]" , replacement = ""))) -> covid_cases
} else {
  NULL
}

# filter out the regional summaries
covid_cases %>% filter(Region != "TOTAL") -> covid_cases

covid_cases %>%
  filter(Region %in% c("Gulf Coast", "Interior", "Municipality  of Anchorage", 
                       "Mat-Su", "Northern", "Southeast", "Southwest",  covid_cases$Region[1])) -> region # this is hacky but there's some weird formatting in the table

cities <- setdiff(covid_cases, region)
# get geolocation data from google geolocate API
register_google(key = Sys.getenv("google_key"))

gis <- geocode(paste(cities$Region, "Alaska", sep=", "), quiet=TRUE)
city_data <- cbind(gis, cities)

# initial parameters for leaflet map
initial_lat <- 62
initial_lng <- -145
initial_zoom <- 4

# this extracts the updated date info from the page. likely will change
url  %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField"]/div[2]/ul[2]/li[4]') %>%
  html_text() %>%
  str_replace_all(pattern = ";", "") %>%
  substr(0, 22) -> updated_date

# testing over time

counts <- data.table::fread("http://www.dhss.alaska.gov/dph/Epi/id/SiteAssets/Pages/HumanCoV/COVID-19_epi_testingcurve.csv")

# the following function will pull the archived data for the daily counts from a private S3 bucket (AWS)

# we can pull the metadata which tracks the last time it was update. if it was last updated more than a day
# ago we will update it with the current counts from DHHS

# custom function to remove rownames from write.csv
mywrite <- function(x, file) {
  write.csv(x, file, row.names=FALSE)
}

# amazon system key
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
           "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"))

old_cases <- s3read_using(object="current_cases.csv",FUN=read.csv, bucket="dataarebeautiful") %>%
  mutate(Date = as.Date(as.character(Date)))
old_cases_metadata <- get_bucket_df(bucket="dataarebeautiful")

updateData <- function() {
if(as.Date(old_cases_metadata$LastModified) - Sys.Date() > 0) {
  new_day <- data.frame(Date = as.Date(substr(updated_date, 9, 22), format="%B %d, %Y"), PositiveCases = sum(cities$Total))

  Totalcovid_cases <<- union(old_cases, new_day)   
  s3write_using(Totalcovid_cases, object="current_cases.csv", mywrite, bucket='dataarebeautiful')
  
  } else {
    Totalcovid_cases <<- old_cases
  } 
}

updateData()
#
