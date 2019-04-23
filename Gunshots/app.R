library(shiny)
library(sf)
library(janitor)
library(lubridate)
library(gganimate)
library(ggthemes)
library(tidyverse)

# All of the code before the fluidPage() call is static -- it involves no
# reactive variables and doesn't need to change throughout any of the UI or
# server calculations

# I'm using data from the ShotSpotter project by the Justice Tech Lab. Big
# thanks to them for making this data freely accessible!

# I opt to download the raw data and include it in the repo because the data is
# historical so I don't expect it to change, making the possibility of the data
# no longer being accessible a greater concern than the upside of continuously
# updating the output

sf_shots_raw <- read_csv("San_Francisco_ShotSpotter.csv",
                              col_types = cols(
                                Type = col_character(),
                                ID = col_double(),
                                Date = col_character(),
                                Time = col_time(format = ""),
                                Rnds = col_double(),
                                Beat = col_logical(),
                                DISPO = col_logical(),
                                `returned address` = col_character(),
                                Latitude = col_double(),
                                Longitude = col_double()
                              )) %>% 
  clean_names() %>% 
  mutate(hour = hour(time),
         date = as.Date(date, format = "%d-%b-%y"))

sf_shots = st_as_sf(sf_shots_raw, 
                    coords = c("longitude", "latitude"), 
                    crs = 4326)

# I had some challenges with the Tigris shapefile for San Francisco, so I found
# an alternate shapefile published by the DataSF open data project
# (data.sfgov.org) Since I don't expect the shapefile to change, and any
# potential changes would likely only cause problems for the visualization, I'm
# including the shapefile in the repository rather than reading it in directly
# from the website

# It's important for the crs on the shape file to match the crs on the shots
# file. Even though I could plot the shape file without specifying the crs, it
# will produce an error message when plotting in the same ggplot() call as the
# shots file if the crs isn't specified

sf_shape <- read_sf("sf_shape.shx",
                    crs = 4326)  

ggplot(data = sf_shape) +
  geom_sf() +
  geom_sf(data = sf_shots)
  
ui <- fluidPage(
   
   titlePanel("Gunshots in San Francisco"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("hour",
                     "Hour:",
                     min = 1,
                     max = 23,
                     value = 12)
      ),
      
      mainPanel(
         plotOutput("shotPlot")
      )
   )
)

server <- function(input, output) {
   
  sf_shots_re <- reactive({
    
    sf_shots %>%
      filter(hour == input$hour)
    
    })
  
  output$shotPlot <- renderPlot({
    ggplot() +
      geom_sf(data = sf_shape) +
      geom_sf(data = sf_shots) +
      theme_map()
  })
  
}

shinyApp(ui = ui, server = server)

