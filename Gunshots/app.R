library(shiny)
library(sf)
library(janitor)
library(lubridate)
library(gganimate)
library(ggthemes)
library(shinythemes)
library(gifski)
library(png)
library(transformr)
library(shinycssloaders)
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
         date = as.Date(date, format = "%d-%b-%y"),
         year = year(date)) 

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
  
ui <- fluidPage(theme = shinytheme("superhero"),
   
   titlePanel("Gunshots in San Francisco"),
   
# The tech-fueled gentrification of SF, combined with efforts by the city to
# contain and decrease crime in the city, has resulted in noticeable changes in
# geospatial crime data over the last 10 years. Giving users control over the
# year is a good way to allow users to interactively visualize that change

   sidebarLayout(
      sidebarPanel(
         radioButtons("year",
                      "Year:",
                      choices = c("2013", "2014", "2015"),
                      selected = "2014")
      ),
      
      mainPanel(
         textOutput("description"),
         withSpinner(imageOutput("shotPlot"))
      )
   )
)

server <- function(input, output) {
   
  sf_shots_re <- reactive({
    
    sf_shots %>%
      filter(year == input$year)
    
    })
  
  output$description <- renderText("This plot was made using data from the ShotSpotter project by the Justice Tech Lab. Big thanks to Justice Tech Lab for putting the dataset together and making it freely accessible to the public! \n The code for this Shiny App can be accessed at https://github.com/ben-hb/san-francisco-gunshots")
  
  output$shotPlot <- renderImage({
    
# Creating a temporary file prior to the plot is necessary in order export the
# plot as a gif
    
    outfile <- tempfile(fileext='.gif')
    
    plot <- ggplot() +
      geom_sf(data = sf_shape) +
      
# I use red as the color for dots because I need the dots to stand out against
# the black and white crowded background and because the red is a visual
# allusion to the bloody nature of gunshots
      
      geom_sf(data = sf_shots_re(), color = "red", alpha = 0.5) +
      
# The black and white theme, as recommended by Healy, is a good way to have a
# simple and quick-loading theme that won't distract from the animation
      
      theme_bw() + 
      transition_time(hour) + 
      labs(
        title = "Gunshot Deaths in San Francisco",
        subtitle = paste("Time = {frame_time}/24 in ", input$year),
        caption = "Data from ShotSpotter"
      )
    
    anim_save("outfile.gif", animate(plot))
    
    list(
      src = "outfile.gif",
      contentType = 'image/gif'
    )
    
  })
  
}

shinyApp(ui = ui, server = server)

