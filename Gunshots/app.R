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
  
ui <- fluidPage(
   
   titlePanel("Gunshots in San Francisco"),
   
   sidebarLayout(
      sidebarPanel(
         radioButtons("year",
                      "Year:",
                      choices = c("2013", "2014", "2015"),
                      selected = "2014")
      ),
      
      mainPanel(
         imageOutput("shotPlot"),
         textOutput("description")
      )
   )
)

server <- function(input, output) {
   
  sf_shots_re <- reactive({
    
    sf_shots %>%
      filter(year == input$year)
    
    })
  
  output$description <- renderText(md("This plot was made using data from the ShotSpotter project by the Justice Tech Lab. Big thanks to Justice Tech Lab for putting the dataset together and making it freely accessible to the public! \n The code for this Shiny App can be accessed [here](https://github.com/ben-hb/san-francisco-gunshots)"))
  
  output$shotPlot <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    plot = ggplot() +
      geom_sf(data = sf_shape) +
      geom_sf(data = sf_shots_re()) +
      theme_bw() + 
      transition_time(hour) + 
      labs(
        title = "Gunshot Deaths in San Francisco",
        subtitle = "Time = {frame_time}/24 in {input$year()}",
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

