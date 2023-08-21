library(pins)
library(tidyverse)
library(sf)
library(leaflet)

board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# # county bar chart
# data %>%
#   count(COUNTY) %>%
#   arrange(n) %>%
#   mutate(county = fct_inorder(COUNTY)) %>%
#   ggplot(aes(x = county, y = n)) +
#   geom_bar(stat = "identity") +
#   coord_flip()

# AZ counties shapefile
# Download and unzip (once)
counties_url <- "https://pubs.usgs.gov/ds/121/arizona/az3/azcounty.zip"
counties_path <- "dashboard/az_counties.zip"
download.file(counties_url, destfile = counties_path)
unzip(counties_path, exdir = "dashboard/az_counties")

# read in counties shapefile
counties <- read_sf("dashboard/az_counties/azcounties.shp")
str(counties)

leaflet(counties) %>%
  addPolygons(
    color = "black", # border color
    weight = 1, # border thickness
    fill = TRUE, # fill the counties
    fillColor = "#2271b8", # county fill color,
    fillOpacity = 0.5 # fill opacity
  )


# Need to join labels with the data frame with a pivot_longer()

# Then, we can filter by Topic, Metric and then display description and percent

# We have code that does this in Top 20 for 'extreme' plus 'very' 


