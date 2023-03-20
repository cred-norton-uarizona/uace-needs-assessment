library(pins)
library(tidyverse)
library(sf)
library(leaflet)

board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

data %>%
  count(COUNTY) %>%
  arrange(n) %>%
  mutate(county = fct_inorder(COUNTY)) %>%
  ggplot(aes(x = county, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

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
  addPolygons()
