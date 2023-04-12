# Plot map showing which county is selected


# Load data  and packages--------------------------------------------------------
# path <- "/Users/ericscott/Library/CloudStorage/Box-Box/CRED - Incubator Collaboration/Data without zips.xlsx"
# 
# 
# 
# library(tidyverse)
# library(readxl)
# 
# data <- read_excel(path)

library(pins)
library(tidyverse)
library(sf)
library(leaflet)
library(ggplot2)
library(maps)
library(stringr)


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
data <- data %>% mutate(COUNTY = tolower(COUNTY))
labels <- read.csv("data/labels.csv", stringsAsFactors = FALSE)


# Get map data
az_counties <- map_data("county", region = "arizona") %>% 
  mutate(COUNTY = str_to_title(subregion))

# Count the number of responses by county
response_count <- data %>%
  group_by(COUNTY) %>%
  summarize(count = n())

# Merge response counts with county map data
az_counties <- left_join(az_counties, response_count, by = "COUNTY")

# Plot the map with the fill colors based on the response counts
ggplot(data = az_counties,
       mapping = aes(x = long, y = lat,
                     group = group, fill = count)) + 
  geom_polygon(color = "black", show.legend = FALSE) +
  scale_fill_gradient(low = "lightgrey", high = "black", na.value = "grey",
                      labels = scales::comma_format()) +
  coord_map() +
  theme_void()


#### Map for item-level data (isn't working yet)

# Compute the percentage of folks who selected 4 as a monochromatic color by county
data_filtered <- data %>%
  filter(!is.na(FCHS_8)) %>%
  group_by(COUNTY) %>%
  summarize(selected_4_count = sum(FCHS_8 == 4),
            total_count = n()) %>%
  mutate(selected_percentage = selected_4_count / total_count)

# Join the data with the map data
az_counties <- left_join(az_counties, data_filtered, by = c("COUNTY" = "COUNTY"))

# Define a color palette to use for the fill colors
color_palette <- colorRampPalette(c("white", "darkblue"))

# Create a function to map percentage values to fill colors
fill_color_func <- function(x) {
  if (is.na(x)) {
    return("grey")
  } else {
    return(color_palette(10)[as.integer(x/10) + 1])
  }
}

# Plot the map with the fill colors based on the percentage values
ggplot(data = az_counties,
       mapping = aes(x = long, y = lat,
                     group = group, fill = selected_percentage)) + 
  geom_polygon(color = "black", show.legend = FALSE) +
  scale_fill_gradientn(colors = color_palette(10), na.value = "grey",
                       labels = scales::percent_format(accuracy = 1)) +
  coord_map() +
  theme_void()
