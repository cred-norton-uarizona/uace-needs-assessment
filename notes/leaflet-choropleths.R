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
counties <- read_sf("dashboard/az_counties/azcounties.shp") %>%
# str(counties)  
  select(NAME, FIPS, geometry)

leaflet(counties) %>%
  addPolygons(
    color = "black", # border color
    weight = 1, # border thickness
    fill = TRUE, # fill the counties
    fillColor = "white", # county fill color,
    fillOpacity = 0.5 # fill opacity
  )

# Need to join labels with the data frame with a pivot_longer()
refine_metric_map <- labels %>% filter(
  Topic == "Health and Well-Being" & Description == "Promoting parenting skills"
  
) %>% 
  pull(Metric)

refine_filtered_map <- data %>%
  select(COUNTY, all_of(refine_metric_map)) %>%
  rename(
    COUNTY = 1,            # Rename column 1 to "COUNTY"
    Response = 2             # Rename column 2 to "Response"
  ) %>% 
# Map data processed
# Later, we'll need to add () to the end of this as a reactive filter
  group_by(COUNTY, Response) %>%
  summarize(n = n()) %>%
  drop_na() %>%
  group_by(COUNTY) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(percent = n/total*100) %>%
  mutate(Response = case_when(Response == 0 ~ "Not at all",
                              Response == 1 ~ "A little",
                              Response == 2 ~ "Somewhat",
                              Response == 3 ~ "Very",
                              Response == 4 ~ "Extremely"),
         Response = factor(Response, level = c("Not at all",
                                               "A little",
                                               "Somewhat", 
                                               "Very",
                                               "Extremely")))


refine_filtered_map_v2 <- refine_filtered_map %>% filter(
  Response %in% c("Not at all", "A little")
) %>% 
  group_by(COUNTY) %>% 
  summarise(percent = sum(percent),
            percent_round = round(sum(percent), digits = 1),
            n = sum(n),
            total = first(total)) %>% 
  mutate(across(.cols = c(percent:total), 
                ~ifelse(n < 6, NA, .)))

counties <- left_join(counties,refine_filtered_map_v2, by = c("NAME" = "COUNTY"))


leaflet(counties) %>%
  addPolygons(
    color = "black", # border color
    weight = 1, # border thickness
    fill = TRUE, # fill the counties
    fillColor = ~colorQuantile("Blues", percent_round)(percent_round), # county fill color,
    fillOpacity = 0.5, # fill opacity
    popup = ~paste(NAME,":", percent_round, "%")
  )

# We want a header that says something like, "You have selected _____.  It reflects XX people who selected ______..."






