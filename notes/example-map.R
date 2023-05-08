# Plot map showing which county is selected


# Load data  and packages--------------------------------------------------------
path <- "/Users/ericscott/Library/CloudStorage/Box-Box/CRED - Incubator Collaboration/Data without zips.xlsx"

library(tidyverse)
library(readxl)

data <- read_excel(path)
data |> mutate(COUNTY = tolower(COUNTY))

# Get map data -------
az_counties <- map_data("county", region = "arizona") |>
  mutate(COUNTY = str_to_title(subregion))


#join data by column `subregion`
test <- az_counties %>%
  mutate(selected = case_when(COUNTY == "Maricopa" ~ TRUE,
                              .default = FALSE))
# Plot --------------------------------------------------------------------
ggplot(data = test,
       mapping = aes(x = long, y = lat,
                     group = group, fill = selected)) + 
  geom_polygon(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#2b556d", "FALSE" = "#89c3e5")) +
  coord_map() +
  theme_void() +
  annotate("text", x = -118, y = 34.5, label = "N = \n3236",
           size = 10,
           vjust = 0.25,
           hjust = 0)
