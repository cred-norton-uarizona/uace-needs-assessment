# Plot map showing which county is selected


# Load data  and packages--------------------------------------------------------
path <- "/Users/ericscott/Library/CloudStorage/Box-Box/CRED - Incubator Collaboration/Data without zips.xlsx"

library(tidyverse)
library(readxl)

data <- read_excel(path)
data |> mutate(COUNTY = tolower(COUNTY))

# Get map data -------
az_counties <- map_data("county", region = "arizona")
az_counties <- az_counties |>
  mutate(selected = ifelse(subregion == "apache", TRUE, FALSE))

#join data by column `subregion`

# Plot --------------------------------------------------------------------
ggplot(data = az_counties,
       mapping = aes(x = long, y = lat,
                     group = group, fill = selected)) + 
  geom_polygon(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "darkblue", "FALSE" = "grey")) +
  coord_map() +
  theme_void()
