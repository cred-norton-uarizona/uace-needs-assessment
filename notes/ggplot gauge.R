library(pins)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(grid)
# install.packages("grid")

# read in data
board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
# labels <- read.csv("labels.csv")



# create a sample data frame with filters applied
df_plot <- data %>%
  filter(COUNTY == "Apache")

# count the number of rows in the filtered data frame
n_rows <- as.integer(nrow(df_plot))

# set gauge parameters
n_value <- n_rows
total_value <- nrow(data)
radius <- 1.25
thickness <- 0.25
start_angle <- pi
end_angle <- pi/2 + (n_value/total_value) * pi

plot.new()

# Define half_donut function using polygon()
half_donut <- function(x, y, radius, thickness, start_angle, end_angle) {
  angles <- seq(start_angle, end_angle, length.out = 50)
  outer_x <- x + cos(angles) * (radius + thickness/2)
  outer_y <- y + sin(angles) * (radius + thickness/2)
  inner_x <- x + cos(rev(angles)) * (radius - thickness/2)
  inner_y <- y + sin(rev(angles)) * (radius - thickness/2)
  path <- polygon(c(outer_x, inner_x), c(outer_y, inner_y))
  return(path)
}

ggplot() +
  # add gauge background
  geom_polygon(data = data.frame(x = 0, y = 0), aes(x = x, y = y, group = 1), fill = "white", color = "grey", size = 2, alpha = 0.2) +
  # add gauge fill
  geom_polygon(data = df_plot, aes(x = x, y = y, group = 1, fill = "fill"), 
               color = "black", size = 2, alpha = 0.8, linejoin = "round", inherit.aes = FALSE,
               d = half_donut(0, 0, radius, thickness, start_angle, end_angle)) +
  # add text label for n-value
  annotate("text", x = 0, y = -0.5, label = paste0(n_value, "/", total_value), size = 8, color = "black", fontface = "bold") +
  # add theme
  theme_void() +
  # set plot limits
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
