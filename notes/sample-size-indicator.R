library(tidyverse)
library(ggforce) #for geom_arc_bar()

#example data.
#`data` is the full data frame
data <- mtcars
#`data_filtered` is the data frame after filtering
data_filtered <- mtcars |> slice_head(n = 10)

df <-
  tibble(
    # get the number of rows (observations) for after and before filtering
    count = c(nrow(data_filtered), nrow(data)),
    category = c("filtered", "total")
  ) |> 
  # convert to coordinates of a bar
  mutate(
    fraction = count/sum(count),
    start = cumsum(fraction),
    end = c(0, head(start, n=-1))
  )

ggplot(df) +
  geom_arc_bar(aes(
    x0 = 0,
    y0 = 0, 
    r0 = 0.5,
    r = 1,
    #convert to radians and shift by 180º counterclockwise
    start = start * pi - pi/2, 
    end = end * pi - pi/2,
    fill = category
  )) +
  annotate(geom = "text", label = nrow(data_filtered), x = 0, y = 0.1, size = 18) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")
