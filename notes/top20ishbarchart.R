# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(pins)

board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv(here::here("data", "labels.csv"))

# Test filtering the data

# % Extremely or Very Important Selected, Top 30 for Now-------


data_Evimp <- data %>% group_by(COUNTY) %>%
summarize(across(
  ends_with("Evimp"),
  ~sum(.x == 1, na.rm = TRUE)/n())*100)

data_Evimp <- data_Evimp %>% pivot_longer(-COUNTY,
                                    names_to = "Metric",
                                    values_to = "Percentage"
) %>%
  group_by(COUNTY) %>% 
  arrange(desc(Percentage)) %>% 
  mutate(Metric = gsub("_EVimp", "", Metric))%>%
  slice(1:30)  # %>% 
 #  select(Metric, Percentage, COUNTY)

labels <- labels %>% 
  mutate(Topic = case_when(
    str_detect(Metric, "FCHS_") ~ "Health and Well-Being",
    str_detect(Metric, "YD_") ~ "Education",
    str_detect(Metric, "AG_") ~ "Agriculture",
    str_detect(Metric, "NR_") ~ "Natural Resources", 
    str_detect(Metric, "CED_") ~ "Community and Economic Development",
    TRUE ~ 'NA'
  ))


data_joined <- left_join(data_Evimp, labels) %>% 
  select(COUNTY, Topic, Metric, Description, Percentage)

# colnames(data_joined)

# print(data_joined)

# I think that the mistake that I've made here is that I need to do this work connected to the full data frame and not a cut
# This isn't going to help us, because we won't have access to other data for filters

plot_df <- data_joined |> 
  arrange(desc(Percentage)) |> 
  ungroup() |> 
  mutate(Percentage = round(Percentage)/100) |> 
  filter(Percentage >= nth(Percentage, 20)) |> 
  mutate(row = 1:n())


colors <- c("Health and Well-Being" = "#604878", "Natural Resources" = "#1B587C", "Agriculture" = "#4E8542", "Community and Economic Development" = "#C09001", "Education" = "#C65A11")

ggplot(plot_df, aes(x = row, y = Percentage)) + 
  geom_col(aes(fill = Topic), width = 0.7, ) +
  geom_text(aes(label = paste(Description, scales::percent(Percentage, accuracy = 1), sep = ", ")), vjust = 0.5, hjust = "right", color = "white", size = 4) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_reverse()+
  scale_fill_manual(values = colors)+
  coord_flip() +
  labs(title = "Top Priorities", # We can explore how to add more than one county name
       subtitle = "Percent of respondents who selected 'extremely' or 'very' important") +
  theme(
    #legend.position = "none",
    legend.position = "left",
    plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
    axis.text.y = element_blank()
  )

# Descriptions in the bars
# Hex colors for each topical area (custom colors)
# Legend
