# Ranked by topic
# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)

break_string <- function(x, n) {
  # x is a character string
  # n is number of characters before the break
  out_string <- stringi::stri_wrap(x, n)
  out_n <- length(out_string)
  
  ifelse(out_n == 1, out_string, 
         paste0(out_string[1], "\n", out_string[2]))
}



board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
labels <- read.csv(here::here("data", "labels.csv"))

cnames <- labels$Metric[labels$Topic == "Agriculture"] # substitute with input$topic

# For range of sample size (we're not including NAs)
n_range <- data %>% # substitute with refine_top_20()
  select(all_of(cnames)) %>%
  pivot_longer(cols = everything(),
               names_to = "Metric",
               values_to = "Response") %>%
  drop_na() %>%
  count(Metric) %>%
  summarize(range = range(n)) %>%
  pull(range)

# Make E + V only for ranking purposes
rank_EV <- data %>% # substitute with refine_top_20()
  select(all_of(cnames)) %>%
  pivot_longer(cols = everything(),
               names_to = "Metric",
               values_to = "Response") %>%
  group_by(Metric, Response) %>%
  summarize(n = n()) %>%
  drop_na() %>%
  group_by(Metric) %>%
  mutate(total = sum(n)) %>%
  filter(Response %in% 3:4) %>%
  summarize(totalEV = sum(n),
            total = unique(total)) %>%
  mutate(percentEV = round(totalEV/total*100)) %>%
  arrange(desc(percentEV)) %>%
  left_join(labels, by = "Metric") %>%
  pull(Description)
  
# Get long dataset for plotting
data_bytopic <- data %>% # substitute with refine_top_20()
  select(all_of(cnames)) %>%
  pivot_longer(cols = everything(),
               names_to = "Metric",
               values_to = "Response") %>%
  group_by(Metric, Response) %>%
  summarize(n = n()) %>%
  drop_na() %>%
  group_by(Metric) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(percent = round(n/total*100)) %>%
  left_join(labels, by = "Metric") %>%
  mutate(Description = factor(Description, levels = rank_EV),
         Response = case_when(Response == 0 ~ "Not at all",
                              Response == 1 ~ "A little",
                              Response == 2 ~ "Somewhat",
                              Response == 3 ~ "Very",
                              Response == 4 ~ "Extremely"),
         Response = factor(Response, level = c("Not at all",
                                               "A little",
                                               "Somewhat", 
                                               "Very",
                                               "Extremely")))

str(data_bytopic)

# data labels - only for top 3 rankings
percent_labels <- data_bytopic %>%
  filter(Response %in% c("Extremely", "Very", "Somewhat")) %>% 
  arrange(Description, desc(Response)) %>%
  group_by(Description) %>%
  mutate(location_half = percent/2,
         location_prev = lag(percent),
         location_prev2 = lag(location_prev),
         y = rowSums(across(starts_with("location")), 
                     na.rm = TRUE))

# Make vector of x labels
xlabs <- sapply(levels(data_bytopic$Description), break_string, 50)

# ggplot prototype

ggplot() +
  geom_col(data = data_bytopic, aes(x = fct_rev(Description), 
                                    y = percent,
                                    fill = Response)) +
  geom_text(data = percent_labels,
            aes(x = fct_rev(Description), 
                y = y, 
                label = paste0(percent, "%")),
            vjust = 0.5, hjust = 0.5,
            color = "white", size = 3.5) +
  scale_x_discrete(labels = xlabs) +
  scale_fill_manual(values = colors_ag) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(subtitle = paste0("Between ", n_range[1], " and ", n_range[2], 
                         " participants responded to each item")) +
  theme(
    #legend.position = "none",
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank()
    # axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
    # axis.text.y = element_blank()
  )


# Color 
# Set the colors for each topical area
colors_health <- c("Extremely" = "#30243c", "Very" = "#5a4a6a", "Somewhat" = "#a088b7", "A little" = "#bfafcf", "Not at all" = "#dfd7e7")
colors_education <- c("Extremely" = "#783f05", "Very" = "#b45f07", "Somewhat" = "#f9b268", "A little" = "#fbcc9a", "Not at all" = "#fde5cd")
colors_ag <- c("Extremely" = "#274221", "Very" = "#4e7345", "Somewhat" = "#8ec182", "A little" = "#b3d6ac", "Not at all" = "#d9ead5")
colors_nr <- c("Extremely" = "#0e2c3e", "Very" = "#2b556d", "Somewhat" = "#4ea5d8", "A little" = "#89c3e5", "Not at all" = "#c4e1f2")
colors_ced <- c("Extremely" = "#c39001", "Very" = "#f5b501", "Somewhat" = "#fecf4c", "A little" = "#fee398", "Not at all" = "#fff6dd")