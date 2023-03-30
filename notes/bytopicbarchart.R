# Ranked by topic
# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(pins)

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
# ggplot prototype

ggplot(data_bytopic, aes(x = fct_rev(Description), y = percent)) +
  geom_col(aes(fill = Response)) +
  coord_flip() +
  theme(legend.position = "bottom")
