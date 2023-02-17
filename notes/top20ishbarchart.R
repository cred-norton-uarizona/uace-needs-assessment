# Load the data and libraries  -----------------------------------------------------------
library(tidyverse)
library(readxl)

data <- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Data without zips.xlsx")


# % Extremely or Very Important Selected, Top 30 for Now-------

# Jessica and Eric, we need to think about how to deal is ties for 20th place.
# We want to sort in descending (largest to smallest) by the tenths place
# and then show items that tied for 20th place when rounded to the ones place.

I keep getting this error.  Can you help fix my code?

  Error in layer(data = data, mapping = mapping, stat = "identity", geom = GeomCol,  : 
                   object 'Description' not found

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


labels<- read_excel("C:/Users/Terrace Ewinghill/Box/Cooperative Extension Needs Assessment 2022/CRED - Incubator Collaboration/Labels for the content areas in survey.xlsx")

labels <- labels %>% 
  mutate(Topic = case_when(
    str_detect(Metric, "FCHS_") ~ "Health and Well-Being",
    str_detect(Metric, "YD_") ~ "Education",
    str_detect(Metric, "AG_") ~ "Agriculture",
    str_detect(Metric, "NR_") ~ "Natural Resources", 
    str_detect(Metric, "CED_") ~ "Community and Econonic Development",
    TRUE ~ 'NA'
  ))


data_joined <- left_join(data_Evimp, labels) %>% 
  select(COUNTY, Topic, Metric, Description, Percentage)

print(data_joined)

# I think that the mistake that I've made here is that I need to do this work connected to the full data frame and not a cut
# This isn't going to help us, because we won't have access to other data for filters


ggplot(data_joined, aes(x = Description, y = Percentage)) + 
  geom_col(fill = Description, width = 0.7) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 1)), vjust = 0.5, hjust = 1.2, color = "white", size = 4) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Top Priorities",
       subtitle = "Percent of respondents who selected 'extremely' or 'very' important") +
  theme(
    plot.title = element_text(size = 18, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(10, 0, 10, 0), color = "gray"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0)),
  )

# I am getting an error that 'Description' doesn't exist
