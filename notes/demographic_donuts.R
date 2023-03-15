library(pins)
library(tidyverse)

board <- board_connect()
data <- pin_read(board, "terrace/uace-na")
data <- data %>% mutate(across(
  c(CE_USER, CE_EXPOSED, Low_Income_FPL_100, Low_Income_FPL_185),
  function(x) ifelse(x == 1, "Yes", "No")))

# Demographic distribution/donut plots
# Urban or Rural
# LIVE_V3

data %>%
  group_by(LIVE_V3) %>%
  summarize(count = n(),
            frac = n()/nrow(.)) %>%
  mutate(ymax = cumsum(frac),
         ymin = ymax - frac) %>%
  ggplot(aes(ymax = ymax, ymin = ymin, 
             xmax = 4, xmin =3,
             fill = LIVE_V3)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "left") +
  guides(fill = guide_legend(title = "Location"))


# UserLanguage
  data %>%
    group_by(UserLanguage) %>%
    summarize(count = n(),
              frac = n()/nrow(.)) %>%
    mutate(ymax = cumsum(frac),
           ymin = ymax - frac) %>%
    ggplot(aes(ymax = ymax, ymin = ymin, 
               xmax = 4, xmin =3,
               fill = UserLanguage)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(1, 4)) +
    theme_void() +
    theme(legend.position = "left") +
    guides(fill = guide_legend(title = "Language"))  
  
# GENDER
  
  data %>%
    group_by(GENDER) %>%
    summarize(count = n(),
              frac = n()/nrow(.)) %>%
    mutate(ymax = cumsum(frac),
           ymin = ymax - frac) %>%
    ggplot(aes(ymax = ymax, ymin = ymin, 
               xmax = 4, xmin =3,
               fill = GENDER)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(1, 4)) +
    theme_void() +
    theme(legend.position = "left") +
    guides(fill = guide_legend(title = "Gender")) 
  
AGE
DEM_11 # educational attainment, maybe don't show all?
Low_Income_FPL_100 # to simplify?
CE_EXPOSED
CE_USER

race_ethnicity

race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latinx" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 