library(pins)
library(tidyverse)
library(plotly)
library(forcats)


board <- board_connect()
data <- pin_read(board, "terrace/uace-na")

# names(data)




# Demographic distribution/donut plots, interactive
# Urban or Rural
# LIVE_V3

colors_location <- c("Rural" = "#4e8542", "Urban" = "#594a6a", "No Response" = "#9f2936")

data %>%
  mutate(LIVE_V3 = ifelse(is.na(LIVE_V3), "No Response", LIVE_V3)) %>%
  group_by(LIVE_V3) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~LIVE_V3, values = ~count, type = 'pie', hole = 0.5,
          marker = list(colors = colors_location)) %>%
  layout(title = "Location",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# UserLanguage

colors_userlanguage <- c("English" = "#2b556d", "Spanish" = "#9f2936")

data %>%
  group_by(UserLanguage) %>%
  summarize(count = n()) %>%
  mutate(frac = count / sum(count), 
         percent = paste0(round(frac*100), "%")) %>%
  plot_ly(labels = ~paste0(UserLanguage, "<br>", percent), values = ~count, 
          type = 'pie', hole = 0.5, 
          marker = list(colors = colors_userlanguage)) %>%
  layout(title = "Language",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# GENDER
colors_gender <- c("Woman" = "#1b587c", "Man" = "#9f2936", 
                   "Non-binary" = "#f07f09", "No Response" = "#f2f2f2")
data %>%
  mutate(Gender = ifelse(is.na(Gender), "No Response", Gender)) %>%
  group_by(Gender) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~Gender, values = ~count, type = 'pie', hole = 0.5,
          marker = list(colors = colors_gender)) %>%
  layout(title = "Gender",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# AGE
total_count <- nrow(data)

data %>%
  group_by(AGE) %>%
  summarize(count = n()) %>%
  mutate(percent = paste0(round(count / total_count * 100, 1), "%")) %>%
  plot_ly(x = ~count, y = ~AGE,
          type = 'bar',
          orientation = 'h',
          text = ~percent,
          marker = list(color = "#4e8542")) %>% 
  layout(xaxis = list(title = ''),
         yaxis = list(title = ''))

# data %>%
#   group_by(AGE) %>%
#   summarize(count = n()) %>%
#   plot_ly(labels = ~AGE, values = ~count) %>%
#   add_pie(hole = 0.5) %>%
#   layout(title = "Age",  
#          showlegend = TRUE,
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# DEM_11 # educational attainment, maybe don't show all?

data %>%
  drop_na(DEM_11) %>%
  mutate(DEM_11 = factor(DEM_11, levels = c("Less than high school diploma",
                                            "High school diploma or GED",
                                            "Some college",
                                            "Trade/technical/vocational training",
                                            "Associate's degree",
                                            "Bachelor's degree",
                                            "Graduate or professional degree"))) %>%
  group_by(DEM_11) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = paste0(round(count/sum(count)*100, 1), "%")) %>%
  plot_ly(x = ~count, y = ~DEM_11,
          type = 'bar',
          orientation = 'h',
          text = ~percent,
          marker = list(color = "#f07f09")) %>%
  layout(title = "Educational Attainment",
         showlegend = FALSE,
         xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
         yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))


# data %>%
#   group_by(DEM_11) %>%
#   summarize(count = n()) %>%
#   plot_ly(labels = ~DEM_11, values = ~count) %>%
#   add_pie(hole = 0.5) %>%
#   layout(title = "Educational Attainment",
#          showlegend = FALSE,
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# names(data)
# str(data$DEM_13)
# Income

data %>%
  drop_na(DEM_13) %>%
  mutate(DEM_13 = fct_relevel(factor(DEM_13), rev(c("$200,000 and above",
                                                    "$150,000 to $199,999",
                                                    "$100,000 to $149,999",
                                                    "$75,000 to $99,999",
                                                    "$50,000 to $74,999",
                                                    "$35,000 to $49,999",
                                                    "$25,000 to $34,999",
                                                    "Less than $25,000")))) %>%
  group_by(DEM_13) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percent = paste0(round(count/sum(count)*100, 1), "%")) %>%
  plot_ly(x = ~count, y = ~DEM_13,
          type = 'bar',
          orientation = 'h',
          text = ~percent,
          marker = list(color = "#594a6a")) %>%
  layout(title = "Household Income",
         showlegend = FALSE,
         xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
         yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE))





# race_ethnicity

# Ask Jessica for help getting rid of the percent as decimal
race_vec <- c("American Indian or Alaska Native" = "AIAN",
              "Asian" = "AS",
              "Black or African American" = "BL", 
              "Hispanic or Latino" =  "HL",
              "Multiracial" = "MR", 
              "Native Hawaiian or Other Pacific Islander" = "NHPI", 
              "White" = "WH" , 
              "Prefer not to answer" = "NR") 

data %>%
  select(race_vec) %>%
  pivot_longer(cols = everything(), 
               names_to = "race_ethnicity") %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = c("American Indian or Alaska Native", 
                                                            "Asian",
                                                            "Black or African American", 
                                                            "Hispanic or Latino", 
                                                            "Multiracial", 
                                                            "Native Hawaiian or Other Pacific Islander", 
                                                            "White", "Prefer not to answer"))) %>%
  filter(value == 1) %>%
  group_by(race_ethnicity) %>%
  summarize(count = n(),
            frac = n()/nrow(.)) %>%
  mutate(percent = sprintf("%d%%", round(frac*100))) %>%
  plot_ly(x = ~frac, y = ~fct_rev(race_ethnicity),
          type = 'bar',
          orientation = 'h',
          text = ~percent) %>% 
  # ~paste(percent, race_ethnicity)) %>%
  layout(xaxis = list(title = ''),
         yaxis = list(title = ''))



# Cooperative Extension User and Familiar



data %>%
  pivot_longer(cols = c("CE_USER", "CE_EXPOSED"), 
               names_to = "variable") %>%
  filter(value == "Yes") %>%
  group_by(variable) %>%
  summarize(count = n(),
            frac = count/nrow(data)) %>%
  mutate(variable = ifelse(variable == "CE_USER", "User of Extension", "Familiar with Extension"),
         percent = paste0(round(frac * 100), "%")) %>%
  plot_ly(x = ~frac, y = ~variable,
          type = 'bar',
          orientation = 'h',
          text = ~percent,
          hoverinfo = "text",
          marker = list(color = "#9f2936")) %>% 
  layout(xaxis = list(title = ""),
         yaxis = list(title = ""),
         title = list(text = "Extension Usage and Familiarity Among Respondents",
                      font = list(size = 20)),
         margin = list(l = 150, b = 150))








# Cooperative Extension Topical Experience

topical_exp_vec <- c("Agriculture" = "AG_EXPERIENCE", 
                     "Education & Youth Development" = "ED_EXPERIENCE", 
                     "Health & Well-Being" = "HLTH_EXPERIENCE", 
                     "Natural Resources" =  "NR_EXPERIENCE", 
                     "Community & Economic Development" = "CD_EXPERIENCE")





# Cooperative Extension Topical Knowledge
topical_knw_vec <- c("Agriculture" = "AG_KNOWLEDGE",
                     "Education & Youth Development" = "ED_KNOWLEDGE",
                     "Health & Well-Being" = "HLTH_KNOWLEDGE",
                     "Natural Resources" = "NR_KNOWLEDGE",
                     "Community & Economic Development" = "CD_KNOWLEDGE")





# Gauge chart for sample size

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = nrow(data), # substitute with actual sample size
  title = list(text = "Sample size"),
  type = "indicator",
  mode = "gauge+number",
  # delta = list(reference = 380),
  gauge = list(
    axis =list(range = list(NULL, nrow(data)),# include maximum
               nticks = 5), 
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = nrow(data))))  # include maximum
fig <- fig %>%
  layout(margin = list(l=20,r=30))
fig

# CE_EXPOSED
data %>%
  group_by(CE_EXPOSED) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~CE_EXPOSED, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "County Extension Exposure",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# CE_USER
data %>%
  group_by(CE_USER) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~CE_USER, values = ~count) %>%
  add_pie(hole = 0.5) %>%
  layout(title = "County Extension User",  
         showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))