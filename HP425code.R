
install.packages("haven")
install.packages("janitor")
library(haven)
library(janitor)
library(tidyverse)


SCOT_ISD <- read_dta("C:/Users/VictoriaZaitceva/Desktop/personal/HP425/HP425_summative dataset_2022-23.dta")

# count of deaths
SCOT_ISD %>% group_by(death) %>% summarise(N=n())
# death = 0 - patients are right-censored

SCOT_ISD <- SCOT_ISD %>%
  mutate(mineadmdate = as.Date(mineadmdate, format = "%Y-%m-%d"),
         edeathdate = as.Date(edeathdate, format = "%Y-%m-%d"),
         elastdate = as.Date(elastdate, format = "%Y-%m-%d"))

# Time to death in days
SCOT_ISD <- SCOT_ISD %>%
  mutate(timetodeath = as.numeric(edeathdate - mineadmdate), 
         timetodeath = ifelse(is.na(edeathdate), NA, timetodeath)) 
# replace the values of the timetodeath variable with NA for cases where edeathdate is missing

# sum statistics
summary(SCOT_ISD$timetodeath)



SCOT_ISD <- SCOT_ISD %>%
  # Step 1: Create censoring identifier
  mutate(censored = if_else(death == 0, 1, 0)) %>%
  # Step 2: Calculate time to censoring 
  mutate(timetocensoring = if_else(censored == 1, as.numeric(elastdate - mineadmdate), NA_real_))

summary(SCOT_ISD$timetocensoring)


SCOT_ISD <- SCOT_ISD %>%
  mutate(timetoevent = if_else(censored == 0, timetodeath, timetocensoring))

summary(SCOT_ISD$timetoevent)

# create discharge date
SCOT_ISD <- SCOT_ISD %>%
  mutate(edisdate = as.Date(mineadmdate + los))

#LoS
summary(SCOT_ISD$los)

#proportion of patints with LoS <> 20 days
sum(SCOT_ISD$los >= 20, na.rm = TRUE) / length(na.omit(SCOT_ISD$los)) #13.5%
sum(SCOT_ISD$los < 20, na.rm = TRUE) / length(na.omit(SCOT_ISD$los)) #86.5%

#age
summary(SCOT_ISD$ageyrs)
# proportion of patients <> 50 years
sum(SCOT_ISD$ageyrs > 50, na.rm = TRUE) / length(na.omit(SCOT_ISD$ageyrs)) #90.75%
sum(SCOT_ISD$ageyrs <= 50, na.rm = TRUE) / length(na.omit(SCOT_ISD$ageyrs)) #9.25%

#sex
#long way
SCOT_ISD %>% group_by(female) %>% summarise(count = n()) %>%
  mutate(total = sum(count),  
         prop = count / total) #52% male and 48% female
#short way (janitor)
SCOT_ISD %>%
  tabyl(female)

#comorbidities
SCOT_ISD %>%
  tabyl(comorb) #27.8% no, 72.2% yes

#operation
SCOT_ISD %>%
  tabyl(oper) #93.5% yes, 6.46% yes

# a new variable equal to the overall mean proportion of time to death
mtimetodeaths_all_mean <- mean(SCOT_ISD$timetodeath, na.rm = TRUE) # Mean time to death regardless of age is 2030.793 days for the uncensored observations


# age groups

SCOT_ISD <- SCOT_ISD %>%
  mutate(agegrp1 = case_when(
    ageyrs < 30 ~ 25,
    ageyrs >= 30 & ageyrs < 40 ~ 35,
    ageyrs >= 40 & ageyrs < 50 ~ 45,
    ageyrs >= 50 & ageyrs < 60 ~ 55,
    ageyrs >= 60 & ageyrs < 70 ~ 65,
    ageyrs >= 70 & ageyrs < 80 ~ 75,
    ageyrs >= 80 & ageyrs < 90 ~ 85,
    ageyrs >= 90 & ageyrs < 100 ~ 95,
    ageyrs >= 100 ~ 105,
    TRUE ~ NA  
  ))

# frequencies of deaths in age groups
table(SCOT_ISD$agegrp1[SCOT_ISD$death == 1])
table(SCOT_ISD$agegrp1[SCOT_ISD$death == 0])
table(SCOT_ISD$agegrp1)

# a new variable equal to the mean time to death within each age group
SCOT_ISD <- SCOT_ISD %>%
  group_by(agegrp1) %>%
  mutate(mtimetodeaths = mean(timetodeath, na.rm = TRUE)) %>%
  ungroup()
#mean time to death for each age group
SCOT_ISD %>%
  select(agegrp1, mtimetodeaths) %>%
  distinct() %>%
  arrange(agegrp1)

#plot Time to Death vs Age Group
plot_data <- SCOT_ISD %>%
  select(agegrp1, mtimetodeaths) %>%
  distinct()

ggplot(plot_data, aes(x = agegrp1, y = mtimetodeaths)) +
  geom_point() +  # Add points to the plot
  labs(title = "Time to Death vs Age Group", x = "Age Group", y = "Mean Time to Death (days)") +
  theme_minimal()

#plot LOESS
ggplot(SCOT_ISD, aes(x = ageyrs, y = timetodeath)) +
  geom_point(size = 1, alpha = 0.6) +  # Adjust point size here; alpha for transparency
  geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue") +  # LOWESS line
  labs(title = "LOWESS: Time to Death vs Age", x = "Age (Years)", y = "Time to Death (Days)") +
  theme_minimal()

# variable age_50
SCOT_ISD <- SCOT_ISD %>%
  mutate(age_50 = if_else(ageyrs > 50, 1, 0))

SCOT_ISD %>%
  tabyl(age_50) # 90.7% patients are 50+

# groups for LoS
library(dplyr)

SCOT_ISD <- SCOT_ISD %>%
  mutate(losgrp1 = case_when(
    los == 0.5 ~ 0,
    los == 1 ~ 1,
    los > 1 & los <= 10 ~ 5,
    los > 10 & los <= 20 ~ 15,
    los > 20 & los <= 30 ~ 25,
    los > 30 & los <= 40 ~ 35,
    los > 40 & los <= 50 ~ 45,
    los > 50 & los <= 60 ~ 55,
    los > 60 & los <= 70 ~ 65,
    los > 70 & los <= 80 ~ 75,
    los > 80 & los <= 90 ~ 85,
    los > 90 & los <= 100 ~ 95,
    los > 100 ~ 105,
    TRUE ~ NA
  ))

summary(SCOT_ISD$los)
table(SCOT_ISD$losgrp1)

# Mean time to death within each LoS group 
SCOT_ISD <- SCOT_ISD %>%
  group_by(losgrp1) %>%
  mutate(mtimetodeaths_2 = mean(timetodeath, na.rm = TRUE)) %>%
  ungroup()
# Creating a table
mtimetodeaths_table <- SCOT_ISD %>%
  select(losgrp1, mtimetodeaths_2) %>%
  distinct() %>%
  arrange(losgrp1)

plot_data2 <- SCOT_ISD %>%
  select(losgrp1, mtimetodeaths_2) %>%
  distinct()

#plot Time to Death vs LOS Group
ggplot(plot_data2, aes(x = losgrp1, y = mtimetodeaths_2)) +
  geom_point() +  
  labs(title = "Time to Death vs LOS Group", x = "LOS Group", y = "Mean Time to Death") +
  theme_minimal() 

# Mean survival time for patients above 20 days of hospital stay is much lower as for the previous groups. 
# Therefore, 20 days of hospital stay will be used for the creation of a dummy variable.

# variable los_20
SCOT_ISD <- SCOT_ISD %>%
  mutate(los_20 = if_else(los >= 20, 1, 0))

SCOT_ISD %>%
  count(los_20) %>%
  mutate(percentage = n / sum(n) * 100) #13.5% patients LoS >=20 days
