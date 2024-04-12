#### setup ####
install.packages("haven")
install.packages("janitor")
install.packages(c("sandwich", "lmtest", "stargazer"))
install.packages("survival")
install.packages("survminer")
install.packages("survRM2")
install.packages("flexsurv")

library(haven)
library(janitor)
library(tidyverse)


SCOT_ISD <- read_dta("C:/Users/VictoriaZaitceva/Desktop/personal/HP425/HP425_summative dataset_2022-23.dta")

#================================Descriptive statistics============
# count of deaths
SCOT_ISD %>% group_by(death) %>% summarise(N=n()) #table(SCOT_ISD$death)
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


#===============LINEAR PROBABILITY MODEL/LINEAR REGRESSION=========== 

library(sandwich)
library(lmtest)
library(stargazer)

# regression models

lmodel1 <- lm(timetodeath ~ ageyrs + I(ageyrs^2), data = SCOT_ISD)
lmodel2 <- lm(timetodeath ~ ageyrs + I(ageyrs^2) + female, data = SCOT_ISD)
lmodel3 <- lm(timetodeath ~ ageyrs + I(ageyrs^2) + female + los, data = SCOT_ISD)
lmodel4 <- lm(timetodeath ~ ageyrs + I(ageyrs^2) + female + los + oper, data = SCOT_ISD)
lmodel5 <- lm(timetodeath ~ ageyrs + I(ageyrs^2) + female + los + oper + comorb, data = SCOT_ISD)


# robust standard errors
robust_se1 <- sqrt(diag(vcovHC(lmodel1, type = "HC1")))
robust_se2 <- sqrt(diag(vcovHC(lmodel2, type = "HC1")))
robust_se3 <- sqrt(diag(vcovHC(lmodel3, type = "HC1")))
robust_se4 <- sqrt(diag(vcovHC(lmodel4, type = "HC1")))
robust_se5 <- sqrt(diag(vcovHC(lmodel5, type = "HC1")))

# nested table
stargazer(lmodel1, lmodel2, lmodel3, lmodel4, lmodel5, type = "text",
          se = list(robust_se1, robust_se2, robust_se3, robust_se4, robust_se5),
          covariate.labels = c("Age (Years)", "Age Squared", "Female", "Length of Stay", "Operation", "Comorbidity"),
          omit.stat = "all", model.numbers = FALSE,
          column.labels = c("OLS-I", "OLS-II", "OLS-III", "OLS-IV", "OLS-V"),
          title = "OLS Regression Results")



#================== NON-PARAMETRIC SURVIVAL MODEL==============

library(survival)
library(survminer)
library(survRM2)



summary(SCOT_ISD$timetoevent)
SCOT_ISD$timetoevent[SCOT_ISD$timetoevent == 0] <- 0.5
summary(SCOT_ISD$timetoevent)



#### Basic Kaplan-Meier estimator ####

# The Surv function takes the time-to-event data and the event indicator, where death == 1
surv_object <- Surv(time = SCOT_ISD$timetoevent, event = SCOT_ISD$death)


km_fit <- survfit(surv_object ~ 1, data = SCOT_ISD) 
summary(km_fit)
ggsurvplot(km_fit, data = SCOT_ISD, risk.table = TRUE,
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Function")  # survminer

#### KM hazard function ####

# Plotting the cumulative hazard function
ggsurvplot(km_fit, data = SCOT_ISD, fun = 'cumhaz', risk.table = TRUE,
           ggtheme = theme_minimal(),
           title = "Cumulative Hazard Function Based on KM Estimates")

#the Kaplan-Meier method provides survival probabilities over time, not hazard rates. The hazard function, which indicates the instantaneous risk of failure at any given time point, can be challenging to estimate non-parametrically without making assumptions about the underlying hazard rate



#### Restricted mean survival time (which accounts for censoring) #### 
# The RMST is defined as a measure of average survival from time 0 to a specific time point, and can be estimated by taking the area under the survival curve up to that point

fit <- survfit(Surv(timetoevent, death) ~ 1, data = SCOT_ISD)

print(fit, print.rmean=TRUE) #By default, this assumes that the longest survival time is equal to the longest survival time in the data. You can set this to a different value by adding an rmean argument (e.g., print(km, print.rmean=TRUE, rmean=250)
#The restricted mean is a more reliable measure, but might underestimate the mean survival time due to censoring
rmean <- 3296


#### Extended mean  survival time ####
# forecasts the graph until all people die, extends the observed survival by projecting 
# the survival curve to zero

#1 way
# Fit an exponential model to your data
exp_model <- survreg(Surv(timetoevent, death) ~ 1, data = SCOT_ISD, dist = "exponential")
# The rate parameter (lambda) for the exponential distribution can be calculated as 1/exp(coef)
lambda <- 1 / exp(coef(exp_model))
# The mean survival time is the reciprocal of the rate parameter
emean <- 1 / lambda


#2 way - same result
library(flexsurv)
exp_fit <- flexsurvreg(Surv(timetoevent, death) ~ 1, data = SCOT_ISD, dist = "exp")
# Extract the rate parameter estimate
rate_estimate <- exp_fit$res["rate", "est"]
# Calculate the mean survival time as the reciprocal of the rate parameter
emean <- 1 / rate_estimate

emean > rmean
# We do see a higher mean of survival time for the extended mean command. This makes sense as 
# extended mean is always higher than the restricted mean as it extends the observed survival by projecting 
# the survival curve to zero. The extended mean accounts for the censoring of observations with large 
# survival times. This approach is limited at it assumes an exponential functional form, which might not fit 
# the data.




#===========Subgroup analyses of mean survival time and survival curve for covariates===========

#### comorbidity #### 
# Fit Kaplan-Meier curves by comorbidity groups
km_fit_comorb <- survfit(surv_object ~ comorb, data = SCOT_ISD)

#restricted mean
print(km_fit_comorb, print.rmean=TRUE) 

#extended mean - too complicated

# Plot the survival curves by comorbidity groups
ggsurvplot(km_fit_comorb, data = SCOT_ISD, risk.table = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves by Comorbidity Groups")


# Perform the log-rank test comparing survival between comorbidity groups
logrank_test1 <- survdiff(surv_object ~ SCOT_ISD$comorb)

# Print the test results
print(logrank_test1) #Chisq= 818, p= <2e-16 
#statistically significant difference in the survivor functions



#### operation #### 

km_fit_oper <- survfit(surv_object ~ oper, data = SCOT_ISD)

#restricted mean
print(km_fit_oper, print.rmean=TRUE)

# the survival curves by oper groups
ggsurvplot(km_fit_oper, data = SCOT_ISD, risk.table = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves by Operation Groups")


# Perform the log-rank test comparing survival between comorbidity groups
logrank_test2 <- survdiff(surv_object ~ SCOT_ISD$oper)

# Print the test results
print(logrank_test2) #Chisq= 30.4 , p= 3e-08

#### sex #### 

km_fit_sex <- survfit(surv_object ~ female, data = SCOT_ISD)

#restricted mean
print(km_fit_sex, print.rmean=TRUE)

# the survival curves by oper groups
ggsurvplot(km_fit_sex, data = SCOT_ISD, risk.table = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves by Gender")


# Perform the log-rank test comparing survival between comorbidity groups
logrank_test3 <- survdiff(surv_object ~ SCOT_ISD$female)

# Print the test results
print(logrank_test3) #Chisq= 92.4 , p= <2e-16 


#### age_50 #### 

km_fit_age50 <- survfit(surv_object ~ age_50, data = SCOT_ISD)

#restricted mean
print(km_fit_age50, print.rmean=TRUE)

# the survival curves by oper groups
ggsurvplot(km_fit_age50, data = SCOT_ISD, risk.table = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves by Age 50 Group")


# Perform the log-rank test comparing survival between comorbidity groups
logrank_test4 <- survdiff(surv_object ~ SCOT_ISD$age_50)

# Print the test results
print(logrank_test4) # Chisq= 825  on 1 degrees of freedom, p= <2e-16 


#### los_20 #### 

km_fit_los20 <- survfit(surv_object ~ los_20, data = SCOT_ISD)

#restricted mean
print(km_fit_los20, print.rmean=TRUE)

# the survival curves by oper groups
ggsurvplot(km_fit_los20, data = SCOT_ISD, risk.table = TRUE, 
           ggtheme = theme_minimal(), 
           title = "Kaplan-Meier Survival Curves by Age 50 Group")


# Perform the log-rank test comparing survival between comorbidity groups
logrank_test5 <- survdiff(surv_object ~ SCOT_ISD$los_20)

# Print the test results
print(logrank_test5) # Chisq= 587 , p= <2e-16 


#===============PARAMETRIC SURVIVAL MODEL==============

#===============Exponential model==============

#### The mean survival time estimated by the exponential model without adding controls ####

# by hand 

1/0.0002708 # 3692.762 days

# taken by the mean of predicted mean survival 

# Fit an exponential model to the data without additional covariates
exp_model_uncon <- survreg(Surv(timetoevent, death) ~ 1, data = SCOT_ISD, dist = "exponential")

# The rate parameter (lambda) from the model; coef() extracts the model coefficients
lambda_uncon <- 1 / exp(coef(exp_model_uncon))

# Calculate the mean survival time as the reciprocal of the rate parameter
mean_surv_time_uncon <- 1 / lambda_uncon

mean_surv_time_uncon # 3693.0843 days


#### Adding control variables #### 

# Fit the exponential model with control variables
exp_model_controls <- survreg(Surv(timetoevent, death) ~ ageyrs + los + female + comorb + oper, 
                              data = SCOT_ISD, dist = "exponential")

# model summary to see the coefficients
summary(exp_model_controls) #positive coefficient =  lower duration
#an increased value  is associated with a decreased expected survival


#R: A positive coefficient implies longer survival times (lower hazard) because it increases the log survival time.
#Stata: A positive coefficient implies higher hazard (shorter survival times) because it directly increases the hazard rate.


# Predict the survival time for each observation
predicted_survival_times <- predict(exp_model_controls, type = "response")

# The mean of the predicted survival times
mean_predicted_survival_time <- mean(predicted_survival_times)

mean_predicted_survival_time # 4466.623 days

#### Quality ####
# evaluate the goodness of fit of exponential survival model using the Akaike Information Criterion (AIC)

# Retrieve the AIC score for the fitted model
aic_score <- AIC(exp_model_controls)
aic_score # 193880.7

# exponential survival function ??

# hazard looks ok

# Since the hazard is constant, create a data frame with the same hazard value across a range of time points
hazard_data <- data.frame(Time = seq(from = 0, to = max(SCOT_ISD_nozero$timetoevent), by = 1),
                          Hazard = rep(lambda, length.out = length(seq(from = 0, to = max(SCOT_ISD_nozero$timetoevent), by = 1))))

# Plot the constant hazard function
ggplot(hazard_data, aes(x = Time, y = Hazard)) +
  geom_line(color = "red") +
  labs(title = "Exponential Model Hazard Function", x = "Time", y = "Hazard Rate") +
  theme_minimal()






#======================Weibull model==================

weibull_model <- survreg(Surv(timetoevent, death) ~ ageyrs + los + female + comorb + oper, 
                         data = SCOT_ISD_nozero, dist = "weibull")

summary(weibull_model)
model_summary <- summary(weibull_model)
format(exp(coef(model_summary)), scientific = FALSE) #hazard datios


weibul_surv <- survfit(weibull_model)
 

weibull_shape_param <- 1 / weibull_model$scale  # Shape parameter (alpha)
weibull_shape_param # α<1: The hazard function is decreasing over time.




AIC(weibull_model) #192652.1


# Predict mean survival time
SCOT_ISD$survmean_wei <- predict(weibull_model, type = "response")
summary(SCOT_ISD$survmean_wei) #5013.3  days




#=====================COX========================

cox_model <- coxph(Surv(timetoevent, death) ~ ageyrs + los + female + comorb + oper, data = SCOT_ISD)
summary(cox_model)

library(survminer)

# Generate survival curve from Cox model
cox_surv <- survfit(cox_model)

# Plot survival curve
ggsurvplot(cox_surv, data = SCOT_ISD, conf.int = TRUE,
           ggtheme = theme_minimal(), title = "Cox Proportional Hazards Model Survival Function")



#### assessing the proportional hazards assumption ####

# Test using Schoenfeld residuals (Grambsch and Therneau hypothesis test)

ph_test <- cox.zph(cox_model)

ph_test #statistically significant evidence (p<0.05) against the null hypothesis of proportionality for all covariates
#  Significant p-values (commonly < 0.05 or < 0.01) suggest evidence against the proportional hazards assumption for the corresponding covariate or the model as a whole.

# Schoenfeld residuals for each covariate
plot(ph_test)
# clear trend - violation 
# The PH assumption is violated.


#Log-Log plot of survival for each binary covariate (Graphical examination)


# gender
loglog_female <- survfit(Surv(timetoevent, death) ~ female, data = SCOT_ISD)

ggsurvplot(loglog_female, data = SCOT_ISD, fun = "cloglog",
           xlab = "Time (in days) using log",
           ylab = "Log-log Survival",
           title = "Log-log Curves by Gender",
           ggtheme = theme_minimal())
# The PH assumption seems tp hold.

# operation
loglog_oper <- survfit(Surv(timetoevent, death) ~ oper, data = SCOT_ISD)

ggsurvplot(loglog_oper, data = SCOT_ISD, fun = "cloglog",
           xlab = "Time (in days) using log",
           ylab = "Log-log Survival",
           title = "Log-log Curves by Operation",
           ggtheme = theme_minimal())
## The PH assumption seems slightly violated.

# comorb

loglog_comorb <- survfit(Surv(timetoevent, death) ~ comorb, data = SCOT_ISD)

ggsurvplot(loglog_comorb, data = SCOT_ISD, fun = "cloglog",
           xlab = "Time (in days) using log",
           ylab = "Log-log Survival",
           title = "Log-log Curves by Comorbidity",
           ggtheme = theme_minimal())
# The PH assumption is violated.




