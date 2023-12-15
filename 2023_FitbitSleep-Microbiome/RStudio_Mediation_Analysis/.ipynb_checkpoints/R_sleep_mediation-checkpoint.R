install.packages('mediation')
install.packages("dplyr")
library(mediation)

colnames(sleep_df)[2] = 'adlercreutzia'

summary(lm(sleep_efficiency~adlercreutzia, data=sleep_df))

sleep_df <- sleep_df %>% relocate(adlercreutzia, .after = sex)

frmla <- as.formula(paste(colnames(sleep_df)[1], paste(colnames(sleep_df)[3:17], sep="", collapse=" + "), sep=" ~ "))

frmla

summary(lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
             activities_distance + activities_elevation + activities_floors + 
             activities_minutesFairlyActive + activities_minutesLightlyActive + 
             activities_minutesSedentary + activities_minutesVeryActive + 
             activities_steps + BMI_CALC + age + sex + adlercreutzia, data=sleep_df))

modelM <- lm(dimethylurate ~ activities_activityCalories + activities_calories + 
             activities_distance + activities_elevation + activities_floors + 
             activities_minutesFairlyActive + activities_minutesLightlyActive + 
             activities_minutesSedentary + activities_minutesVeryActive + 
             activities_steps + BMI_CALC + age + sex + adlercreutzia, data=sleep_df)

modelY <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia + dimethylurate, data=sleep_df)

results <- mediate(modelM, modelY, treat="adlercreutzia", mediator='dimethylurate', bootstrap=T, sims=500)