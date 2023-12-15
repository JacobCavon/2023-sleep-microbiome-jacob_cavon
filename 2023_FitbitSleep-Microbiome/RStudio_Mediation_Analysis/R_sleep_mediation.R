install.packages('mediation')
install.packages("dplyr")
library(mediation)


#adlercreutzia, 1,7-dimethylurate, sleep_efficiency mediation analysis
direct <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia, data=adlercreutzia_dimethylurate_sleep_efficiency)

modelM <- lm(dimethylurate ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia, data=adlercreutzia_dimethylurate_sleep_efficiency)

modelY <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia + dimethylurate, data=adlercreutzia_dimethylurate_sleep_efficiency)

results <- mediate(modelM, modelY, treat="adlercreutzia", mediator='dimethylurate', bootstrap=T, sims=500)

summary(results)

summary(direct)


#reverse adlercreutzia, 1,7-dimethylurate, sleep_efficiency mediation analysis
direct <- lm(adlercreutzia ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=adlercreutzia_dimethylurate_sleep_efficiency)

modelM <- lm(dimethylurate ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=adlercreutzia_dimethylurate_sleep_efficiency)

modelY <- lm(adlercreutzia ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency + dimethylurate, data=adlercreutzia_dimethylurate_sleep_efficiency)

results <- mediate(modelM, modelY, treat="sleep_efficiency", mediator='dimethylurate', bootstrap=T, sims=500)

summary(results)

summary(direct)


#adlercreutzia, 2-piperidinone, sleep_efficiency mediation analysis
direct <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia, data=adlercreutzia_piperidinone2_sleep_efficiency)

modelM <- lm(piperidinone2 ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia, data=adlercreutzia_piperidinone2_sleep_efficiency)

modelY <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + adlercreutzia + piperidinone2, data=adlercreutzia_piperidinone2_sleep_efficiency)

results <- mediate(modelM, modelY, treat="adlercreutzia", mediator='piperidinone2', bootstrap=T, sims=500)

summary(results)

summary(direct)

#reverse adlercreutzia, 2-piperidinone, sleep_efficiency mediation analysis
direct <- lm(adlercreutzia ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=adlercreutzia_piperidinone2_sleep_efficiency)

modelM <- lm(piperidinone2 ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=adlercreutzia_piperidinone2_sleep_efficiency)

modelY <- lm(adlercreutzia ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency + piperidinone2, data=adlercreutzia_piperidinone2_sleep_efficiency)

results <- mediate(modelM, modelY, treat="sleep_efficiency", mediator='piperidinone2', bootstrap=T, sims=500)

summary(results)

summary(direct)


# erys, cortisol, sleep_efficiency mediation analysis
direct <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys, data=erys_cortisol_sleep_efficiency)

modelM <- lm(cortisol ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys, data=erys_cortisol_sleep_efficiency)

modelY <- lm(sleep_efficiency ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys + cortisol, data=erys_cortisol_sleep_efficiency)

results <- mediate(modelM, modelY, treat="erys", mediator='cortisol', bootstrap=T, sims=500)

summary(results)

summary(direct)


# reverse erys, cortisol, sleep_efficiency mediation analysis
direct <- lm(erys ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=erys_cortisol_sleep_efficiency)

modelM <- lm(cortisol ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency, data=erys_cortisol_sleep_efficiency)

modelY <- lm(erys ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_efficiency + cortisol, data=erys_cortisol_sleep_efficiency)

results <- mediate(modelM, modelY, treat="sleep_efficiency", mediator='cortisol', bootstrap=T, sims=500)

summary(results)

summary(direct)


# erys, cortisol, sleep_restlessDuration mediation analysis
direct <- lm(sleep_restlessDuration ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys, data=erys_cortisol_sleep_restlessDuration)

modelM <- lm(cortisol ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys, data=erys_cortisol_sleep_restlessDuration)

modelY <- lm(sleep_restlessDuration ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + erys + cortisol, data=erys_cortisol_sleep_restlessDuration)

results <- mediate(modelM, modelY, treat="erys", mediator='cortisol', bootstrap=T, sims=500)

summary(results)

summary(direct)


# reverse erys, cortisol, sleep_restlessDuration mediation analysis
direct <- lm(erys ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_restlessDuration, data=erys_cortisol_sleep_restlessDuration)

modelM <- lm(cortisol ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_restlessDuration, data=erys_cortisol_sleep_restlessDuration)

modelY <- lm(erys ~ activities_activityCalories + activities_calories + 
               activities_distance + activities_elevation + activities_floors + 
               activities_minutesFairlyActive + activities_minutesLightlyActive + 
               activities_minutesSedentary + activities_minutesVeryActive + 
               activities_steps + BMI_CALC + age + sex + sleep_restlessDuration + cortisol, data=erys_cortisol_sleep_restlessDuration)

results <- mediate(modelM, modelY, treat="sleep_restlessDuration", mediator='cortisol', bootstrap=T, sims=500)

summary(results)

summary(direct)
