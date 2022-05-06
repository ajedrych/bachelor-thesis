library("readxl")
library("sandwich")
library("knitr")
library("tidyverse")

options(scipen = 999)

#LOAD DATA
df <- read_excel("model_2020.xlsx") 
View(df)

library(Hmisc)
hist.data.frame(df)

################################### ALL TRAFFIC INCIDENTS ###############################
#MODEL 1
model1=lm(traffic_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model1)

resettest(model1, power=2:3, type="fitted")

#MODEL 2 - USUWAM TEMPERATURE
model2=lm(traffic_incidents ~ quarantine + new_cases + new_deaths + rainfall + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model2)

resettest(model2, power=2:3, type="fitted")

#MODEL 3 - USUWAM QUARANTINE
model3=lm(traffic_incidents ~ new_cases + new_deaths + rainfall + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model3)

resettest(model3, power=2:3, type="fitted")

#MODEL 4 - USUWAM RAINFALL
model4=lm(traffic_incidents ~ quarantine + new_cases + new_deaths + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model4)

resettest(model4, power=2:3, type="fitted")

#MODEL 5 - USUWAM quarantine
model5=lm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model5)

resettest(model5, power=2:3, type="fitted")



################################### DEAD AND INJURED INCIDENTS ###############################
#MODEL 1
model_d_1=lm(dead_injured_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_d_1)

resettest(model_d_1, power=2:3, type="fitted")

#MODEL 2 NEW DEATHS
model_d_2=lm(dead_injured_incidents ~ quarantine + new_cases +rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_d_2)

resettest(model_d_2, power=2:3, type="fitted")

#MODEL 2 
model_d_2=lm(dead_injured_incidents ~ quarantine + new_cases +rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_d_2)

resettest(model_d_2, power=2:3, type="fitted")

################################### BIKE AND PEDESTRIANS INCIDENTS ###############################
#MODEL 1
model_b_1=lm(bike_pedestrian_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_b_1)

resettest(model_b_1, power=2:3, type="fitted")

#MODEL 2 humidity
model_b_2=lm(bike_pedestrian_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_b_2)

resettest(model_b_2, power=2:3, type="fitted")

#MODEL 3 temperature
model_b_3=lm(bike_pedestrian_incidents ~ quarantine + new_cases + new_deaths+rainfall + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_b_3)

resettest(model_b_3, power=2:3, type="fitted")

#MODEL 4 quarantine
model_b_4=lm(bike_pedestrian_incidents ~ new_cases + new_deaths+rainfall + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_b_4)

resettest(model_b_4, power=2:3, type="fitted")

#MODEL 5 
model_b_5(bike_pedestrian_incidents ~ new_cases + new_deaths+rainfall + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_b_5)

resettest(model_b_5, power=2:3, type="fitted")

################################### FOREIGNERS INCIDENTS ###############################
#MODEL 1
model_f_1=lm(foreigners_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_f_1)

resettest(model_f_1, power=2:3, type="fitted")

################################### ALCOHOL INCIDENTS ###############################
#MODEL 1
model_a_1=lm(alcohol_incidents ~ quarantine + new_cases + new_deaths+rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_a_1)

resettest(model_a_1, power=2:3, type="fitted")


library("lmtest")
library("foreign")

resettest(model2, power=2, type="fitted")
#p-value < 0.05

resettest(model1, power=2, type="regressor")
#p-value < 0.05, but the fitted version is OK

model2=lm(traffic_incidents~quarantine+new_cases+new_deaths+rainfall+temperature+stringency_index+driving+walking+fuel, data=df)
summary(model2)

model3=lm(traffic_incidents~quarantine+new_cases+new_deaths+temperature+stringency_index+driving+walking+fuel, data=df)
summary(model3)



