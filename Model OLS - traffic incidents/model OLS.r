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

library("lmtest")
library("foreign")

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
model4=lm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
            driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model4)

resettest(model4, power=2:3, type="fitted")

#MODEL 5 - USUWAM walking
model5=lm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
            driving + fuel + pandemic + weekend_free, data = df)
summary(model5)

resettest(model5, power=2:3, type="fitted")

#MODEL 6 - USUWAM fuel
model6=lm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
            driving + pandemic + weekend_free, data = df)
summary(model6)

resettest(model6, power=2:3, type="fitted")

library("car")
linearHypothesis(model=model1, c("temperature", "quarantine", "rainfall", "walking", "fuel" ))
# p-value = 0.991 >0.05 - zmienne są łącznie nieistotne statystycznie

library(strucchange)
sctest(model6, type = "Chow", point = 10)

library(stargazer)
stargazer(model1, model6, type="text", align= TRUE, style="default", df=TRUE, column.labels = c("wersja początkowa", "wersja finalna"))

############ TEST - HOMOSKEDASTYCZNOŚĆ RESZT #################
bptest(model6, studentize=TRUE)
#p-value = 0.0009 < 0.05; nie OK

# stosowalna uogólniona metoda kwadratów
# generalized least squares

library("car")
model_glm=glm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
                driving + pandemic + weekend_free, data = df)
summary(model_glm)

# ważona metoda najmniejszych kwadratów 
# Weighted least sqaure (WLS)

model6.weights <- 1 / lm(abs(model6$residuals) ~ model6$fitted.values)$fitted.values^2
model6.lmw <- lm(traffic_incidents ~ new_cases + new_deaths + humidity + stringency_index+
                    driving + pandemic + weekend_free,
                  data = df,
                  weights = model6.weights)

summary.lm(model6.lmw)$coefficients
summary(model6.lmw)

stargazer(model1, model6, model_glm, model6.lmw, type="text", align= TRUE, style="default", df=TRUE, column.labels = c("MNK_first", "MNK_final", "SUMNK", "WMNK"))


bptest(model6.lmw, studentize=TRUE)

################ normalność rozkładu reszt
library("sandwich")
library("knitr")
library("tseries")

jarque.bera.test(model6.lmw$residuals)
#p-value = 0.4487 > 0.05; OK

library(rcompanion)
par(mfrow=c(1,2))

d<-density(model6.lmw$residuals)
plot(d,main='Residual KDE Plot',xlab='Residual value')

plot(ecdf(model6.lmw$residuals), main='Residual Empirical CDF')

# TEST SHAPIRO-WILKA

shapiro.test(model6.lmw$residuals)


plot(model6.lmw, 1) #residuals vs fitted
plot(model6.lmw, 2) #normal q-q
plot(model6.lmw, 3) #scale-location
plot(model6.lmw, 4) #cook's distance
plot(model6.lmw, 5) #residuals vs leverage
plot(model6.lmw, 6) #cook's dist vs leverage

########################################################################
########################### DURBIN-WATSON TEST ###########################

dwtest(model6)
#p-value = 0.000007595 < 0.05
#autocorrelation

#breuscha godfreya
bgtest(model6.lmw)
#p-value = 0.00003686 < 0.05 -> autocorrelation

# ESTYMATOR ODPORNY NA AUTOKORELACJĘ - ESTYMATOR NEWEY'A WESTA
install.packages("sandwich")
library("lmtest")
library("sandwich")

NeweyWest(model6.lmw, lag=5, prewhite=FALSE)
autocorr = coeftest(model6.lmw, vcov=NeweyWest(model6.lmw, lag=5, prewhite=FALSE))
show(autocorr)
summary(autocorr)

stargazer(model1, model6, model_glm, model6.lmw, autocorr, type="text", align= TRUE, style="default", df=TRUE, column.labels = c("MNK_first", "MNK_final", "SUMNK", "WMNK", "estymator Neweya-Westa"))


#WSPÓŁLINIOWOŚĆ
# jesli VIF > 10, to zmienna objasniajaca jest współliniowa
vif(model6.lmw)


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

#MODEL 3 new cases
model_d_3=lm(dead_injured_incidents ~ quarantine + rainfall + temperature + humidity + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_d_3)

resettest(model_d_3, power=2:3, type="fitted")

#MODEL 4 humidity
model_d_4=lm(dead_injured_incidents ~ quarantine + rainfall + temperature + stringency_index+
               driving + walking + fuel + pandemic + weekend_free, data = df)
summary(model_d_4)

resettest(model_d_4, power=2:3, type="fitted")


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



