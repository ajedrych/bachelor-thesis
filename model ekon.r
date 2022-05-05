library("readxl")
library("sandwich")
library("knitr")
library("tidyverse")

options(scipen = 999)

#LOAD DATA
df <- read_excel("model.xlsx") 
View(df)

model1=lm(traffic_incidents~quarantine+new_cases+new_deaths+rainfall+temperature+humidity+stringency_index+driving+walking+fuel, data=df)
summary(model1)


library("lmtest")
library("foreign")

resettest(model3, power=2, type="fitted")
#p-value < 0.05

resettest(model1, power=2, type="regressor")
#p-value < 0.05, but the fitted version is OK

model2=lm(traffic_incidents~quarantine+new_cases+new_deaths+rainfall+temperature+stringency_index+driving+walking+fuel, data=df)
summary(model2)

model3=lm(traffic_incidents~quarantine+new_cases+new_deaths+temperature+stringency_index+driving+walking+fuel, data=df)
summary(model3)



# TEST CHOWA
library(strucchange)
sctest(model1, type = "Chow", point = 10)
#p-value < 0.05


# WARTOŚĆI BRAKUJĄCE
sapply(df, function(x) sum(is.na(x)))

#DELETE NA
df1<-na.omit(df)
str(df1) #TYPES OF VARIABLES

summary(df)

pairs(df1[,0:10]) 

# CORRELATION PLOT
library(corrplot)
kor<-cor(df)
corrplot(kor, method="circle")

# STATISTICS TABLE

library("stargazer")
stargazer(as.data.frame(df), type = "text")



###################################################################################
########################### DEPENDENT VARIABLE CHECKING ###########################
# DEPENDENT VARIABLE - wypadki

library(rcompanion)
par(mfrow=c(1,2))

plotNormalHistogram(df$traffic_incidents, prob = FALSE,
                    main = "wypadki and normal distribution",
                    linecol = "red",
                    length = 1000)

df$ln_traffic_incidents = log(df$traffic_incidents)

plotNormalHistogram(df$ln_traffic_incidents, prob = FALSE,
                    main = "ln_wypadki and normal distribution",
                    linecol = "red",
                    length = 1000)

library("tseries")
jarque.bera.test(df$f traffic_incidents) #p-value < 0.05, I reject H0 about normal distribution of wypadki

summary(df1$wypadki)

install.packages("ggthemes")
library("ggthemes")

par(mfrow=c(1,2))

boxplot_wypadki <- ggplot( data.frame(df1$wypadki), aes(y=df1$wypadki)) +
  geom_boxplot() +
  ggtitle("Boxplot zmiennej wypadki") +
  xlab("")
boxplot_wypadki


###################################################################
########################### FIRST MODEL ###########################
#MODEL 1
model1=lm(wypadki~kwarantanna + zachorowania + zgony + opady + temp + wilgotnosc + stringency_index + 
            driving_apple + walking_apple + retail_and_recreation + grocery_and_pharmacy + 
            parks + transit_stations + workplaces + residential, data=df1)
summary(model1)

#R^2 = 0.6919
#adj. r^2 = 0.6848
# łącznie istotne

library("stargazer")
stargazer(model1, type="text", align= TRUE, style="default", df=FALSE)

##################################################################
########################### RESET TEST ###########################

library("lmtest")
library("foreign")

resettest(model1, power=2:3, type="fitted")
#p-value < 0.05

resettest(model1, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK


# TEST CHOWA
library(strucchange)
sctest(model1, type = "Chow", point = 10)
#p-value < 0.05

########################### VARIABLES CHECKING ###########################
# VARIABLE DRIVING_APPLE
g_driving_apple<-ggplot(df1, aes(x=wypadki, y=driving_apple)) +geom_point(color="red")
g_driving_apple

par(mfrow=c(1,2))
plotNormalHistogram(df1$driving_apple, prob = FALSE,
                    main = "driving_apple and normal distribution",
                    linecol = "red",
                    length = 1000) 

library("psych")
describe(df1$driving_apple)

library("tseries")
jarque.bera.test(df1$driving_apple) #p-value < 0.05, I reject H0 about normal distribution

df1$ln_driving_apple = log(df1$driving_apple)

plotNormalHistogram(df1$ln_driving_apple, prob = FALSE,
                    main = "ln_driving_apple and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$ln_driving_apple)

##################################################################
########################### NEXT MODEL ###########################
#MODEL 2 - delete driving_apple
model2=lm(wypadki~kwarantanna + zachorowania + zgony + opady + temp + wilgotnosc + stringency_index + 
            walking_apple + retail_and_recreation + grocery_and_pharmacy + 
            parks + transit_stations + workplaces + residential, data=df1)
summary(model2)

#R^2 = 0.5819
#adj. r^2 = 0.573 

stargazer(model2, type="text", align= TRUE, style="default", df=FALSE)

########################### RESET TEST ###########################

resettest(model2, power=2:3, type="fitted")
#p-value < 0.05

resettest(model2, power=2:3, type="regressor")
#p-value < 0.05


# TEST CHOWA
library(strucchange)
sctest(model2, type = "Chow", point = 10)
#p-value < 0.05

########################### VARIABLES CHECKING ###########################
# VARIABLE retail and recreation
g_retail_and_recreation<-ggplot(df1, aes(x=wypadki, y=retail_and_recreation)) +geom_point(color="red")
g_retail_and_recreation

par(mfrow=c(1,2))
plotNormalHistogram(df1$retail_and_recreation, prob = FALSE,
                    main = "retail_and_recreation and normal distribution",
                    linecol = "red",
                    length = 1000) 

describe(df1$retail_and_recreation)

jarque.bera.test(df1$retail_and_recreation) #p-value < 0.05, I reject H0 about normal distribution

##################################################################
########################### NEXT MODEL ###########################
#MODEL 3 - retail_and_recreation 
model3=lm(wypadki~kwarantanna + zachorowania + zgony + opady + temp + wilgotnosc + stringency_index, data=df1)
summary(model3)

#R^2 = 0.5205
#adj. r^2 = 0.511 

resettest(model2, power=2:3, type="fitted")
#p-value < 0.05

resettest(model2, power=2:3, type="regressor")
#p-value < 0.05

##################################################################
########################### GETS ###########################

library("car")
linearHypothesis(model=model3, c("covid19_quarantine=0"))
#p-value = 0.8611, so variable is insignificant

linearHypothesis(model=model3, c("covid19_quarantine=0", "avg_people_per_house=0"))
#p-value = 0.9475, so variables are jointly insignificant

#USUWAM TAKŻE INTERAKCJE NURSERYXCHILDREN PONIEWAŻ JEST WSPÓŁLINIOWA A RÓWNIEŻ NIEISTOTNA

##################################################################
########################### NEXT MODEL ###########################
#MODEL 4 - REMOVE COVID19_QUARANTINE, AVG_PEOPLE_PER_HOUSEAND NURSERYXCHILDREN
model4=lm(ln_wypadki~driving_apple + covid19_deaths + driving_apple + divorce_rate +
            budget_reve_pc + ln_unemployment_rate + women_reproductive + femininity_ratio + ln_avg_salary +
            women_working + men_working + ln_median_house_price + house_ratio + houses_area_pc + houses_area_pc_2 +
            education_expenditure + health_expenditure + social_expenditure +
            family_expenditure + children + nursery_places + doctors+ urbanisation_rate + bus_stops +
            education_expenditurexchildren, data=df1)
summary(model4)

#R^2 = 0.7737
#adj. r^2 = 0.7578 better

linearHypothesis(model=model4, c("women_working=0"))
#p-value = 0.5327, so variable is insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0"))
#p-value = 0.6107, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0"))
#p-value = 0.6266, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0"))
#p-value = 0.617, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0", "budget_reve_pc=0"))
#p-value = 0.5288, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0", "budget_reve_pc=0", "driving_apple=0"))
#p-value = 0.5804, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0", "budget_reve_pc=0", "driving_apple=0",
                                 "education_expenditure=0"))
#p-value = 0.411, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0", "budget_reve_pc=0", "driving_apple=0",
                                 "education_expenditure=0", "femininity_ratio=0"))
#p-value = 0.4783, so variables are jointly insignificant

linearHypothesis(model=model4, c("women_working=0","houses_area_pc_2=0", "urbanisation_rate=0",
                                 "houses_area_pc=0", "budget_reve_pc=0", "driving_apple=0",
                                 "education_expenditure=0", "femininity_ratio=0", "nursery_places=0"))
#p-value = 0.2529, so variables are jointly insignificant
#RESZTA ZMIENNYCH MA P-VALUE MNIEJSZE OD 0.1, DLATEGO CHCE IM SIE PRZYJRZEC PRZED ICH WYRZUCENIEM

##################################################################
########################### NEXT MODEL ###########################
#MODEL 5 - REMOVE VARIABLE JOINTLY INSIGNIFICANT FROM GETS
model5=lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            ln_avg_salary + men_working + ln_median_house_price + house_ratio + health_expenditure +
            social_expenditure + family_expenditure + children + doctors + bus_stops +
            education_expenditurexchildren, data=df1)
summary(model5)

#R^2 = 0.7664
#adj. r^2 = 0.7561

linearHypothesis(model=model5, c("ln_avg_salary=0"))
#p-value = 0.1269, so variable is insignificant

linearHypothesis(model=model5, c("ln_avg_salary=0", "health_expenditure=0"))
#p-value = 0.06125, so variable are jointly insignificant

linearHypothesis(model=model5, c("ln_avg_salary=0", "health_expenditure=0", "house_ratio=0"))
#p-value = 0.04442, so variable are not jointly insignificant

##################################################################
########################### NEXT MODEL ###########################
#MODEL 6 - REMOVE VARIABLE JOINTLY INSIGNIFICANT FROM GETS
model6=lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            men_working + ln_median_house_price + house_ratio +
            social_expenditure + family_expenditure + children + doctors + bus_stops +
            education_expenditurexchildren, data=df1)
summary(model6)

#R^2 = 0.7628
#adj. r^2 = 0.7537

linearHypothesis(model=model6, c("house_ratio=0"))
#p-value = 0.1269, so variable is insignificant

linearHypothesis(model=model6, c("house_ratio=0", "men_working=0"))
#p-value = 0.06696, so variable are jointly insignificant

##################################################################
########################### NEXT MODEL ###########################
#MODEL 6 - REMOVE VARIABLE JOINTLY INSIGNIFICANT FROM GETS
model6=lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            ln_median_house_price + social_expenditure + family_expenditure + children + doctors + bus_stops +
            education_expenditurexchildren, data=df1)
summary(model6)

#R^2 = 0.7593
#adj. r^2 = 0.7514

###################################################################
########################### FINAL MODEL ###########################
#MODEL 7 - REMOVE interaction because of her insignificance
model7=lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            ln_median_house_price + social_expenditure + family_expenditure + children + doctors + bus_stops, data=df1)
summary(model7)

#R^2 = 0.7575
#adj. r^2 = 0.7503

stargazer(model3, model7, type="text", align= TRUE, style="default", df=TRUE, column.labels = c("wersja przejściowa", "wersja finalna"))

##################################################################
########################### RESET TEST ###########################

library("lmtest")
library("foreign")

resettest(model7, power=2:3, type="fitted")
# p-value = 0.3377 and it's more than 0.05, OK

resettest(model7, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK


# TEST CHOWA
library(strucchange)
sctest(model7, type = "Chow", point = 10)

##########################################################################
########################### BREUSCH PAGAN TEST ###########################

bptest(model7, studentize=TRUE)
#p-value = 0.975 > 0.05; OK

par(mfrow=c(1,1))
plot(model7, 1) #residuals vs fitted
plot(model7, 2) #normal q-q
plot(model7, 3) #scale-location
plot(model7, 4) #cook's distance
plot(model7, 5) #residuals vs leverage
plot(model7, 6) #cook's dist vs leverage

par(mfrow=c(2,2))
plot(model7)

par(mfrow=c(1,1))
plotNormalHistogram(model7$residuals, prob = FALSE,
                    main = "residuals and normal distribution",
                    linecol = "red",
                    length = 1000) 

# stosowalna uogólniona metoda kwadratów
# generalized least squares

library("car")
model_glm=glm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            ln_median_house_price + social_expenditure + family_expenditure + children + doctors + bus_stops, data=df1)
summary(model_glm)

# ważona metoda najmniejszych kwadratów 
# Weighted least sqaure (WLS)

model7.weights <- 1 / lm(abs(model7$residuals) ~ model7$fitted.values)$fitted.values^2
model7.lmw <- lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
                   ln_median_house_price + social_expenditure + family_expenditure + children + doctors + bus_stops, 
              data = df1, 
              weights = model7.weights)
summary.lm(model7.lmw)$coefficients
summary(model7.lmw)

stargazer(model7, model_glm, model7.lmw, type="text", align= TRUE, style="default", df=TRUE, column.labels = c("MNK", "SUMNK", "WMNK"))

########################################################################
########################### JARQUE-BERA TEST ###########################

jarque.bera.test(model7$residuals)
#p-value = 0.4487 > 0.05; OK


d<-density(model7$residuals)
plot(d,main='Residual KDE Plot',xlab='Residual value')

plot(ecdf(model7$residuals), main='Residual Empirical CDF')

# TEST SHAPIRO-WILKA

shapiro.test(model7$residuals)
#0,5698

########################################################################
########################### DURBIN-WATSON TEST ###########################

dwtest(model7)
#p-value = 0.000007595 < 0.05
#autocorrelation

#breuscha godfreya
bgtest(model7)
#p-value = 0.00003686 < 0.05 -> autocorrelation

##########################################################################
#WSPÓŁLINIOWOŚĆ
# jesli VIF > 10, to zmienna objasniajaca jest współliniowa
vif(model7)
#children vif = 25,632833
#divorce_rate VIF = 20,312211

#POMIMO WYZSZYCH NIZ 10 WARTOSCI POZOSTAWIAM TE ZMIENNE, PONIEWAZ ICH USUNIECIE SPOWODUJE BRAK ISTOTNOSCI
#STATYSTYCZNEJ INNYCH ZMIENNYCH W MODELU

plot(rstandard(model7))
plot(rstudent(model7))
plot(dffits(model7))
matplot(dfbetas(model7))
lines(sqrt(cooks.distance(model7)))

# SUBSET OF VARIABLES USED IN MODEL 7
colnames(df1)
df1_subset <- df1[,c("ln_wypadki", "covid19_deaths", "driving_apple", "divorce_rate",
                     "ln_unemployment_rate", "women_reproductive", "ln_median_house_price",
                     "social_expenditure", "family_expenditure","children", "doctors", "bus_stops")]
summary(df1_subset)

# CORRELATION PLOT MODEL 7
par(mfrow=c(1,1))

library(corrplot)
kor_subset<-cor(df1_subset)
corrplot(kor_subset, method="circle")

# STATISTICS  MODEL 5
library("stargazer")
stargazer(as.data.frame(df1_subset), type = "text")

#LEVERAGE
# u mnie obserwacja nietypowa to >= 2K/N, czyli 2*17/380 = 0.0089474
library("car")
leveragePlots(model7)
dzwignie<-hatvalues(model7)
which.max(dzwignie)

describe(df1_subset[175,])
describe(df1_subset[224,])

# STANDARYZOWANE RESZTY (OUTLIERS)
# gdy ich wartości bezwględne > 2 to obserwacje nietypowe
rstandard(model7)[abs(rstandard(model7)) > 2] 

#ODLEGLOSC COOKA (OBSERWACJE WPLYWOWE)
par(mfrow=c(1,1))
cutoff <- 4/((nrow(df1)))
plot(model7, which=4, cook.levels=cutoff)
#obserwacje podejrzane 179, 345, 379

#overfitting lub underfitting formy funkcyjnej
avPlots(model5)

# ESTYMATOR ODPORNY NA AUTOKORELACJĘ - ESTYMATOR NEWEY'A WESTA
install.packages("sandwich")
library("lmtest")
library("sandwich")

NeweyWest(model7, lag=5, prewhite=FALSE)
autocorr = coeftest(model7, vcov=NeweyWest(model7, lag=5, prewhite=FALSE))
show(autocorr)

stargazer(model7, autocorr, type="text", df=FALSE, column.labels = c("wersja finalna", "estymator Neweya-Westa"))


#MODEL BEZ OUTLIERS
df_out <- df1[-c(19,179,345),]
model_out=lm(ln_wypadki~ covid19_deaths + driving_apple + divorce_rate + ln_unemployment_rate + women_reproductive +
            ln_median_house_price + social_expenditure + family_expenditure + children + doctors + bus_stops, data=df_out)
summary(model_out)

#R^2 = 0.7562
#adj. r^2 = 0.7489

stargazer(model7, model_out, type="text", align= TRUE, style="default", df=TRUE,
          column.labels = c("wersja finalna", "wersja bez outlierów"))


#####################################################################
# WERYFIKACJA HIPOTEZ ###############################################

#PANDEMIA COVID 19

linearHypothesis(model=model7, c("covid19_deaths=0", "ln_unemployment_rate=0", "women_reproductive=0"))
# p-value < 0.05, odrzucamy lączna hipoteze o nieistotnosci wplywu pandemii covid 19 na wspolczynnik urodzen

# wydatki na dzieci
linearHypothesis(model=model7, c("social_expenditure=0", "family_expenditure=0"))
#p-value < 0.05, odrzucam hipoteze o lacznej nieistotnosci

#cena mieszkan
linearHypothesis(model=model7, c("ln_median_house_price=0"))
#p-value < 0.05, odrzucam hipoteze o nieistotnosci

#######################################################################
########################### VARIABLES STATS ###########################



########################### VARIABLES CHECKING ###########################
#VARIABLE COVID19_DEATHS

g_covid19_deaths<-ggplot(df1, aes(x=wypadki, y=covid19_deaths)) +geom_point(color="red")
g_covid19_deaths


plotNormalHistogram(df1$covid19_deaths, prob = FALSE,
                    main = "covid19_deaths and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$covid19_deaths)

describe(df1$covid19_deaths)

########################### VARIABLES CHECKING ###########################
#VARIABLE driving_apple

plotNormalHistogram(df1$driving_apple, prob = FALSE,
                    main = "driving_apple and normal distribution",
                    linecol = "red",
                    length = 1000) 

describe(df1$driving_apple)

jarque.bera.test(df1$driving_apple) #p-value < 0.05, I reject H0 about normal distribution

########################### VARIABLES CHECKING ###########################
#VARIABLE DIVORCE_RATE

plotNormalHistogram(df1$divorce_rate, prob = FALSE,
                    main = "divorce_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

describe(df1$divorce_rate)

jarque.bera.test(df1$divorce_rate) #p-value < 0.05, I reject H0 about normal distribution

########################### VARIABLES CHECKING ###########################
#VARIABLE UNEMPLOYMENT_RATE

plotNormalHistogram(df1$unemployment_rate, prob = FALSE,
                    main = "unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_unemployment_rate = log(df1$unemployment_rate)

plotNormalHistogram(df1$ln_unemployment_rate, prob = FALSE,
                    main = "ln_unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$unemployment_rate) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_unemployment_rate) #p-value <0.05, but higher than in previous case

describe(df1$unemployment_rate)

########################### VARIABLES CHECKING ###########################
#VARIABLE WOMEN_REPRODUCTIVE

plotNormalHistogram(df1$women_reproductive, prob = FALSE,
                    main = "women_reproductive and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$women_reproductive) #p-value > 0.05

describe(df1$women_reproductive)

########################### VARIABLES CHECKING ###########################
#VARIABLE AVG_SALARY

g_avg_salary<-ggplot(df1, aes(x=wypadki, y=avg_salary)) +geom_point(color="red")
g_avg_salary

par(mfrow=c(1,2))
plotNormalHistogram(df1$avg_salary, prob = FALSE,
                    main = "avg_salary and normal distribution",
                    linecol = "red",
                    length = 1000) 
library(psych)
describe(df1$avg_salary)
summary(df1$avg_salary)

df1$ln_avg_salary = log(df1$avg_salary)

plotNormalHistogram(df1$ln_avg_salary, prob = FALSE,
                    main = "ln_avg_salary and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_avg_salary<-ggplot(df1, aes(x=wypadki, y=ln_avg_salary)) +geom_point(color="red")
g_ln_avg_salary

jarque.bera.test(df1$avg_salary) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_avg_salary) #p-value <0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE MEN_WORKING
plotNormalHistogram(df1$men_working, prob = FALSE,
                    main = "men_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$men_working) #p-value < 0.05, I reject H0 about normal distribution

########################### VARIABLES CHECKING ###########################
#VARIABLE MEDIAN_HOUSE_PRICE

jarque.bera.test(df1$median_house_price) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_median_house_price = log(df1$median_house_price)
jarque.bera.test(df1$ln_median_house_price) #p-value < 0.05, but higher than in previous case

plotNormalHistogram(df1$median_house_price, prob = FALSE,
                    main = "median_house_price and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_median_house_price, prob = FALSE,
                    main = "ln_median_house_price and normal distribution",
                    linecol = "red",
                    length = 1000)

describe(df1$median_house_price)

########################### VARIABLES CHECKING ###########################
#VARIABLE HOUSE_RATIO

jarque.bera.test(df1$house_ratio) #p-value < 0.05, I reject H0 about normal distribution

par(mfrow=c(1,1))
plotNormalHistogram(df1$house_ratio, prob = FALSE,
                    main = "house_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

library("psych")
describe(df1$house_ratio)

########################### VARIABLES CHECKING ###########################
#VARIABLE WOMEN_WORKING
plotNormalHistogram(df1$women_working, prob = FALSE,
                    main = "women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_women_working = log(df1$women_working)

plotNormalHistogram(df1$ln_women_working, prob = FALSE,
                    main = "ln_women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_women_working<-ggplot(df1, aes(x=wypadki, y=ln_women_working)) +geom_point(color="red")
g_ln_women_working

jarque.bera.test(df1$women_working) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_women_working) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE COVID19_QUARANTINE

plotNormalHistogram(df1$covid19_quarantine, prob = FALSE,
                    main = "covid19_quarantine and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_covid19_quarantine = log(df1$covid19_quarantine)

plotNormalHistogram(df1$ln_covid19_quarantine, prob = FALSE,
                    main = "ln_covid19_quarantine and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_covid19_quarantine<-ggplot(df1, aes(x=wypadki, y=ln_covid19_quarantine)) +geom_point(color="red")
g_ln_covid19_quarantine

jarque.bera.test(df1$covid19_quarantine) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_covid19_quarantine) #p-value < 0.05, so I reject H0 about normal distribution, but p-value is higher
#than in previous test

########################### VARIABLES CHECKING ###########################
#VARIABLE EDUCATION_EXPENDITURE

jarque.bera.test(df1$education_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_education_expenditure = log(df1$education_expenditure)
jarque.bera.test(df1$ln_education_expenditure) #p-value < 0.05

plotNormalHistogram(df1$education_expenditure, prob = FALSE,
                    main = "education_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_education_expenditure, prob = FALSE,
                    main = "ln_education_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
#VARIABLE HOUSES_AREA_PC

jarque.bera.test(df1$houses_area_pc) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_houses_area_pc = log(df1$houses_area_pc)
jarque.bera.test(df1$ln_houses_area_pc) #p-value < 0.05, but higher than without log

plotNormalHistogram(df1$houses_area_pc, prob = FALSE,
                    main = "houses_area_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_houses_area_pc, prob = FALSE,
                    main = "ln_houses_area_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
# VARIABLE BUDGET_REVE_PC
plotNormalHistogram(df1$budget_reve_pc, prob = FALSE,
                    main = "budget_reve_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_budget_reve_pc = log(df1$budget_reve_pc)

plotNormalHistogram(df1$ln_budget_reve_pc, prob = FALSE,
                    main = "ln_budget_reve_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_budget_reve_pc<-ggplot(df1, aes(x=wypadki, y=ln_budget_reve_pc)) +geom_point(color="red")
g_ln_budget_reve_pc

jarque.bera.test(df1$budget_reve_pc) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_budget_reve_pc) #p-value <0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE FEMININITY_RATIO

g_femininity_ratio<-ggplot(df1, aes(x=wypadki, y=femininity_ratio)) +geom_point(color="red")
g_femininity_ratio

plotNormalHistogram(df1$femininity_ratio, prob = FALSE,
                    main = "femininity_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_femininity_ratio = log(df1$femininity_ratio)

plotNormalHistogram(df1$ln_femininity_ratio, prob = FALSE,
                    main = "ln_femininity_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_femininity_ratio<-ggplot(df1, aes(x=wypadki, y=ln_femininity_ratio)) +geom_point(color="red")
g_ln_femininity_ratio

jarque.bera.test(df1$femininity_ratio) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_femininity_ratio) #p-value <0.05, but higher than in previous case

########################### VARIABLES CHECKING ###########################
#VARIABLE HEALTH_EXPENDITURE

jarque.bera.test(df1$health_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_health_expenditure = log(df1$health_expenditure)
jarque.bera.test(df1$ln_health_expenditure) #p-value < 0.05

plotNormalHistogram(df1$health_expenditure, prob = FALSE,
                    main = "health_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_health_expenditure, prob = FALSE,
                    main = "ln_health_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
#VARIABLE NURSERY_PLACES

jarque.bera.test(df1$nursery_places) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_nursery_places = log(df1$nursery_places)
jarque.bera.test(df1$ln_nursery_places) #p-value < 0.05, but p-value is higher

# nursery_places has values equal to 0, so I can't use the logarithm, because I get infinity values (log from 0 is equal to inf)

########################### VARIABLES CHECKING ###########################
#VARIABLE CHILDREN

jarque.bera.test(df1$children) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_children = log(df1$children)
jarque.bera.test(df1$ln_children) #p-value < 0.05, but p-value is higher

plotNormalHistogram(df1$children, prob = FALSE,
                    main = "children and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_children, prob = FALSE,
                    main = "ln_children and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
#VARIABLE URBANISATION_RATE

jarque.bera.test(df1$urbanisation_rate) #p-value < 0.05, but p-value is higher than in the next case

g_urbanisation_rate<-ggplot(df1, aes(x=wypadki, y=urbanisation_rate)) +geom_point(color="red")
g_urbanisation_rate

plotNormalHistogram(df1$urbanisation_rate, prob = FALSE,
                    main = "urbanisation_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
#VARIABLE HOUSES_AREA_PC

jarque.bera.test(df1$houses_area_pc) #p-value < 0.05, I reject H0 about normal distribution

plotNormalHistogram(df1$houses_area_pc, prob = FALSE,
                    main = "houses_area_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

########################### VARIABLES CHECKING ###########################
#VARIABLE SOCIAL_EXPENDITURE

jarque.bera.test(df1$social_expenditure) #p-value < 0.05, I reject H0 about normal distribution

plotNormalHistogram(df1$social_expenditure, prob = FALSE,
                    main = "social_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000)

describe(df1$social_expenditure)

########################### VARIABLES CHECKING ###########################
#VARIABLE FAMILY_EXPENDITURE

jarque.bera.test(df1$family_expenditure) #p-value < 0.05, I reject H0 about normal distribution

plotNormalHistogram(df1$family_expenditure, prob = FALSE,
                    main = "family_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 


describe(df1$family_expenditure)

########################### VARIABLES CHECKING ###########################
#VARIABLE DOCTORS

jarque.bera.test(df1$doctors) #p-value < 0.05, I reject H0 about normal distribution

plotNormalHistogram(df1$doctors, prob = FALSE,
                    main = "doctors and normal distribution",
                    linecol = "red",
                    length = 1000) 

describe(df1$doctors)


