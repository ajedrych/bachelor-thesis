library("readxl")
library(ggplot2)
options(scipen = 999)
library(tidyverse)
library(scales)
library(dsa)
library(forecast)
library(stR)
library(xts)
library(stargazer)
library(grid)
library(ppcor)
library(corrplot)
library(psych)

df <- read_excel("model_2020.xlsx")
df1 <- subset(df, select = c("traffic_incidents","driving", "walking", "retail_recreation", "food_pharmacy", "parks", "transit", "workplaces", "home"))
df_t <- subset(df, select = c("traffic_incidents","quarantine", "new_cases", "new_deaths", "stringency_index"))
df_d <- subset(df, select = c("dead_injured_incidents","quarantine", "new_cases", "new_deaths", "stringency_index" ))
df_f <- subset(df, select = c("foreigners_incidents","quarantine", "new_cases", "new_deaths", "stringency_index" ))
df_b <- subset(df, select = c("bike_pedestrian_incidents","quarantine", "new_cases", "new_deaths", "stringency_index" ))
df_a <- subset(df, select = c("alcohol_incidents","quarantine", "new_cases", "new_deaths", "stringency_index" ))
df2 <- subset(df, select = c("dead_injured_incidents","foreigners_incidents", "bike_pedestrian_incidents", "alcohol_incidents","quarantine", "new_cases", "new_deaths", "stringency_index"))

shapiro.test(df$traffic_incidents) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$dead_injured_incidents) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$foreigners_incidents) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$bike_pedestrian_incidents) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$alcohol_incidents) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$quarantine) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$new_cases) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$new_deaths) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$stringency_index) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df$pandemic) # p< 0.05 - rozklad nie jest r. normalnym

#################### mobilność #######################

#korelacja pearsona
pears1 <- corr.test(x = na.omit(df1), use = "pairwise", method = "pearson")
corrplot(pears1$r, method = "number")

#korelacja speramana
spear1 <- corr.test(x = na.omit(df1), use = "pairwise", method = "spearman")
corrplot(spear1$r, method = "number")

################## traffic incidents ###################

#korelacja pearsona
pears_t <- corr.test(x = na.omit(df_t), use = "pairwise", method = "pearson")
corrplot(pears_t$r, method = "number")

#korelacja speramana
spear_t <- corr.test(x = na.omit(df_t), use = "pairwise", method = "spearman")
corrplot(spear_t$r, method = "number")


library(rcompanion)
par(mfrow=c(2,2))
################## dead and injured incidents ###################

#korelacja pearsona
pears_d <- corr.test(x = na.omit(df_d), use = "pairwise", method = "pearson")
corrplot(pears_d$r, method = "number")

#korelacja speramana
spear_d <- corr.test(x = na.omit(df_d), use = "pairwise", method = "spearman")
corrplot(spear_d$r, method = "circle")

################## foreigners incidents ###################

#korelacja pearsona
pears_f <- corr.test(x = na.omit(df_f), use = "pairwise", method = "pearson")
corrplot(pears_f$r, method = "number")

#korelacja speramana
spear_f <- corr.test(x = na.omit(df_f), use = "pairwise", method = "spearman")
corrplot(spear_f$r, method = "circle", title = "foreigners")

################## bike and pedestrian incidents ###################

#korelacja pearsona
pears_b <- corr.test(x = na.omit(df_b), use = "pairwise", method = "pearson")
corrplot(pears_b$r, method = "number")

#korelacja speramana
spear_b <- corr.test(x = na.omit(df_b), use = "pairwise", method = "spearman")
corrplot(spear_b$r, method = "circle")

################## alcohol incidents ###################

#korelacja pearsona
pears_a <- corr.test(x = na.omit(df_a), use = "pairwise", method = "pearson")
corrplot(pears_a$r, method = "number")

#korelacja speramana
spear_a <- corr.test(x = na.omit(df_a), use = "pairwise", method = "spearman")
corrplot(spear_a$r, method = "circle")


# all

par(mfrow = c(1,1))

#korelacja speramana
spear2 <- corr.test(x = na.omit(df2), use = "pairwise", method = "spearman")
corrplot(spear2$r, method = "circle")




library(rcompanion)
par(mfrow=c(1,2))
corrplot(pears$r, method = "circle", title = "\n\n\nKorelacja Pearsona")
corrplot(spear$r, method = "circle", title = "\n\n\nKorelacja Spearmana")


# CORRELATION PLOT
library(corrplot)
kor<-cor(df1)
corrplot(kor, method="circle") 

shapiro.test(df1$driving) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$walking) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$retail_recreation) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$food_pharmacy) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$parks) # p >0.05 - rozkład normalny
shapiro.test(df1$transit) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$workplaces) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$home) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$new_cases) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$new_deaths) # p< 0.05 - rozklad nie jest r. normalnym
shapiro.test(df1$stringency_index) # p< 0.05 - rozklad nie jest r. normalnym

library("stargazer")
stargazer(as.data.frame(df), type = "text", digits = 1)
