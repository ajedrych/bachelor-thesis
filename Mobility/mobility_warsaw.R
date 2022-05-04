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

######## APPLE MOBILITY TRENDS ###############

wwa_apple <- read_excel("warszawa_apple_2020.xlsx")

scaleFactor1 <- max(wwa_apple$walking)/ max(wwa_apple$stringency_index)

ggplot(wwa_apple, aes(x = data)) +
  geom_line(aes(y = driving, colour = "samochód"), size=1) +
  geom_line(aes(y= walking, colour = "spacer"), size = 1) +
  geom_hline(yintercept=100)+
  geom_line(aes(y = stringency_index * scaleFactor1, colour = "COVID-19\nStringency Index"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor1, name="COVID-19 Stringency Index
                        "))+ 
  scale_colour_manual(values = c("black", "#F8766D", "#7CAE00"))+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

ggplot(wwa_apple, aes(x = data)) +
  geom_line(aes(y = driving, colour = "samochód"), size=1) +
  geom_line(aes(y= walking, colour = "spacer"), size = 1) +
  geom_hline(yintercept=100)+
  scale_colour_manual(values = c("#F8766D", "#7CAE00"))+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

wwa_apple_w <- read_excel("warszawa_apple_2020_week.xlsx")

ggplot(wwa_apple_w, aes(x = data)) +
  geom_line(aes(y = driving, colour = "driving"), size=1) +
  geom_line(aes(y= walking, colour = "walking"), size = 1) +
  geom_hline(yintercept=100)+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")


library(stargazer)
summary(wwa_apple)


######## GOOGLE MOBILITY TRENDS ###############

wwa_google <- read_excel("warszawa_google_2020.xlsx")

summary(wwa_google)

wwa_google_w <- read_excel("warszawa_google_2020_week.xlsx")

ggplot(wwa_google_w, aes(x = date)) +
  geom_line(aes(y = retail_and_recreation, colour = "retail_recreation"), size=1) +
  geom_line(aes(y= grocery_and_pharmacy, colour = "food_pharmacy"), size = 1) +
  geom_line(aes(y= parks, colour = "parks"), size = 1) +
  geom_line(aes(y= transit, colour = "transit"), size = 1) +
  geom_line(aes(y= workplaces, colour = "workplace"), size = 1) +
  geom_line(aes(y= residential, colour = "home"), size = 1) +
  geom_hline(yintercept=100)+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")



############### KORELACJA ####################

df <- read_excel("warszawa_analiza_korelacja.xlsx")
df1 <- df[ -c(1) ]

#korelacja czastkowa
library(ppcor)
czast <- pcor(na.omit(df1), method = c("pearson"))
corrplot(czast$estimate, method =  "circle")

#korelacja pearsona
library(psych)
pears <- corr.test(x = na.omit(df1), use = "pairwise", method = "pearson")
corrplot(pears$r, method = "circle")

#korelacja speramana
spear <- corr.test(x = na.omit(df1), use = "pairwise", method = "spearman")
corrplot(spear$r, method = "circle")

# CORRELATION PLOT
library(corrplot)
kor<-cor(df1)
corrplot(kor, method="circle") 
        

library("stargazer")
stargazer(as.data.frame(df), type = "text", digits = 1)


#manova

man <- manova(cbind(driving, walking, retail_recreation, food_pharmacy, parks, transit, workplaces, home)~new_cases+new_deaths+stringency_index, data= df1)
summary(man)

man1 <- manova(cbind(retail_recreation, food_pharmacy, parks, transit, workplaces, home)~new_cases+new_deaths+stringency_index, data= df1)
summary(man1)

man2 <- manova(cbind(retail_recreation, food_pharmacy, parks, transit, workplaces)~new_cases+new_deaths+stringency_index, data= df1)
summary(man2)
