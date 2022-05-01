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
  geom_line(aes(y = driving, colour = "samochód"), size=1) +
  geom_line(aes(y= walking, colour = "spacer"), size = 1) +
  geom_hline(yintercept=100)+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")


######## GOOGLE MOBILITY TRENDS ###############

wwa_google <- read_excel("warszawa_google_2020_week.xlsx")

ggplot(wwa_google, aes(x = date)) +
  geom_line(aes(y = retail_and_recreation, colour = "handel i rekreacja"), size=1) +
  geom_line(aes(y= grocery_and_pharmacy, colour = "sklepy spożywcze i apteki"), size = 1) +
  geom_line(aes(y= parks, colour = "parki"), size = 1) +
  geom_line(aes(y= transit, colour = "stacje i przystanki"), size = 1) +
  geom_line(aes(y= workplaces, colour = "miejsca pracy"), size = 1) +
  geom_line(aes(y= residential, colour = "miejsca zamieszkania"), size = 1) +
  geom_hline(yintercept=100)+
  labs(y = "Zmiana
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")




