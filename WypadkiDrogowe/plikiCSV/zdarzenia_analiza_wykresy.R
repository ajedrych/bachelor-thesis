library("readxl")
library(ggplot2)
library(tidyverse)
library(lubridate)
options(scipen = 999)

############################################################################
############ ANALIZA STATYSTYCZNA WARSZAWA WYPADKI #########################

wwa2018 <- read_excel("all_zdarzenia_warszawa2018.xlsx")
wwa2019 <- read_excel("all_zdarzenia_warszawa2019.xlsx")
wwa2020 <- read_excel("all_zdarzenia_warszawa2020.xlsx")

aggregate(zdarzenia~week(data), data = wwa2018, sum)
aggregate(zdarzenia~week(data), data = wwa2019, sum)
aggregate(zdarzenia~week(data), data = wwa2020, sum)

wwa <- read_excel("wykres_2018_20_warszawa_zdarzenia.xlsx")
###############################
# 2018, 2019 i 2020 na jednym wykresie zdarzenia drogowe

ggplot(data = wwa, mapping = aes(x = week, y = zdarzenia, colour = as.factor(rok))) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia drogowe
       ")+
  xlab("")+
  scale_x_continuous(breaks = month_numeric, 
                     labels = month_label)

month <- seq(as.Date("2020-01-01"), 
             as.Date("2020-12-31"), 
             by = "2 month")

month_numeric <- as.numeric(format(month, format = "%U"))
month_label <- format(month, format = "%b")

