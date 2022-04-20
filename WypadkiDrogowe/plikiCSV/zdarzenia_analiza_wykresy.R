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

############################################################
#zmiana 2020 do 2019 wypadki, kolizje, smierc, ranni warszawa

zmiana <- read_excel("wykres_zmiana2019_20_warszawa.xlsx")

level_order <- factor(zmiana$miesiąc, level = c('sty', 'lut', 'mar', 'kwi', 'maj', 'cze',
                                                'lip', 'sie', 'wrz', 'paź', 'lis', 'gru'))

ggplot(zmiana, aes(fill=legenda, y=zmiana, x=level_order)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("")+
  xlab("")
