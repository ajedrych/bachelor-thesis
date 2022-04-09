library("readxl")
library(ggplot2)

###################################################
######### COVID-19 kontynenty #####################

kontynenty <- read_excel("kontynenty_cumulative.xlsx")
kontynenty21 <- read_excel("kontynenty nowe przypadki.xlsx")

######### zachorowania skumulowane 2020 ################

ggplot(data = kontynenty, mapping = aes(x = miesiąc, y = zachorowania, colour = kontynent)) +
  geom_line(size=1, aes(linetype=kontynent))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zachorowań na COVID-19")+
  xlab("")

######### zgony skumulowane 2020 #######################

ggplot(data = kontynenty, mapping = aes(x = miesiąc, y = zgony, colour = kontynent)) +
  geom_line(size=1, aes(linetype=kontynent))+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zgonów spowodowanych COVID-19")+
  xlab("")

######### nowe przypadki 2021 #######################

ggplot(data = kontynenty21, mapping = aes(x = data, y = nowe_przypadki, colour = kontynent)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba nowych przypadków zachorowań na COVID-19")+
  xlab("")

###################################################
######### COVID-19 polska #####################

polska <- read_excel("polska_covid19.xlsx")

######### zachorowania ############################

ggplot(data = polska, mapping = aes(x = data, y = zachorowania)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zachorowań na COVID-19")+
  xlab("")

######### zachorowania skumulowane ################

ggplot(data = polska, mapping = aes(x = data, y = zachorowania_cumulated)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zachorowań na COVID-19")+
  xlab("")


######### zgony skumulowane  #####################

ggplot(data = polska, mapping = aes(x = data, y = zgony)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zgonów spowodowaych COVID-19")+
  xlab("")
