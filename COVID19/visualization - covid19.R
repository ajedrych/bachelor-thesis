library("readxl")
library(ggplot2)
options(scipen = 999)

###################################################
######### COVID-19 kontynenty #####################

kontynenty <- read_excel("kontynenty_cumulative.xlsx")
kontynenty21 <- read_excel("kontynenty nowe przypadki.xlsx")

######### zachorowania skumulowane 2020 ################

library(rcompanion)
par(mfrow=c(1,2))

zacho <- ggplot(data = kontynenty, mapping = aes(x = miesiąc, y = zachorowania, colour = kontynent)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("Zachorowania
       ")+
  xlab("")

######### zgony skumulowane 2020 #######################

zgon <- ggplot(data = kontynenty, mapping = aes(x = miesiąc, y = zgony, colour = kontynent)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zgony
       ")+
  xlab("")

require(gridExtra)

grid.arrange(zacho, zgon, ncol=2)

######### nowe przypadki 2021 #######################

ggplot(data = kontynenty21, mapping = aes(x = data, y = nowe_przypadki, colour = kontynent)) +
  geom_line(size=1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zachorowania
       ")+
  xlab("")

###################################################
######### COVID-19 polska #####################

polska <- read_excel("polska-covid.xlsx")
polska_pierwsza <- read_excel("polska-covid-pierwszafala.xlsx") #pierwsza fala
polska_szczep <- read_excel("szczepienia polska.xlsx")

########## zachorowania pierwsza fala i stringency index #########

scaleFactor_pierwsza <- max(polska_pierwsza$zachorowania) / max(polska_pierwsza$stringency_index)

ggplot(polska_pierwsza, aes(x = data)) +
  geom_line(aes(y = zachorowania, colour = "zachorowania"), size=1) + 
  geom_line(aes(y = stringency_index * scaleFactor_pierwsza, colour = "COVID-19\nStringency\nIndex"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor_pierwsza, name="COVID-19 Stringency Index
                        "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Zachorowania
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")


########## zachorowania 2020-2021 i stringency index #########

scaleFactor <- max(polska$zachorowania) / max(polska$stringency_index)

ggplot(polska, aes(x = data)) +
  geom_line(aes(y = zachorowania, colour = "zachorowania"), size=1) + 
  geom_line(aes(y = stringency_index * scaleFactor, colour = "COVID-19\nStringency\nIndex"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor, name="COVID-19 Stringency Index
                        "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Zachorowania
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")


########## zgony 2020-2021 i stringency index #########

scaleFactor_zgony <- max(polska$zgony) / max(polska$stringency_index)

ggplot(polska, aes(x = data)) +
  geom_line(aes(y = zgony, colour = "zgony"), size=1) + 
  geom_line(aes(y = stringency_index * scaleFactor_zgony, colour = "COVID-19\nStringency\nIndex"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor_zgony, name="COVID-19 Stringency Index
                        "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Zgony
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

########## zachorowania i zgony 2020-2021 #########

scaleFactor1 <- max(polska$zachorowania) / max(polska$zgony)

ggplot(polska, aes(x = data)) +
  geom_line(aes(y = zachorowania, colour = "zachorowania"), size=1) + 
  geom_line(aes(y = zgony * scaleFactor1, colour = "zgony"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor1, name="Zgony
                        "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Zachorowania
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

############ szczepienia polska ############

ggplot(data = polska_szczep, mapping = aes(x = data)) +
  geom_line(aes(y = szczepionki), size=1, linetype = 1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zaszczepieni
       ")+
  xlab("")

#################################################################
################### COVID-19 WARSZAWA PIERWSZA FALA #############

wwa_pierwsza <- read_excel("covid19_WWA_pierwszafala.xlsx")


scaleFactor_wwa <- max(wwa_pierwsza$ill_cumulated) / max(wwa_pierwsza$death_cumulated)

ggplot(wwa_pierwsza, aes(x = date)) +
  geom_line(aes(y = ill_cumulated, colour = "zachorowania"), size=1) + 
  geom_line(aes(y = death_cumulated * scaleFactor_wwa, colour = "zgony"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor_wwa, name="Zgony
                                         "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Zachorowania
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

