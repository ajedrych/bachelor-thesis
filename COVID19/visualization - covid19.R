library("readxl")
library(ggplot2)
options(scipen = 999)

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

polska <- read_excel("polska-covid.xlsx")
polska_pierwsza <- read_excel("polska-covid-pierwsza-fala.xlsx") #pierwsza fala

########## zachorowania pierwsza fala i stringency index #########

scaleFactor_pierwsza <- max(polska_pierwsza$zachorowania) / max(polska_pierwsza$stringency_index)

ggplot(polska_pierwsza, aes(x=data)) +
  geom_line( aes(y=zachorowania), size=1, linetype = 1) + 
  geom_line( aes(y=stringency_index* scaleFactor_pierwsza), size=1, linetype = 2, colour = "#F8766D") +
  scale_y_continuous(
    name = "Liczba zachorowań na COVID-19
    ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . /scaleFactor_pierwsza, name="COVID-19 Stringency Index
                        "))+ 
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")


########## zachorowania 2020-2021 i stringency index #########

scaleFactor <- max(polska$zachorowania) / max(polska$stringency_index)

ggplot(polska, aes(x=data)) +
  geom_line( aes(y=zachorowania), size=1, linetype = 1) + 
  geom_line( aes(y=stringency_index* scaleFactor), size=1, linetype = 2, colour = "#F8766D") +
  scale_y_continuous(
    name = "Liczba zachorowań na COVID-19
    ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . /scaleFactor, name="COVID-19 Stringency Index
                        "))+ 
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

########## zgony 2020-2021 i stringency index #########

scaleFactor_zgony <- max(polska$zgony) / max(polska$stringency_index)

ggplot(polska, aes(x=data)) +
  geom_line( aes(y=zgony), size=1, linetype = 1) + 
  geom_line( aes(y=stringency_index* scaleFactor_zgony), size=1, linetype = 2, colour = "#F8766D") +
  scale_y_continuous(
    name = "Liczba zgonów spowodowanych COVID-19
    ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . /scaleFactor_zgony, name="COVID-19 Stringency Index
                        "))+ 
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

########## zachorowania i zgony 2020-2021 #########

scaleFactor1 <- max(polska$zachorowania) / max(polska$zgony)

ggplot(polska, aes(x=data)) +
  geom_line( aes(y=zachorowania), size=1) + 
  geom_line( aes(y=zgony* scaleFactor1), size=0.5, colour = "#F8766D") +
  scale_y_continuous(
    name = "Liczba zachorowań na COVID-19
    ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . /scaleFactor1, name="Liczba zgonów spowodowanych COVID-19
                        "))+ 
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")

########## zachorowania i srednia kroczaca #########

ggplot(data = polska, mapping = aes(x = data)) +
  geom_line(aes(y = zachorowania), size=0.5, linetype = 1)+
  geom_line(aes(y=srednia), size=1, linetype = 2, colour = "#F8766D" )+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba zachorowań na COVID-19")+
  xlab("")
