library("readxl")
library(ggplot2)
options(scipen = 999)


#########################################
####### POLSKA WYPADKI DROGOWE ##########
#########################################

polska2000 <- read_excel("wypadki_polska_2000-2020.xlsx")

##############################
# wypadki komunikacyjne polska

ggplot(data = polska2000, mapping = aes(x = rok)) +
  geom_line(aes(y = wypadki), size=1, linetype = 1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Wypadki komunikacyjne
       ")+
  xlab("")

###############################
# ofiary i ranni polska

scaleFactor_polska2000 <- max(polska2000$ranni) / max(polska2000$ofiary)

ggplot(polska2000, aes(x = rok))+
  geom_line(aes(y = ranni, colour = "ranni"), size = 1)+
  geom_line(aes(y = ofiary * scaleFactor_polska2000, colour = "ofiary"), size = 1)+
  scale_y_continuous(sec.axis = sec_axis(~./scaleFactor_polska2000, name = "Ofiary
                                         "))+
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Ranni
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")



warszawa2010 <- read_excel("wypadki_warszawa_rok_2010-2021.xlsx")

ggplot(data = warszawa2010, mapping = aes(x = Date)) +
  geom_line(aes(y = Value), size=1, linetype = 1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba wypadków komunikacyjnych w Warszawie
       ")+
  xlab("")
