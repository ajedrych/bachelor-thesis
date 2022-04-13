library("readxl")
library(ggplot2)
options(scipen = 999)

polska2000 <- read_excel("wypadki_polska_2000-2020.xlsx")

ggplot(data = polska2000, mapping = aes(x = rok)) +
  geom_line(aes(y = wypadki), size=1, linetype = 1)+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Liczba wypadków komunikacyjnych w Polsce
       ")+
  xlab("")

scaleFactor_polska2000 <- max(polska2000$ranni) / max(polska2000$ofiary)

ggplot(polska2000, aes(x=rok)) +
  geom_line( aes(y=ranni), size=1) + 
  geom_line( aes(y=ofiary* scaleFactor_polska2000), size=1, colour = "#F8766D") +
  scale_y_continuous(
    name = "Liczba rannych
    ",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ . /scaleFactor_polska2000, name="Liczba ofiar
                        "))+ 
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
