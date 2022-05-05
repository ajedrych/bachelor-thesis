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




############# podstawowe staty

df <- read_excel("wykresy.xlsx")
library("stargazer")
stargazer(as.data.frame(df), type = "text", digits = 1)

#################### boxploty

wwa <- read_excel("wykresy_warszawa_2018_21.xlsx")

wwa <-
  wwa %>%
  mutate(date = as.Date(DATA_ZDARZ)) %>%
  group_by(date) %>%
  mutate(weekday= weekdays(date),
         month =  format(date, "%m"),
         year = format(date, "%Y"))

level_order <- factor(wwa$weekday, level = c("poniedziałek", "wtorek", "środa",
                                             "czwartek", "piątek", "sobota", "niedziela"))

level_order1 <- factor(df18_19$weekday, level = c("poniedziałek", "wtorek", "środa",
                                             "czwartek", "piątek", "sobota", "niedziela"))

level_order2 <- factor(df20$weekday, level = c("poniedziałek", "wtorek", "środa",
                                             "czwartek", "piątek", "sobota", "niedziela"))


df18_19 <- wwa[wwa$date >= "2018-01-01" & wwa$date <= "2019-12-31", ]
df20 <- wwa[wwa$date >= "2020-01-01" & wwa$date <= "2020-12-31", ]


options(scipen = 999)
library(rcompanion)
par(mfrow=c(1,2))

############ dni tyg
ggplot(df18_19, aes(x= level_order1, y=ID))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia w latach 2018-2019
       ")+
  xlab("")

ggplot(df20, aes(x= level_order2, y=ID))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia w 2020 roku
       ")+
  xlab("")



# miesiace
df18_19 %>%
  mutate(month = format(month, format = "%m")) %>%
  ggplot(aes(x = month, y = ID)) +
  geom_boxplot() +
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia w latach 2018-2019
       ")+
  xlab("
       Miesiąc")

df20 %>%
  mutate(month = format(month, format = "%m")) %>%
  ggplot(aes(x = month, y = ID)) +
  geom_boxplot() +
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia w 2020 roku
       ")+
  xlab("
       Miesiąc")

#rok

ggplot(wwa, aes(x= year, y=ID))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia
       ")+
  xlab("")
