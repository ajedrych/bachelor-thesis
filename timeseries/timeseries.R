library(readxl)
library(tidyverse)
library(scales)
install.packages("dsa")
library(dsa)
library(forecast)
install.packages("stR")
library(stR)
library(xts)
library(stargazer)
library(grid)

wwa <- read_excel("all_zdarzenia_warszawa2018_21.xlsx")

wwa <-
  wwa %>%
  mutate(date = as.Date(DATA_ZDARZ)) %>%
  group_by(date) %>%
  mutate(weekday= weekdays(date),
         month =  format(date, "%m"),
         year = format(date, "%Y"))


# boxplot plot - day of the week
level_order <- factor(wwa$weekday, level = c("poniedziałek", "wtorek", "środa",
                                             "czwartek", "piątek", "sobota", "niedziela"))


ggplot(wwa, aes(x= level_order, y=ID))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia
       ")+
  xlab("Dzień tygodnia
       ")

wwa %>%
  mutate(day = format(date, format = "%d")) %>%
  ggplot(aes(x = day, y = ID)) +
  geom_boxplot() +
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia
       ")+
  xlab("Dzień miesiąca
       ")


