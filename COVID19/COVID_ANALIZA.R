library("readxl")
library("sandwich")
library("knitr")
library("tidyverse")

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

options(scipen = 999)

#DANE ZAKAŻENIA COVID-19 WARSZAWA
df <- read_excel("covid19_warszawa.xlsx") 
View(df)

colnames(df) <- c("data", "zachorowania", "zgony", "kwarantanna")

df$data <- as.Date(df$data)

#NOWE PRZYPADKI ZACHOROWAŃ NA COVID-19 W WARSZAWIE
plot_zachorowania <- ggplot(df, aes(x=data, y=zachorowania)) +
  geom_line(color="#000081") +
  xlab("")+
  ylab("Liczba nowych przypadków zachorowań na COVID-19 w Warszawie")

plot_zachorowania

#ZGONY W WARSZAWIE SPOWODOWANE COVID-19
plot_zgony <- ggplot(df, aes(x=data, y=zgony)) +
  geom_line(color="#000081") +
  xlab("")+
  ylab("Liczba zgonów spowodowanych COVID-19 w Warszawie")

plot_zgony

#OSOBY PRZEBYWAJĄCE NA KWARANTANNIE W WARSZAWIE
plot_kwarantanna <- ggplot(df, aes(x=data, y=kwarantanna)) +
  geom_line(color="#000081") +
  xlab("")+
  ylab("Liczba osób przebywających na kwarantannie w Warszawie")

plot_kwarantanna
