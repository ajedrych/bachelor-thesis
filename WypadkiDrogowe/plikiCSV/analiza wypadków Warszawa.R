library(readxl)

options(scipen = 999)

#LOAD DATA
df <- read_excel("liczba_wypadków_warszawa_2019_20.xlsx") 
df$DATA_ZDARZ <- as.Date(df$DATA_ZDARZ)

colnames(df) <- c("data", "zdarzenia", "średnia roczna")

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

dates_vline <- as.Date(c("2020-03-11", "2020-10-17"))                 # Define positions of vline
dates_vline <- which(df$data %in% dates_vline)

# Usual area chart
p <- df %>%
  ggplot( aes(x=data, y=zdarzenia)) +
  geom_line(color="#69b3a2") +
  ylab("liczba zdarzeń drogowych w Warszawie")+
  geom_segment(aes(x = lubridate::ymd("2019 January 1"), y = 85, xend = lubridate::ymd("2019 December 31"), yend = 85))+ #średnia liczba zdarzeń drogowych w 2019 roku
  geom_segment(aes(x = lubridate::ymd("2020 January 1"), y = 67, xend = lubridate::ymd("2020 December 31"), yend = 67))+ #średnia liczba zdarzeń drogowych w 2020 roku
  geom_vline(xintercept = as.numeric(df$data[dates_vline]),
             col = "red")
  

p <- ggplotly(p)
p

#   geom_vline(xintercept = lubridate::ymd("2020 May 1"), linetype=4, color = 'black')