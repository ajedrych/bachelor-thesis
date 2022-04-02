library(readxl)

options(scipen = 999)

#LOAD DATA
df <- read_excel("liczba_wypadków_warszawa_2019_20.xlsx") 
df$DATA_ZDARZ <- as.Date(df$DATA_ZDARZ)

colnames(df) <- c("data", "zdarzenia", "średnia roczna", "średnia lockdown")

library("stargazer")
stargazer(as.data.frame(df), type = "text")

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

dates_vline <- as.Date(c("2020-03-11", "2020-10-17")) #lockdowny
dates_vline <- which(df$data %in% dates_vline)

# Usual area chart
avg_year <- df %>%
  ggplot( aes(x=data, y=zdarzenia)) +
  geom_line(color="#69b3a2") +
  xlab("")+
  ylab("Liczba zdarzeń drogowych w Warszawie")+
  geom_segment(aes(x = lubridate::ymd("2019 January 1"), y = 85, xend = lubridate::ymd("2019 December 31"), yend = 85))+ #średnia liczba zdarzeń drogowych w 2019 roku
  geom_segment(aes(x = lubridate::ymd("2020 January 1"), y = 67, xend = lubridate::ymd("2020 December 31"), yend = 67))+ #średnia liczba zdarzeń drogowych w 2020 roku
  geom_vline(xintercept = as.numeric(df$data[dates_vline]),
             col = "red")
  
avg_year <- ggplotly(avg_year)
avg_year

######################
#### AVG LOCKDOWN ####
######################

# Usual area chart
avg_lockdown <- df %>%
  ggplot(aes(x=data, y=zdarzenia)) +
  geom_line(color="#69b3a2") +
  xlab("")+
  ylab("Liczba zdarzeń drogowych w Warszawie")+
  geom_segment(aes(x = lubridate::ymd("2019 January 1"), y = 85, xend = lubridate::ymd("2020 March 11"), yend = 85))+ #średnia liczba zdarzeń drogowych w 2019 roku
  geom_segment(aes(x = lubridate::ymd("2020 March 12"), y = 63, xend = lubridate::ymd("2020 December 31"), yend = 63))+ #średnia liczba zdarzeń drogowych w 2020 roku
  geom_vline(xintercept = as.numeric(df$data[dates_vline]),
             col = "red")

avg_lockdown <- ggplotly(avg_lockdown)
avg_lockdown


####################
#### Sezonowosc ####
####################

df2018_20 <- read_excel("liczba_wypadków_warszawa.xlsx") 
df2018_20$DATA_ZDARZ <- as.Date(df2018_20$DATA_ZDARZ)

colnames(df2018_20) <- c("data", "zdarzenia")

df2018_20$Rok <- format(df2018_20$data, "%Y")

df2018_20$DayMonth <- format(as.Date(df2018_20$data), "%d-%m")

df2018_20$Y2018 <- ifelse(df2018_20$Rok==2018,1,0)
df2018_20$Y2019 <- ifelse(df2018_20$Rok==2019,1,0)
df2018_20$Y2020 <- ifelse(df2018_20$Rok==2020,1,0)

sez2018_20 <- ggplot(df2018_20, aes(x = DayMonth, y = zdarzenia, color = Rok, group = Rok)) + 
  geom_point() + 
  geom_line() + 
  xlab("")+
  facet_grid(rows = vars(Rok)) + 
  theme(axis.text.x = element_blank())
print(sez2018_20)


library(plyr)
df2018_20$week <- format(df2018_20$data, format="%Y-%U")
df1 <- ddply(df2018_20, .(week), summarize, suma=sum(zdarzenia))
df1$Rok <- as.Date(df1$week, format ="%Y")
df1$Rok <- format(df1$Rok, "%Y")

df2018_20$Y2018 <- ifelse(df2018_20$Rok==2018,1,0)
df2018_20$Y2019 <- ifelse(df2018_20$Rok==2019,1,0)



################################################################################
weeks_suma <- read_excel("liczba_wypadków_warszawa_2019_20_tydzien_suma.xlsx") 

colnames(weeks_suma) <- c("tydzien", "r2019", "r2020", "r2021")

ggplot(weeks_suma, aes(tydzien, y = value, color = variable)) + 
  geom_line(aes(y = r2019, col = "2019")) + 
  geom_line(aes(y = r2020, col = "2020"))+
  geom_line(aes(y = r2021, col = "2021"))

