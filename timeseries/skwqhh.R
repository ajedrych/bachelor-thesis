library(tidyverse)
library(scales)
library(dsa)
library(forecast)
library(stR)
library(xts)
library(stargazer)
library(grid)
library(readxl)
wwa <- read_excel("all_zdarzenia_warszawa2018_20.xlsx")

wwa <-
  wwa %>%
  mutate(date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarise(zdarzenia = sum(Value)) %>%
  mutate(weekday = weekdays(Date),
         month =  format(Date, "%m"),
         year = format(Date, "%Y"))

wwa %>%
  mutate(
    weekday = factor(weekday, levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))
  ) %>%
  ggplot(aes(x = weekday, y = zdarzenia)) +
  geom_boxplot() +
  labs(
    x = "Dzień tygodnia
    ",
    y = "Zdarzenia drogowe
    "
  )+
  theme_minimal()+
  theme(legend.title = element_blank())

wwa %>%
  mutate(day = format(Date, format = "%d")) %>%
  ggplot(aes(x = day, y = zdarzenia)) +
  geom_boxplot() +
  labs(
    x = "Dzień miesiąca
    ",
    y = "Zdarzenia drogowe
    "
  )+
  theme_minimal()+
  theme(legend.title = element_blank())

wwa %>%
  mutate(month = format(month, format = "%m")) %>%
  ggplot(aes(x = month, y = zdarzenia)) +
  geom_boxplot() +
  labs(
    x = "Miesiąc
    ",
    y = "Zdarzenia drogowe
    "
  )+
  theme_minimal()+
  theme(legend.title = element_blank())

ggplot(wwa) +
  geom_line(aes(Date, zdarzenia)) +
  labs(
    x = "",
    y = "Zdarzenia drogowe
    "
  ) +
  theme_minimal()+
  theme(legend.title = element_blank())

# Calculate daily adjusted and trend time series using DSA
Sys.setenv(TZ = "UTC")
daily_adjusted <- xts::xts(x = wwa$zdarzenia, order.by = wwa$Date)
daily_adjusted <- daily_adjusted[!is.na(daily_adjusted)]
dimnames(daily_adjusted)[[2]] <- "original"
reference_series <- daily_adjusted
restrict <- seq.Date(from = as.Date(stats::start(reference_series)),
                     to = as.Date(stats::end(reference_series)), by = "days")
restrict_forecast <- seq.Date(from=as.Date(stats::end(reference_series)) + 1,
                              length.out = 365, by = "days")
AllHol <- 
  merge(
    holidays[, c(
      "EasterSunday",
      "EasterMonday",
      "GoodFriday",
      "Ascension",
      "CorpusChristi",
      "Pentecost",
      "ChristmasEve",
      "ChristmasDay",
      "NewYearsEve",
      "NewYearsDay",
      "LabourDay"
    )])
AllHolUse <- dsa::multi_xts2ts(AllHol[restrict])
AllHolForecast <- multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]

daily_adjusted_sa <- 
  dsa::dsa(daily_adjusted,
           Log = TRUE,
           cval = 10,
           robust1 = TRUE,
           robust2 = TRUE,
           robust3 = TRUE,
           s.window1 = 13,
           s.window2 = NULL,
           s.window3 = 13,
           fourier_number = 26,
           regressor = AllHolUse,
           forecast_regressor = AllHolForecast,
           feb29 = "sfac"
  )
daily_adjusted_sa$reg$aicc
