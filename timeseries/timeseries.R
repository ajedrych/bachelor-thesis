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

wwa <- read_excel("all_zdarzenia_warszawa2018_20.xlsx")

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

#boxplot day of the month
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

#boxplot miesiąc
wwa %>%
  mutate(month = format(month, format = "%m")) %>%
  ggplot(aes(x = month, y = ID)) +
  geom_boxplot() +
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia
       ")+
  xlab("Miesiąc
       ")

# daily time series chart (line)
ggplot(wwa) +
  geom_line(aes(date, ID)) +
  theme_minimal()+
  theme(legend.title = element_blank())+
  ylab("Zdarzenia
       ")+
  xlab("")+
  scale_x_date(date_breaks = "4 months",
               date_minor_breaks = "4 months", 
               labels = date_format("%Y-%m"))

# Calculate daily adjusted and trend time series using DSA
Sys.setenv(TZ = "UTC")
daily_adjusted <- xts::xts(x = wwa$ID, order.by = wwa$date)
daily_adjusted <- daily_adjusted[!is.na(daily_adjusted)]
dimnames(daily_adjusted)[[2]] <- "original"
reference_series <- daily_adjusted
restrict <- seq.Date(from = stats::start(reference_series),
                     to = stats::end(reference_series), by = "days")
restrict_forecast <- seq.Date(from=stats::end(reference_series) + 1,
                              length.out = 365, by = "days")



AllHol <- 
  merge(
    dsa::holidays[, c(
      "EasterSunday",
      "EasterMonday",
      "EasterMondayAft1Day",
      "HolyThursday",
      "GoodFriday",
      "Ascension",
      "CorpusChristi",
      "Pentecost",
      "PentecostAft1Day",
      "PentecostMonday",
      "ChristmasEve",
      "ChristmasDay",
      "NewYearsEve",
      "NewYearsDay",
      "ReformationDay",
      "LabourDay",
      "GermanUnity"
    )],
    stats::lag(dsa::holidays$LabourDay, 1),
    stats::lag(dsa::holidays$ChristmasDay, 1),
    stats::lag(dsa::holidays$NewYearsDay, 1),
    stats::lag(dsa::holidays$NewYearsDay, 5)
  )

AllHolUse <- dsa::multi_xts2ts(AllHol[restrict])
AllHolForecast <-dsa::multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
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

# Merge DSA data to daily data
original <- dsa::get_original(daily_adjusted_sa)
original <- data.frame(date = index(original), coredata(original))
sa <- dsa::get_sa(daily_adjusted_sa)
sa <- data.frame(date = index(sa), coredata(sa))
trend <- dsa::get_trend(daily_adjusted_sa)
trend <- data.frame(date = index(trend), coredata(trend))
daily <- left_join(original, sa)
daily <- left_join(daily, trend)

holiday <-
  data.frame(estimate = daily_adjusted_sa$reg$coef, se = sqrt(diag(daily_adjusted_sa$reg$var.coef)))[59:75,] %>%
  mutate(
    t_stat = (estimate / se)^2,
    p_value = round(pchisq(t_stat, df =1 , lower = FALSE), 4)
  )

holiday

stargazer(holiday, summary = FALSE)
