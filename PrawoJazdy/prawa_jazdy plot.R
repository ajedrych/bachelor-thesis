library("readxl")
library(ggplot2)
options(scipen = 999)

prawa <- read_excel("prawo_jazdy.xlsx")

scaleFactor <- max(prawa$polska) / max(prawa$mazowieckie)

ggplot(prawa, aes(x = data)) +
  geom_line(aes(y = polska, colour = "Polska"), size=1) + 
  geom_line(aes(y = mazowieckie * scaleFactor, colour = "mazowieckie"), size=1) +
  scale_y_continuous(sec.axis = sec_axis(~ . /scaleFactor, name="Prawa jazdy wydane w woj. mazowieckim
                                         "))+ 
  scale_colour_manual(values = c("black", "#F8766D"))+
  labs(y = "Prawa jazdy wydane w Polsce
       ",
       colour = "")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  xlab("")
