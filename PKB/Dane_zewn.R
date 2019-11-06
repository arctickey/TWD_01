library(haven)
library(intsvy)
library(countrycode)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)

dane <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")
data <- dane
x <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = data)
pom1 <- codelist %>% select(continent,iso3c)
pom1 <- na.omit(pom1)
x1 <- left_join(x,pom1,by = c("CNT"="iso3c"))
x1 <- na.omit(x1)

# teach <- read.csv("./teachers.csv") %>% filter(TIME==2015,SUBJECT=="LOWSRY") %>% select(LOCATION,Value)
# x3 <- left_join(x1,teach,by=c("CNT"="LOCATION"))
# x3 <- na.omit(x3)

GDP <- read.csv("./PKB/GDP.csv",header = FALSE)
GDP <- select(GDP,V2,V50) %>% slice(4:n())
x2 <- left_join(x1,GDP,by=c("CNT"="V2"))

#Dane do uzyskania realnej sily nabywczej pieniadza
wartosc_pieniadza <- read.csv("big-mac-raw-index.csv")
wartosc_pieniadza <- wartosc_pieniadza[wartosc_pieniadza$date=="2015-07-01", c(2,4,7)]
x2 <- merge(x2, wartosc_pieniadza, by.x="CNT", by.y="iso_a3")


a <- ggplot(x2,aes(x=V50/dollar_price,y=Mean,colour=continent))+
  geom_point(size=4)+
  stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=V50/dollar_price,y=Mean))+
  ggtitle("Średni wynik w kraju względem PKB per capita")+
  labs(y = "Średni wynik w kraju", x="PKB per capita (realna siła nabywcza)",color="Kontynent")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
  geom_label_repel(aes(label = name), size = 5, color = "black")

a

#ggsave("PKB/PKB.png", width = 18, height = 9)


# b <- ggplot(x3,aes(x=Value,y=Mean,colour=continent))+
#   geom_point(size=4)+
#   stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=Value,y=Mean))+
#   ggtitle("Czas pracy nauczyciela vs sredni wynik")+
#   labs(y = "Średni wynik w kraju", x="Czas pracy nauczyciela",color="Kontynent")+
#   theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
# 
# ggsave("Teach_salaries,png",plot = b,device="png")
