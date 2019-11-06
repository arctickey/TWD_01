library(haven)
library(intsvy)
library(countrycode)
library(tidyverse)



data <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

data1 <- data %>% select(CNT,PV1MATH:PV10SCIE)
data1 <- best_in_all(100,data1) 
data1 <- data1 %>% select(CNT,average_pv_all)
data1 <- data1 %>% podziel_grupy_PKB()

#wykres grupami dochodowymi z fukcji podziel_grupy_PKB
a0 <- ggplot(data1,aes(x=average_pv_all,fill=grupa_rozwoju))+geom_density(alpha=0.6)
ggsave(a0,filename = "Rozklad wynikow po grupach dochodow",device="png",width=16,height=9,units='in')


#Wybranie po dwoch panstw z kazdej grupy aby zobrazowac roznice w rozkladzie wynikow

#grupa1
data2 <- filter(data1,CNT %in% c("MEX","URY"))
a <- ggplot(data2,aes(x=average_pv_all,fill=CNT))+geom_density(alpha=0.6)
ggsave(a,filename="Najnizsza grupa dochodowa przyklad rozkladu wynikow",device="png",width=16,height=9,units='in')

#grupa2
data3 <- filter(data1,CNT %in% c("ISR","POL"))
b <- ggplot(data3,aes(x=average_pv_all,fill=CNT))+geom_density(alpha=0.6)
ggsave(b,filename = "Srednia grupa dochodowa przyklad rozkladu wynikow",device="png",width=16,height=9,units='in')


#grupa3
data4 <- filter(data1,CNT %in% c("USA","NZL"))
c <- ggplot(data4,aes(x=average_pv_all,fill=CNT))+geom_density(alpha=0.6)
ggsave(c,filename = "Najwyzsza grupa dochodowa przyklad rozkladu wynikow",device="png",width=16,height=9,units='in')




