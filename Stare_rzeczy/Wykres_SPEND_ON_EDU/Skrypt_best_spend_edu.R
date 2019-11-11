library(intsvy)
library(countrycode)
library(dplyr)

# wczytywanie ramki danych 
EDU <- read.csv("EDU.csv")

# dane z 2015 r 

EDU_2015 <- EDU[ifelse(EDU[,6]==2015,TRUE,FALSE) & ifelse(EDU[,3]=="EARLYCHILDEDU",TRUE,FALSE),c(1,7)]

## wprowadzanie i poprawki ramik z danymi pissa 
#dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")

dane1 <- pisa2015.mean.pv(pvlabel = c("MATH","SCIE","READ"), by = "CNT", data = dane)

#dane <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
#dane <- best_in_all(10,dane)## ZMIEN W CELU ZMIANY DANYCH z pissa

dane1 <- dane1[,c(1,3)]
  
# dane o krajach
cod <- countrycode::codelist
cod_final<- cod[,c(2,5,36)]
EDU_2015 <- na.omit(EDU_2015)
EDU_2015<- merge(EDU_2015,cod_final,by.x ="LOCATION",by.y="iso3c")

## łaczenie z danymi z pissa 
dane_plot3 <- merge(dane1,EDU_2015,by.x="CNT",by.y="LOCATION")
colnames(dane_plot3)[c(2,3)] <- c("mean_all_by_CNT","SPEND_EDU")

## grupowanie po krajach 

## Wykres

#Uwzglednienie realnej sily nabywczej pieniadza
wartosc_pieniadza <- read.csv("big-mac-raw-index.csv")
wartosc_pieniadza <- wartosc_pieniadza[wartosc_pieniadza$date=="2015-07-01", c(2,7)]
dane_plot3 <- merge(dane_plot3, wartosc_pieniadza, by.x="CNT", by.y="iso_a3")

library(ggplot2)
library(ggrepel)

ggplot(dane_plot3, aes(x=SPEND_EDU/dollar_price, y=mean_all_by_CNT,color=continent))+
  geom_point(size=4)+
  stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=SPEND_EDU/dollar_price,y=mean_all_by_CNT))+
  ggtitle("Średnia od wydatków na edukacje")+
  labs(y = "Średni wynik w kraju", x="Wydatki na eduakcje USD/Student")+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), title = element_text(size=20))+
  scale_colour_discrete(name = "Kontynenty", labels = c("Ameryki", "Azia", "Europa","Oceania"))+
  geom_label_repel(aes(label = country.name.en), size = 5, color = "black")
  # geom_label_repel(aes(label=country.name.en), size=6,color="black" ,data=subset(dane_plot3, SPEND_EDU >16000 ))+
  # geom_label_repel(aes(label=country.name.en), size=6,color="black" ,data=subset(dane_plot3, mean_all_by_CNT >523 ))+
  # geom_label_repel(aes(label=country.name.en), size=6,color="black" ,data=subset(dane_plot3, SPEND_EDU >5000 & mean_all_by_CNT<450))+
  # geom_label_repel(aes(label=country.name.en), size=6,color="black" ,data=subset(dane_plot3, SPEND_EDU <1000 ))+
  # geom_label_repel(aes(label=country.name.en), size=6,color="black" ,data=dane_plot3[25,])
  

                                                                                                                                       

#ggsave("Wykres_SPEND_ON_EDU/all_EDU.png", width = 18, height = 9)
