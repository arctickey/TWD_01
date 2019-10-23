# wczytywanie ramki danych 
EDU <- read.csv("EDU.csv")

# dane z 2015 r 

EDU_2015 <- EDU[ifelse(EDU[,6]==2015,TRUE,FALSE) & ifelse(EDU[,3]=="EARLYCHILDEDU",TRUE,FALSE),c(1,7)]
## wprowadzanie i poprawki ramik z danymi pissa 
dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
dane <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
dane <- best_in_all(10,dane)## ZMIEN W CELU ZMIANY DANYCH z pissa
dane <- dane[,c(1,33)]

# dane o krajach 
cod <- codelist
cod_final<- cod[,c(2,5,36)]
EDU_2015 <- na.omit(EDU_2015)
EDU_2015<- merge(EDU_2015,cod_final,by.x ="LOCATION",by.y="iso3c")


## łaczenie z danymi z pissa 
dane_plot3 <- merge(dane,EDU_2015,by.x="CNT",by.y="LOCATION")
colnames(dane_plot3)[3] <- "SPEND_EDU"
## grupowanie po krajach 
library(dplyr)
data_plot3<- dane_plot3 %>% group_by(CNT,SPEND_EDU,continent,country.name.en) %>% summarise(mean_all_by_CNT=mean(average_pv_all))

## Wykres
library(ggplot2)
ggplot(data_plot3,aes(x=SPEND_EDU,y=mean_all_by_CNT,colour=continent))+geom_point(size=4)+stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=SPEND_EDU,y=mean_all_by_CNT))+
  ggtitle("Średnia od wydatków na edukacje")+labs(y = "Średni wynik w kraju dla najlepszych 10%", x="Wydatki na eduakcje USD/Student",color="Kontynent")+theme(axis.title.x = element_text(size = 14),
                                                                                                                                           axis.title.y = element_text(size = 14))

ggsave("Wykres_SPEND_ON_EDU/all_EDU.png",width = 18, height = 9)
