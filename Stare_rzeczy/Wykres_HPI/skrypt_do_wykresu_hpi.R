# wczytywanie danych o HPI 
HPI <- read.csv("data_HPI.csv")
HPI <- HPI[,-1]
# wczytywanie danych o krajach 
install.packages("countrycode")
cod_final<- cod[,c(2,5,36)]
HPI <- HPI[,c(2,11)]
HPI_COD <- merge(HPI,cod_final,by.x ="Country",by.y="country.name.en")

## wprowadzanie i poprawki ramik z danymi pissa 
#dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
#dane <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
#dane <- best_in_all(10,dane)## ZMIEN W CELU ZMIANY DANYCH z pissa
#dane <- dane[,c(1,33)]

dane_plot2 <- merge(dane,HPI_COD,by.x="CNT",by.y="iso3c")
library(dplyr)
## grupowanie danych po krajach 

data_plot2<- dane_plot2 %>% group_by(CNT,Country,continent,Happy.Planet.Index) %>% summarise(mean_all_by_CNT=mean(average_pv_all))

## rysowanie i zapisywanie wykresu 

library(ggplot2)
ggplot(data_plot2,aes(x=Happy.Planet.Index,y=mean_all_by_CNT,colour=continent))+geom_point(size=4)+stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=Happy.Planet.Index,y=mean_all_by_CNT))+
  ggtitle("Średnia od HPI")+labs(y = "Średni wynik w kraju dla najlepszych 10%", x="Indeks HPI",color="Kontynent")+theme(axis.title.x = element_text(size = 14),
                                                                                                                         axis.title.y = element_text(size = 14))

ggsave("Wykres_HPI/best_allto_HPI.png",width = 18, height = 9)