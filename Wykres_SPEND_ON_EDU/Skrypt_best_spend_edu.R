# wczytywanie ramki danych 
EDU <- read.csv("EDU.csv")
EDU_2015 <- EDU[2,] 
# dane z 2015 r 
for (i in 3:276){
  if (EDU[i,6]==2015){
    EDU_2015 <- rbind(EDU_2015,EDU[i,])}
  
}
EDU_2015 <- EDU_2015[,c(1,7)]
## wprowadzanie i poprawki ramik z danymi pissa 
dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
dane <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
dane <- best_in_all(10,dane)## ZMIEN W CELU ZMIANY DANYCH z pissa
dane <- dane[,c(1,33)]

# dane o krajach 
cod <- codelist
cod_final<- cod[,c(2,5,36)]
EDU_2015<- merge(EDU_2015,cod_final,by.x ="LOCATION",by.y="iso3c")
EDU_2015 <- na.omit(EDU_2015)

## łaczenie z danymi z pissa 
dane_plot3 <- merge(dane,EDU_2015,by.x="CNT",by.y="LOCATION")
colnames(dane_plot3)[3] <- "SPEND_EDU"
## grupowanie po krajach 
data_plot3<- dane_plot3 %>% group_by(CNT,SPEND_EDU,continent,country.name.en) %>% summarise(mean_all_by_CNT=mean(average_pv_all))

## Wykres
ggplot(data_plot3,aes(x=SPEND_EDU,y=mean_all_by_CNT,colour=continent))+geom_point()+stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=SPEND_EDU,y=mean_all_by_CNT))+
  ggtitle("SREDNIA_10%_DO_WYDATKÓW_NA_EDUKACJE")

ggsave("Wykres_SPEND_ON_EDU/best_allto_HPI.png",width = 18, height = 9)