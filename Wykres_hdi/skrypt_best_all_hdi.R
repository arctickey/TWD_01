# wczytywywanie danych 
hdi <- read.csv("data.csv",sep = ",",header = FALSE)
d <-  as.matrix(hdi[2,])
colnames(hdi) <- d
hdi <-  hdi[c(-1,-2),]
for (i in 3:30){
  hdi[,i] <- ifelse(hdi[,i]=="..",NA,hdi[,i])
}

hdi <- hdi[,c(1,2,28)]
# wczytywanie kodów krajów 
#install.packages("countrycode")
library(countrycode)

cod <- codelist
cod_final<- cod[,c(2,5,36)]
## połączenie kodu krajów z danymi hdi 
data_hdi <- merge(cod_final,hdi,by.x="country.name.en",by.y ="Country" )
colnames(data_hdi)[3] <- "CNT"
library(dplyr)
## wprowadzanie i poprawki ramik z danymi pissa 
#dane <- haven::read_sas("/home/samba/ficp/TWD/Projekt1/cy6_ms_cmb_stu_qqq.sas7bdat")
dane1 <- select(dane, CNT,CNTSTUID,PV1MATH:PV10SCIE)
dane1 <- best_in_all(5,dane1)
dane1 <- dane1[,c(1,33)]
# merge danych 
dane_plot <-  merge(dane1,data_hdi,by = "CNT")

data_plot1<- dane_plot %>% group_by(CNT,country.name.en,continent,`HDI Rank (2017)`,`2015`)%>% summarise(mean_all_by_CNT=mean(average_pv_all))
colnames(data_plot1)[5] <- "HDI"
library(ggplot2)
# wykresikk

ggplot(data_plot1,aes(x=HDI,y=mean_all_by_CNT,colour=continent))+geom_point(size=4)+stat_smooth(method = "lm",inherit.aes = FALSE,aes(x=HDI,y=mean_all_by_CNT))+
  ggtitle("Średnia od HDI")+labs(y = "Średni wynik w kraju dla najlepszych 10%", x="Indeks HDI",color="Kontynent")+theme(axis.title.x = element_text(size = 14),
                                                                                                                                           axis.title.y = element_text(size = 14))

ggsave("Wykres_hdi/best_allto_hdi.png",width = 18, height = 9)

