# wczytywywanie danych 
hdi <- read.csv2("/home/samba/borowskij/PPPD/TWD_01/Wykres_hdi/data.csv",sep = ",",header = FALSE)
d <-  as.matrix(hdi[2,])
colnames(hdi) <- d
hdi <-  hdi[c(-1,-2),]
for (i in 3:30){
  hdi[,i] <- ifelse(hdi[,i]=="..",NA,hdi[,i])
}

hdi <- hdi[,c(1,2,28)]
# wczytywanie kodów krajów 
install.packages("countrycode")
library(countrycode)
cod <- codelist
colnames(cod)
cod_final<- cod[,c(5,36)]

data_hdi <- merge(cod_final,hdi,by.x="country.name.en",by.y ="Country" )
colnames(data_hdi)[2] <- "CNT"

dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")