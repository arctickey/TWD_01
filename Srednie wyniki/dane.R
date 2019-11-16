library(haven)
library(reshape2)
library(ggplot2)
library(dplyr)
library(intsvy)


data <- read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")

slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")


wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA","JPN")
kolumny_st012 <- c( "ST012Q02TA",
                    "ST012Q03TA",
                    "ST012Q05NA",
                    "ST012Q06NA",
                    "ST012Q07NA",
                    "ST012Q09NA")



dane_wr <- data[data$CNT%in%wysoko_rozwiniete,] 

dane_sr <- data[data$CNT%in%slabo_rozwiniete,] 
wynik <-as.data.frame(matrix(nrow=52,ncol = 4))
colnames(wynik) <- c("Pytanie","grupa rozwoju ", "kraj ", "ma nie ma ")
remove(data)

dane_sr$ST011Q04TA
kolumny_st012 <- c( "ST012Q02TA",
  "ST012Q03TA",
  "ST012Q05NA",
  "ST012Q06NA",
  "ST012Q07NA",
  "ST012Q09NA")



podpisy <- c("Auto","Łazienka","Połączenie z Internetem","Komputer","Tablet","Instrument muzyczny")

podpisy <- rep(podpisy,each=4)

podpisy <- c(podpisy,rep(NA,28))
wynik$Pytanie <- podpisy
licznik <- 1 
for (i in kolumny_st012) {
 indeksy_nie_ma <-  (!is.na(dane_wr[,i]) & (dane_wr[,i])==1)
 a <- dane_wr[indeksy_nie_ma,] 
 a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
 a <-  mean(a$Mean)
 gc(verbose = FALSE)
 wynik[licznik,2] <- "Wr"
 wynik[licznik,3] <- a
 wynik[licznik,4] <- "nie ma "
 licznik <-  licznik+1 
 indeksy_ma <- !indeksy_nie_ma
 a <-dane_wr[indeksy_ma,]
 a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
 a <-  mean(a$Mean)
 gc(verbose = FALSE)
 wynik[licznik,2] <- "Wr"
 wynik[licznik,3] <- a
 wynik[licznik,4] <- "ma "
 licznik <-  licznik+1 
 ##########
 indeksy_nie_ma <-  (!is.na(dane_sr[,i]) & (dane_sr[,i])==1)
 a <- dane_sr[indeksy_nie_ma,] 
 a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
 a <-  mean(a$Mean)
 gc(verbose = FALSE)
 wynik[licznik,2] <- "Sr"
 wynik[licznik,3] <- a
 wynik[licznik,4] <- "nie ma "
 licznik <-  licznik+1 
 indeksy_ma <- !indeksy_nie_ma
 a <-dane_sr[indeksy_ma,]
 a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
 a <-  mean(a$Mean)
 gc(verbose = FALSE)
 wynik[licznik,2] <- "Sr"
 wynik[licznik,3] <- a
 wynik[licznik,4] <- "ma "
 licznik <-  licznik+1 
}

kolumny_st011 <- c(#"ST011Q04TA",
  "ST011Q01TA",
  "ST011Q12TA",
  "ST011Q06TA",
  "ST011Q03TA",
  "ST011Q02TA",
  #"ST011Q08TA",
  #"ST011Q16NA",
  #"ST011Q10TA",
  #"ST011Q07TA",
  "ST011Q05TA",
  "ST011Q09TA")



podpisy2 <- c("Biurko do nauki","Słownik","Połączenie z Internetem","Ciche miejsce do nauki","Własny pokój",
              "Oprogramowanie edukacyjne","Dzieło sztuki")

podpisy2 <-  c(podpisy[1:24],rep(podpisy2,each=4))
wynik$Pytanie <- podpisy2

for (i in kolumny_st011) {
  indeksy_nie_ma <-  (!is.na(dane_wr[,i]) & (dane_wr[,i])==2)
  a <- dane_wr[indeksy_nie_ma,] 
  a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
  a <-  mean(a$Mean)
  gc(verbose = FALSE)
  wynik[licznik,2] <- "Wr"
  wynik[licznik,3] <- a
  wynik[licznik,4] <- "nie ma "
  licznik <-  licznik+1 
  indeksy_ma <- !indeksy_nie_ma
  a <-dane_wr[indeksy_ma,]
  a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
  a <-  mean(a$Mean)
  gc(verbose = FALSE)
  wynik[licznik,2] <- "Wr"
  wynik[licznik,3] <- a
  wynik[licznik,4] <- "ma "
  licznik <-  licznik+1 
  ##########
  indeksy_nie_ma <-  (!is.na(dane_sr[,i]) & (dane_sr[,i])==2)
  a <- dane_sr[indeksy_nie_ma,] 
  a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
  a <-  mean(a$Mean)
  gc(verbose = FALSE)
  wynik[licznik,2] <- "Sr"
  wynik[licznik,3] <- a
  wynik[licznik,4] <- "nie ma "
  licznik <-  licznik+1 
  indeksy_ma <- !indeksy_nie_ma
  a <-dane_sr[indeksy_ma,]
  a <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = a)
  a <-  mean(a$Mean)
  gc(verbose = FALSE)
  wynik[licznik,2] <- "Sr"
  wynik[licznik,3] <- a
  wynik[licznik,4] <- "ma "
  licznik <-  licznik+1 
}

write.csv(wynik,"Srednie wyniki/dana.csv")
  
