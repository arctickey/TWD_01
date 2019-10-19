#Wczytanie glownej ramki danych

library(haven)
library(tidyverse)
library(reshape2)
data<- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn
df <- select(data, CNT,CNTSTUID,PV1MATH:PV10SCIE)

#Uzycie funkcji ze skryptu "best_in_all.r"
result_math <- best_in_math(10, df)
result_scie <- best_in_science(10, df)
result_read <- best_in_reading(10, df)

#Przygotowanie do merge'a z cala tabela
result_math <- as.data.frame(result_math$CNTSTUID)
colnames(result_math) <- c("CNTSTUID")
result_scie <- as.data.frame(result_scie$CNTSTUID)
colnames(result_scie) <- c("CNTSTUID")
result_read <- as.data.frame(result_read$CNTSTUID)
colnames(result_read) <- c("CNTSTUID")

#bierzemy pełne dane dla tego najlepszego procenta studentow,
#aby sprawdzić które zmienne koreluja z dobrymi wynikamiv
dane_math <- left_join(result_math,data,by="CNTSTUID")
dane_read <- left_join(result_read,data,by="CNTSTUID")
dane_scie <- left_join(result_scie,data,by="CNTSTUID")

#liczenie srednie dla kazdego z przedmiotow z osobna i tworzenie nowej kolumny
#average_scores <- function(df){
  df_math <- select(df,PV1MATH:PV10MATH)
  df_scie <- select(df,PV1SCIE:PV10SCIE)
  df_read <- select(df,PV1READ:PV10READ)
  math_avr <- as.numeric(rowMeans(df_math))
  read_avr <- as.numeric(rowMeans(df_read))
  scie_avr <- as.numeric(rowMeans(df_scie))
  all_avr <- as.numeric(rowMeans(cbind(math_avr,read_avr,scie_avr)))
  df <- select(df, -c(PV1MATH:PV10SCIE))
  df$MATH_AVR <- math_avr
  df$SCIE_AVR <- scie_avr
  df$READ_AVR <- read_avr
  df$ALL_AVR <- all_avr
  return(df)
}

average_scores2_math <- function(df){
  country_means_math <- select(df,CNT,PV1MATH:PV10MATH) %>% group_by(CNT) %>% 
    summarise(mean1 = mean(PV1MATH),
    mean2 = mean(PV2MATH), mean3 = mean(PV3MATH),mean4 = mean(PV4MATH),mean5 = mean(PV5MATH),mean6 = mean(PV6MATH),
    mean7 = mean(PV7MATH), mean8 = mean(PV8MATH), mean9 = mean(PV9MATH), mean10 = mean(PV10MATH))
  country_means_math <-rowMeans(select(country_means_math,-c(CNT)))
  nazwy <- unique(df$CNT)
  x <- data.frame(CNT=nazwy,mean_math=country_means_math)
  df <- left_join(x,df,by="CNT")
    }

average_scores2_read <- function(df){
  country_means_read <- select(df,CNT,PV1READ:PV10READ) %>% group_by(CNT) %>% 
    summarise(mean1 = mean(PV1READ),
              mean2 = mean(PV2READ), mean3 = mean(PV3READ),mean4 = mean(PV4READ),mean5 = mean(PV5READ),mean6 = mean(PV6READ),
              mean7 = mean(PV7READ), mean8 = mean(PV8READ), mean9 = mean(PV9READ), mean10 = mean(PV10READ))
  country_means_read <-rowMeans(select(country_means_read,-c(CNT)))
  nazwy <- unique(df$CNT)
  x <- data.frame(CNT=nazwy,mean_read=country_means_read)
  df <- left_join(x,df,by="CNT")
}


average_scores2_scie <- function(df){
  country_means_scie <- select(df,CNT,PV1SCIE:PV10SCIE) %>% group_by(CNT) %>% 
    summarise(mean1 = mean(PV1SCIE),
              mean2 = mean(PV2SCIE), mean3 = mean(PV3SCIE),mean4 = mean(PV4SCIE),mean5 = mean(PV5SCIE),mean6 = mean(PV6SCIE),
              mean7 = mean(PV7SCIE), mean8 = mean(PV8SCIE), mean9 = mean(PV9SCIE), mean10 = mean(PV10SCIE))
  country_means_scie <-rowMeans(select(country_means_scie,-c(CNT)))
  nazwy <- unique(df$CNT)
  x <- data.frame(CNT=nazwy,mean_scie=country_means_scie)
  df <- left_join(x,df,by="CNT")
}




dane_math1 <- average_scores2_math(dane_math)
dane_pom_math1 <- select(dane_math1, -c(mean_math)) %>% select_if(is.numeric)
corr_matma1 <- cor(dane_math1$mean_math,dane_pom_math1,use = "pairwise.complete.obs")
corr_matma1 <- as.data.frame(x=corr_matma1,colnames(dane_pom_math1))
corr_matma1 <- melt(sort(corr_matma1,decreasing = TRUE))

dane_read1 <- average_scores2_read(dane_read)
dane_pom_read1 <- select(dane_read1, -c(mean_read)) %>% select_if(is.numeric)
corr_read1 <- cor(dane_read1$mean_read,dane_pom_read1,use = "pairwise.complete.obs")
corr_read1 <- as.data.frame(x=corr_read1,colnames(dane_pom_read1))
corr_read1 <- melt(sort(corr_read1,decreasing = TRUE))

dane_scie1 <- average_scores2_scie(dane_scie)
dane_pom_scie1 <- select(dane_scie1, -c(mean_scie)) %>% select_if(is.numeric)
corr_scie1 <- cor(dane_scie1$mean_scie,dane_pom_scie1,use = "pairwise.complete.obs")
corr_scie1 <- as.data.frame(x=corr_scie1,colnames(dane_pom_scie1))
corr_scie1 <- melt(sort(corr_scie1,decreasing = TRUE))







write.csv(corr_matma1,file="Corr_math1.csv")
write.csv(corr_scie1,file="Corr_scie1.csv")
write.csv(corr_read1,file="Corr_read1.csv")











dane_math <- average_scores(dane_math)
dane_math <- select_if(dane_math,is.numeric)

dane_read <- average_scores(dane_read)
dane_read<- select_if(dane_read,is.numeric)

dane_all<- average_scores(dane_all)
dane_all <- select_if(dane_all,is.numeric)

dane_scie <- average_scores(dane_scie)
dane_scie <- select_if(dane_scie,is.numeric)


#dane pomocnicze do zrobienia korelacji
dane_pom_read<- select(dane_read, -c(MATH_AVR,READ_AVR,SCIE_AVR,ALL_AVR))
dane_pom_math <- select(dane_math, -c(MATH_AVR,READ_AVR,SCIE_AVR,ALL_AVR))
dane_pom_all <- select(dane_all, -c(MATH_AVR,READ_AVR,SCIE_AVR,ALL_AVR))
dane_pom_scie <- select(dane_scie, -c(MATH_AVR,READ_AVR,SCIE_AVR,ALL_AVR))


#korelacje z osobna dla kazdej z najlepszej grup w kazdym przedmiocie
corr_matma <- cor(dane_math$MATH_AVR,dane_pom_math,use = "pairwise.complete.obs")
corr_matma <- as.data.frame(x=corr_matma,colnames(dane_pom_math))
corr_matma <- melt(sort(corr_matma,decreasing = TRUE))


corr_read <- cor(dane_read$READ_AVR,dane_pom_read,use = "pairwise.complete.obs")
corr_read <- as.data.frame(x=corr_read,colnames(dane_pom_read))
corr_read <- melt(sort(corr_read,decreasing = TRUE))


corr_scie <- cor(dane_scie$SCIE_AVR,dane_pom_scie,use = "pairwise.complete.obs")
corr_scie <- as.data.frame(x=corr_scie,colnames(dane_pom_scie))
corr_scie <- melt(sort(corr_scie,decreasing = TRUE))

corr_all <- cor(dane_all$ALL_AVR,dane_pom_all,use = "pairwise.complete.obs")
corr_all <- as.data.frame(x=corr_all,colnames(dane_pom_all))
corr_all <- melt(sort(corr_all,decreasing = TRUE))

#zapisanie do csv, aby łatwiej było potem z tego korzystać
write.csv(corr_math,file="Corr_math.csv")
write.csv(corr_scie,file="Corr_scie.csv")
write.csv(corr_all,file="Corr_all.csv")
write.csv(corr_read,file="Corr_read.csv")

