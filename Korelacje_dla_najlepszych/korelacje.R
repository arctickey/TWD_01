#Wczytanie glownej ramki danych
data<- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn
df <- select(data, CNT,CNTSTUID,PV1MATH:PV10SCIE)

#Uzycie funkcji ze skryptu "best_in_all.r"
result_math <- best_in_math(10, df)
result_all <- best_in_all(10, df)
result_scie <- best_in_science(10, df)
result_read <- best_in_reading(10, df)

#Przygotowanie do merge'a z cala tabela
result_math <- as.data.frame(result_math$CNTSTUID)
colnames(result_math) <- c("CNTSTUID")
result_scie <- as.data.frame(result_scie$CNTSTUID)
colnames(result_scie) <- c("CNTSTUID")
result_all <- as.data.frame(result_all$CNTSTUID)
colnames(result_all) <- c("CNTSTUID")
result_read <- as.data.frame(result_read$CNTSTUID)
colnames(result_read) <- c("CNTSTUID")

#bierzemy pełne dane dla tego najlepszego procenta studentow,
#aby sprawdzić które zmienne koreluja z dobrymi wynikamiv
dane_math <- left_join(result_math,data,by="CNTSTUID")
dane_read <- left_join(result_read,data,by="CNTSTUID")
dane_all <- left_join(result_all,data,by="CNTSTUID")
dane_scie <- left_join(result_scie,data,by="CNTSTUID")

#liczenie srednie dla kazdego z przedmiotow z osobna i tworzenie nowej kolumny
average_scores <- function(df){
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

