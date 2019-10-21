#Skrypt sluzy do zliczenia osob z bardzo dobrym wynikiem wzgledem krajow
library(haven)
library(dplyr)
library(ggplot2)

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn
df <- select(dane, CNT, PV1MATH:PV10SCIE)

#Uzycie funkcji ze skryptu "best_in_all.r"
result <- best_in_all(5, df)

#Ograniczam do top10 krajow
top10 <- result %>%
  group_by(CNT) %>%
  count() %>%
  arrange(., -n)

#Szybki wykres
ggplot(top10[1:10,], aes(x=reorder(CNT, n), y=n)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  xlab("Country") +
  ylab("Count of geeks")