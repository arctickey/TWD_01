library(dplyr)
library(haven)
library(intsvy)
library(countrycode)

#Wczytanie ramki danych z wynikami

#dane <- haven::read_sas("~/Programowanie/TWD_01/cy6_ms_cmb_stu_qqq.sas7bdat")


#Wczytanie danych ranking BLI
bli_data <- read.csv("Wykres_BLI/Ranking_BLI.csv")

#Wczytanie etykiet krajow
codes <- countrycode::codelist %>% select(., c(2, 5, 36))

#Obliczenie srednich wynikow dla danego kraju (pakiet intsvy)
result <- pisa2015.mean.pv(c("MATH", "SCIE", "READ"), by = "CNT", data = dane)
result <- select(result, CNT, Mean)

#Polaczenie z danymi rankingu BLI 
final <- left_join(bli_data, y = codes, by = c("CNT" = "country.name.en")) %>% 
  inner_join(., result, by = c("iso3c" = "CNT"))

#Wykres
ggplot(final, aes(x = Rank, y=Mean, colour=continent, label = CNT))+
  geom_point(size=4)+
  stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=Rank,y=Mean))+
  ggtitle("Średni wynik względem Better Life Index")+
  labs(y = "Średni wynik w kraju", x="Pozycja w rankingu BLI",color="Kontynent")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  geom_label_repel()
  

ggsave("Wykres_BLI/mean_bli.png",width = 18, height = 9)

