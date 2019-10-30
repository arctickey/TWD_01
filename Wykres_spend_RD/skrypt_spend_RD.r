library(dplyr)
library(haven)
library(intsvy)
library(countrycode)

#Wczytanie ramki danych z wynikami

#dane <- haven::read_sas("~/Programowanie/TWD_01/cy6_ms_cmb_stu_qqq.sas7bdat")

#Wczytanie danych o współczynniku liczba uczniów na nauczyciela (pochodzenie danych OECD)
rd_data <- read.csv("Wykres_spend_RD/Spend_on_RD.csv")

#Usrednienie wartosci wspolczynnika dla krajow z kilku lat badan
rd_data <- group_by(rd_data, LOCATION) %>%
  summarise(., avg_rd = mean(Value, na.rm = TRUE))

#Wczytanie etykiet krajow
codes <- countrycode::codelist %>% select(., c(2, 5, 36))

#Obliczenie srednich wynikow dla danego kraju (pakiet intsvy)
result <- pisa2015.mean.pv(c("MATH", "SCIE", "READ"), by = "CNT", data = dane)
result <- select(result, CNT, Mean)

#Polaczenie ze wspolczynnikiem i dopisanie pelnych nazw panstw
final <- inner_join(result, rd_data, by = c("CNT" = "LOCATION")) %>% left_join(., y = codes, by = c("CNT" = "iso3c"))

ggplot(final, aes(x = avg_rd, y=Mean, colour=continent, label = country.name.en))+
  geom_point(size=4)+
  stat_smooth(method = "loess",inherit.aes = FALSE,aes(x=avg_rd,y=Mean))+
  ggtitle("Średni wynik względem wydatków na Reaserch & Development")+
  labs(y = "Średni wynik w kraju", x="% PKB wydawany na R&D",color="Kontynent")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

#ggsave("Wykres_spend_RD/all_student_spend_RD_auto.png",width = 18, height = 9)
