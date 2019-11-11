library(dplyr)
library(haven)
library(intsvy)
library(countrycode)

#Wczytanie ramki danych z wynikami

#dane <- haven::read_sas("~/Programowanie/TWD_01/cy6_ms_cmb_stu_qqq.sas7bdat")

#Wczytanie danych o współczynniku liczba uczniów na nauczyciela (pochodzenie danych OECD)
ratio_data <- read.csv("Wykres_student_teacher_ratio/Students_per_teacher.csv")

#Usrednienie wartosci wspolczynnika dla krajow z kilku lat badan
ratio <- group_by(ratio_data, LOCATION) %>%
  summarise(., avg_ratio = mean(Value, na.rm = TRUE))

#Wczytanie etykiet krajow
codes <- countrycode::codelist %>% select(., c(2, 5, 36))

#Obliczenie srednich wynikow dla danego kraju (pakiet intsvy)
result <- pisa2015.mean.pv(c("MATH", "SCIE", "READ"), by = "CNT", data = dane)
result <- select(result, CNT, Mean)

#Polaczenie ze wspolczynnikiem i dopisanie pelnych nazw panstw
final <- inner_join(result, ratio, by = c("CNT" = "LOCATION")) %>% left_join(., y = codes, by = c("CNT" = "iso3c"))

#Wykres
ggplot(final, aes(x = avg_ratio, y=Mean, colour=continent, label = country.name.en))+
  geom_point(size=4)+
  stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=avg_ratio,y=Mean))+
  ggtitle("Średni wynik względem liczby uczniów przypadających na nauczyciela")+
  labs(y = "Średni wynik w kraju", x="Liczba uczniów / liczba nauczycieli",color="Kontynent")+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))

#ggsave("Wykres_student_teacher_ratio/all_student_teacher_ratio_auto.png",width = 18, height = 9)
