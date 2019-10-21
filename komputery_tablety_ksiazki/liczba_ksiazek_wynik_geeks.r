#Skrypt analizy wplywu dostepnosci komputerow na wyniki najlepszych uczniow

library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn: pytanie o liczbe komputerow oraz kolumny z wynikami
df <- select(dane, ST013Q01TA, PV1MATH:PV10SCIE) %>%
  filter(., !is.na(ST013Q01TA))

#Wyodrebnienie grup najlepszych 5%
df_math <- best_in_math(10, df)
df_read <- best_in_reading(10, df)
df_scie <- best_in_science(10, df)

#Oddzielnie dla kazdego przedmiotu liczymy srednie wyniki
#w grupach osob w zaleznosci od liczby komputerow w domu
means_math <- group_by(df_math, ST013Q01TA) %>%
  summarise(mean_1 = mean(PV1MATH),
            mean_2 = mean(PV2MATH),
            mean_3 = mean(PV3MATH),
            mean_4 = mean(PV4MATH),
            mean_5 = mean(PV5MATH),
            mean_6 = mean(PV6MATH),
            mean_7 = mean(PV7MATH),
            mean_8 = mean(PV8MATH),
            mean_9 = mean(PV9MATH),
            mean_10 = mean(PV10MATH))

means_math <- rowwise(means_math[, -1]) %>%
  mutate(Mean = mean(mean_1:mean_10)) %>%
  select(Mean)

means_read <- group_by(df_read, ST013Q01TA) %>%
  summarise(mean_1 = mean(PV1READ),
            mean_2 = mean(PV2READ),
            mean_3 = mean(PV3READ),
            mean_4 = mean(PV4READ),
            mean_5 = mean(PV5READ),
            mean_6 = mean(PV6READ),
            mean_7 = mean(PV7READ),
            mean_8 = mean(PV8READ),
            mean_9 = mean(PV9READ),
            mean_10 = mean(PV10READ))

means_read <- rowwise(means_read[, -1]) %>%
  mutate(Mean = mean(mean_1:mean_10)) %>%
  select(Mean)


means_scie <- group_by(df_scie, ST013Q01TA) %>%
  summarise(mean_1 = mean(PV1SCIE),
            mean_2 = mean(PV2SCIE),
            mean_3 = mean(PV3SCIE),
            mean_4 = mean(PV4SCIE),
            mean_5 = mean(PV5SCIE),
            mean_6 = mean(PV6SCIE),
            mean_7 = mean(PV7SCIE),
            mean_8 = mean(PV8SCIE),
            mean_9 = mean(PV9SCIE),
            mean_10 = mean(PV10SCIE))

means_scie <- rowwise(means_scie[, -1]) %>%
  mutate(Mean = mean(mean_1:mean_10)) %>%
  select(Mean)

result <- cbind(c("0-10","11-25","26-100","101-200",'201-500','500+'), means_math, means_read, means_scie)
colnames(result) <- c("Books_number","Mean_math", "Mean_reading", "Mean_science")

to_plot <- melt(result, id.vars = "Books_number")

ggplot(to_plot, aes(x= factor(Books_number, levels = c("0-10","11-25","26-100","101-200",'201-500','500+')), y=value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable) +
  ylab("Średni wynik") +
  ggtitle("Liczba książek w domu a wyniki najlepszych uczniów") +
  xlab("Liczba książek w domu")
