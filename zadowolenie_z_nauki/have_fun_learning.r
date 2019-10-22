#Skrypt analizy wplywu zadowolenia z nauki na wyniki uczniow

library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn: pytanie o liczbe komputerow oraz kolumny z wynikami
df <- select(dane, ST094Q01NA, PV1MATH:PV10SCIE) %>%
  filter(., !is.na(ST094Q01NA))

#Oddzielnie dla kazdego przedmiotu liczymy srednie wyniki
#w grupach osob w zaleznosci od udzielonej odpowiedzi na pytanie
means_math <- group_by(df, ST094Q01NA) %>%
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

means_read <- group_by(df, ST094Q01NA) %>%
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


means_scie <- group_by(df, ST094Q01NA) %>%
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

result <- cbind(c("strongly disagree","disagree","agree","strongly agree"), means_math, means_read, means_scie)
colnames(result) <- c("opinion","Mean_math", "Mean_reading", "Mean_science")

to_plot <- melt(result, id.vars = "opinion")

ggplot(to_plot, aes(x=reorder(opinion, value), y=value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable) +
  ylab("Średni wynik") +
  coord_cartesian(ylim=c(300,550)) +
  xlab("Zadowolenie podczas nauki") +
  ggtitle("I generally have fun when I am learning")
