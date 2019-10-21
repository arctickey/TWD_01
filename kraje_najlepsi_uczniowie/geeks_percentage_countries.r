library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
#Skrypt umozliwia uzyskanie informacji jakie kraje 
#maja najwyzszy odsetek osob z grupy 5% najlepszych 

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn
df <- select(dane, CNT, PV1MATH:PV10SCIE)

#Grupy najlepszych 5%
b_all <- best_in_all(5, df)
b_math <- best_in_math(5, df)
b_read <- best_in_reading(5, df)
b_scie <- best_in_science(5, df)

#Zliczamy ile osob z danego kraju bralo udzial w badaniu
count_all <- df %>%
  group_by(CNT) %>%
  count()
#Zliczamy osoby nalezace do 5% najlepszych wynikow wedlug krajow
count_b_all <- b_all %>%
  group_by(CNT) %>%
  count()

count_b_math <- b_math %>%
  group_by(CNT) %>%
  count()

count_b_read <- b_read %>%
  group_by(CNT) %>%
  count()

count_b_scie <- b_scie %>%
  group_by(CNT) %>%
  count()

#Laczymy ramki z pierwsza "count_all" oraz liczymy odsetek
count_b_all <- dplyr::inner_join(count_b_all, count_all, by = "CNT") %>%
  mutate(., per = round(((100*n.x)/n.y), digits = 2)) %>%
  select(., CNT, per_all = per)
count_b_math <- dplyr::inner_join(count_b_math, count_all, by = "CNT") %>%
  mutate(., per = round(((100*n.x)/n.y), digits = 2)) %>%
  select(., CNT, per_math = per)
count_b_read <- dplyr::inner_join(count_b_read, count_all, by = "CNT") %>%
  mutate(., per = round(((100*n.x)/n.y), digits = 2)) %>%
  select(., CNT, per_read = per)
count_b_scie <- dplyr::inner_join(count_b_scie, count_all, by = "CNT") %>%
  mutate(., per = round(((100*n.x)/n.y), digits = 2)) %>%
  select(., CNT, per_scie = per)

result <- inner_join(count_b_all, count_b_math, by = "CNT") %>%
  inner_join(., count_b_read, by = "CNT") %>%
  inner_join(., count_b_scie, by = "CNT")

result_to_plot1 <- melt(result, id = "CNT")

#Wykres dla wszystkich krajow
ggplot(result_to_plot1, aes(x = CNT, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Odsetek geniuszy wśród badanych") +
  xlab("Kraj") +
  ylab("Odsetek")

#Ograniczymy wykres do top 20 krajow wedlug odsetka najlepszych "per_all"
result_to_plot2 <- result[order(result$per_all, decreasing = TRUE),]
result_to_plot2 <-  result_to_plot2[1:20,]
result_to_plot2 <- melt(result_to_plot2, id = "CNT")

ggplot(result_to_plot2, aes(x = reorder(CNT, value), y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Kraje z najwyższym odsetkiem geniuszy wśród badanych") +
  xlab("Kraj") +
  ylab("Odsetek")
