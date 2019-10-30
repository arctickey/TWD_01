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

result_all <- group_by(df, ST094Q01NA) %>%
  count()
result_all$n <- 100*result_all$n*(1/nrow(df))

df_best <- best_in_all(10, df)

result_best <- group_by(df_best, ST094Q01NA) %>%
  count() 
result_best$n <- 100*result_best$n*(1/nrow(df_best))

to_plot <- data.frame(result_all, result_best) %>%
  select(ST094Q01NA, n, n.1)
colnames(to_plot) <- c("ST094Q01NA", "Wszyscy", "Geniusze")

to_plot <- melt(to_plot, id.vars = "ST094Q01NA")

ggplot(to_plot, aes(x = factor(ST094Q01NA), y=value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  xlab("Zgodność ze stwierdzeniem") +
  ylab("Odsetek ankietowanych") +
  theme(legend.title = element_blank()) +
  ggtitle("\"I generally have fun when I am learning\" odpowiedzi geniuszy względem całości") +
  scale_x_discrete(labels = c("strongly disagree","disagree","agree","strongly agree")) +
  ggsave("wykresy_pytania_kwestionariusz/have_fun_odsetek.png",width = 18, height = 9)
  