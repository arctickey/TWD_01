#Skrypt analizy wplywu zadowolenia z nauki na wyniki uczniow

library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Wybor kolumn: pytanie o liczbe komputerow oraz kolumny z wynikami
df <- select(dane, ST094Q04NA, PV1MATH:PV10SCIE) %>%
  filter(., !is.na(ST094Q04NA))

result_all <- group_by(df, ST094Q04NA) %>%
  count()
result_all$n <- 100*result_all$n*(1/nrow(df))

df_best <- best_in_all(10, df)

result_best <- group_by(df_best, ST094Q04NA) %>%
  count() 
result_best$n <- 100*result_best$n*(1/nrow(df_best))

to_plot <- data.frame(result_all, result_best) %>%
  select(ST094Q04NA, n, n.1)
colnames(to_plot) <- c("ST094Q04NA", "Wszyscy", "Geniusze")

to_plot <- melt(to_plot, id.vars = "ST094Q04NA")

ggplot(to_plot, aes(x = factor(ST094Q04NA), y=value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  xlab("Odpowiedź na pytanie") +
  ylab("Odsetek ankietowanych") +
  theme(legend.title = element_blank()) +
  ggtitle("\"I enjoy acquiring new knowledge\" odpowiedzi geniuszy względem całości") +
  scale_x_discrete(labels = c("strongly disagree","disagree","agree","strongly agree")) +
  ggsave("wykresy_pytania_kwestionariusz/enjoy_new_knowledge_odsetek.png",width = 18, height = 9)

