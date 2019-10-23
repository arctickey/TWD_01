library(dplyr)
library(ggplot2)

#Skrypt sluzy do analizy wplywu wyksztalcenia rodzicow na wyniki ucznia w tescie PISA
#Zbadamy sredni wynik ucznia w zaleznosci od tego jakie wyksztalcenie posiadali jego rodzice

#Wczytanie glownej ramki danych
dane <- haven::read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Przygotowanie danych, pozostawione kolumny:
#odpowiedzi na pytania dot. wyksztalcenia rodzicow, wyniki testu
df <- select(dane, ST006Q01TA:ST006Q04TA, ST008Q01TA:ST008Q04TA, PV1MATH:PV10SCIE) 

#Wyodrebnienie grup osob, dzielimy je na grupy:
#doctoral; bachelor_master; post_secondary; secondary_lower
#biezemy pod uwage rodzica, ktory posiada wyzsze wyksztalcenie
#przykladowo: matka - post_secondary, ojceic - bachelor -> uczen zostanie przypisany do grupy bachelor_master

doctoral <- filter(df, ST006Q01TA==1 | ST008Q01TA==1)

bachelor_master <- filter(df, (ST006Q02TA==1 | ST008Q02TA==1) & ST006Q01TA!=1 & ST008Q01TA!=1)

post_secondary <- filter(df, (ST006Q03TA==1 | ST008Q03TA==1 | ST006Q04TA==1 | ST008Q04TA==1) &
                           ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1)

secondary_lower <- filter(df, ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1 &
                            ST006Q03TA!=1 & ST008Q03TA!=1 & ST006Q04TA!=1 & ST008Q04TA!=1)

#Wyliczenie srednich wynikow w kazdej z grup
math_mean <- function(df){
  mean(sapply(df[,9:18], mean))
}

read_mean <- function(df){
  mean(sapply(df[,19:28], mean))
}
scie_mean <- function(df){
  mean(sapply(df[,29:38], mean))
}

all_mean <- function(df){
  mean(sapply(df[,9:38], mean))
}

result_df <- function(df){
  v <- c(math_mean(df), read_mean(df), scie_mean(df), all_mean(df))
  d <- rbind(c("math", "reading", "science", "all"), v)
  return(t(d))
}

result_doctoral <- cbind(rep("Doctoral", times = 4), result_df(doctoral))
result_bach_mast <- cbind(rep("Bachelor\nMaster", times = 4), result_df(bachelor_master))
result_post_secondary <- cbind(rep("Post\nsecondary", times = 4), result_df(post_secondary))
result_secondary_lower <- cbind(rep("Secondary\n or lower", times = 4), result_df(secondary_lower))

result_together <- rbind.data.frame(result_doctoral, result_bach_mast, result_post_secondary, result_secondary_lower, stringsAsFactors = FALSE)
colnames(result_together) <- c("Title", "Test_part", "Average_score")
result_together[,3] <- as.numeric(result_together[,3])

ggplot(result_together, aes(x = reorder(Title, Average_score), y = Average_score)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Test_part, nrow = 2) +
  coord_cartesian(ylim=c(400,550)) +
  ggtitle("Średni wynik względem wykształcenia rodziców") +
  xlab("Wykształcenie rodziców") +
  ylab("Średni wynik") +
  ggsave("wykresy_pytania_kwestionariusz/wyksztalcenie_rodzicow.png",width = 18, height = 9)

  