library(dplyr)
library(reshape2)
library(ggplot2)

#Skrypt sluzy do analizy jak rozkladaja sie osoby z najwyzszymi wynikami
#w zaleznosci od wyksztalcenia rodzicow

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

#Ramka danych osob, ktore podaly wyksztalcenie rodzicow
podajacy_wyksztalcenie_rodzicow <- rbind(doctoral,bachelor_master,post_secondary,secondary_lower)


#Za pomoca skryptow z folderu 'skrypty_naklepsze_wyniki', przygotowujemy grupe osob
#ktora osiagnela 5% najlepszych wynikow
best_all <- best_in_all(5, podajacy_wyksztalcenie_rodzicow)
best_math <- best_in_math(5, podajacy_wyksztalcenie_rodzicow)
best_reading <- best_in_reading(5, podajacy_wyksztalcenie_rodzicow)
best_science <- best_in_science(5, podajacy_wyksztalcenie_rodzicow)

#Obliczamy ile procentowo osob z danych grup wyksztalcenia rodzicow
#jest rowniez w grupie 5% najlepszych wynikow
#ALL
best_all_doctoral <- filter(best_all, ST006Q01TA==1 | ST008Q01TA==1) %>%
  count()/nrow(doctoral)
best_all_bachelor_master <- filter(best_all, (ST006Q02TA==1 | ST008Q02TA==1) & ST006Q01TA!=1 & ST008Q01TA!=1) %>%
  count()/nrow(bachelor_master)
best_all_post_secondary <- filter(best_all, (ST006Q03TA==1 | ST008Q03TA==1 | ST006Q04TA==1 | ST008Q04TA==1) &
                                    ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1) %>%
  count()/nrow(post_secondary)
best_all_secondary_lower <- filter(best_all, ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1 &
                                     ST006Q03TA!=1 & ST008Q03TA!=1 & ST006Q04TA!=1 & ST008Q04TA!=1) %>%
  count()/nrow(secondary_lower)

best_all_geeks <- c(best_all_doctoral, best_all_bachelor_master, best_all_post_secondary, best_all_secondary_lower)

#MATH
best_math_doctoral <- filter(best_math, ST006Q01TA==1 | ST008Q01TA==1) %>%
  count()/nrow(doctoral)
best_math_bachelor_master <- filter(best_math, (ST006Q02TA==1 | ST008Q02TA==1) & ST006Q01TA!=1 & ST008Q01TA!=1) %>%
  count()/nrow(bachelor_master)
best_math_post_secondary <- filter(best_math, (ST006Q03TA==1 | ST008Q03TA==1 | ST006Q04TA==1 | ST008Q04TA==1) &
                                    ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1) %>%
  count()/nrow(post_secondary)
best_math_secondary_lower <- filter(best_math, ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1 &
                                     ST006Q03TA!=1 & ST008Q03TA!=1 & ST006Q04TA!=1 & ST008Q04TA!=1) %>%
  count()/nrow(secondary_lower)

best_math_geeks <- c(best_math_doctoral, best_math_bachelor_master, best_math_post_secondary, best_math_secondary_lower)

#READING
best_reading_doctoral <- filter(best_reading, ST006Q01TA==1 | ST008Q01TA==1) %>%
  count()/nrow(doctoral)
best_reading_bachelor_master <- filter(best_reading, (ST006Q02TA==1 | ST008Q02TA==1) & ST006Q01TA!=1 & ST008Q01TA!=1) %>%
  count()/nrow(bachelor_master)
best_reading_post_secondary <- filter(best_reading, (ST006Q03TA==1 | ST008Q03TA==1 | ST006Q04TA==1 | ST008Q04TA==1) &
                                     ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1) %>%
  count()/nrow(post_secondary)
best_reading_secondary_lower <- filter(best_reading, ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1 &
                                      ST006Q03TA!=1 & ST008Q03TA!=1 & ST006Q04TA!=1 & ST008Q04TA!=1) %>%
  count()/nrow(secondary_lower)

best_reading_geeks <- c(best_reading_doctoral, best_reading_bachelor_master, best_reading_post_secondary, best_reading_secondary_lower)

#SCIENCE
best_science_doctoral <- filter(best_science, ST006Q01TA==1 | ST008Q01TA==1) %>%
  count()/nrow(doctoral)
best_science_bachelor_master <- filter(best_science, (ST006Q02TA==1 | ST008Q02TA==1) & ST006Q01TA!=1 & ST008Q01TA!=1) %>%
  count()/nrow(bachelor_master)
best_science_post_secondary <- filter(best_science, (ST006Q03TA==1 | ST008Q03TA==1 | ST006Q04TA==1 | ST008Q04TA==1) &
                                        ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1) %>%
  count()/nrow(post_secondary)
best_science_secondary_lower <- filter(best_science, ST006Q01TA!=1 & ST008Q01TA!=1 & ST006Q02TA!=1 & ST008Q02TA!=1 &
                                         ST006Q03TA!=1 & ST008Q03TA!=1 & ST006Q04TA!=1 & ST008Q04TA!=1) %>%
  count()/nrow(secondary_lower)

best_science_geeks <- c(best_science_doctoral, best_science_bachelor_master, best_science_post_secondary, best_science_secondary_lower)

#Results together
result_geeks_1 <- rbind.data.frame(as.vector(best_all_geeks), as.vector(best_math_geeks),
                                 as.vector(best_reading_geeks), as.vector(best_science_geeks))

result_geeks <- 100*result_geeks_1
result_geeks <- sapply(result_geeks, FUN = function(x){round(x, digits = 2)})

colnames(result_geeks) <- c("Doctoral", "Bachelor\nmaster", "Post\nsecondary", "Secondary\nor lower")
row.names(result_geeks) <- c("all", "math", "reading", "science")

#Wykres
to_plot <- melt(result_geeks, id=row.names(result_geeks))
#Odsetek osob z poszczegolnych grup wuksztalcenia nalezacych do top 5% wynikow
plot1 <- ggplot(to_plot, aes(x = Var2, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Var1) +
  ggtitle("Odsetek osób z danej grupy wykształcenia rodziców\nbędących w grupie 5% najlepszych wyników") +
  xlab("Wykształcenie rodziców") +
  ylab("Odsetek ")

#Procentowy rozklad osob z grupy top 5% na grupy wyksztalcenia
result_geeks_1 <- rbind.data.frame(as.vector(best_all_geeks), as.vector(best_math_geeks),
                                   as.vector(best_reading_geeks), as.vector(best_science_geeks))
result_geeks_1[,1] <- result_geeks_1[,1]*nrow(doctoral)
result_geeks_1[,2] <- result_geeks_1[,2]*nrow(bachelor_master)
result_geeks_1[,3] <- result_geeks_1[,3]*nrow(post_secondary)
result_geeks_1[,4] <- result_geeks_1[,4]*nrow(secondary_lower)

result_geeks_1 <- result_geeks_1/nrow(best_all)*100
row.names(result_geeks_1) <- c("all", "math", "reading", "science")
colnames(result_geeks_1) <- c("Doctoral", "Bachelor\nmaster", "Post\nsecondary", "Secondary\nor lower")
result_geeks_1 <- as.matrix(result_geeks_1)

#Wykres
to_plot_2 <- melt(result_geeks_1, id.vars = row.names(result_geeks_1))
#Odsetek osob z poszczegolnych grup wyksztalcenia nalezacych do top 5% wynikow
plot2 <- ggplot(to_plot_2, aes(x = reorder(Var2, value), y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Var1) +
  ggtitle("Podział grupy geniuszów względem wykształcenia rodziców") +
  xlab("Wykształcenie rodziców") +
  ylab("Odsetek")
  
plot2

ggsave(plot = plot2, "wykresy_pytania_kwestionariusz/rozklad_geeks_na_wyksztalcenie.png",width = 18, height = 9)
