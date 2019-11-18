#Skrypt do analizy srednich wynikow w zaleznosci od wyksztalcenia rodzicow
#uwzgledniajac podzial na grupy rozwoju

#Funkcja do podzialu krajow na grupy
podziel_grupy_PKB <- function(frame){
  library(dplyr)
  #Podajemy data frame wynikow PISA, otrzynujemy dodatkowa kolumne grupujaca na kraje
  #rozwijajace sie i rozwiniete, wzgledem relacji PKB per capita do sredniech wynikow testu
  #(czyli te ktore zaobserwowalismy na wykresie nr1)
  #Grupy: 1 - rozwijajace sie, 2 - rozwiniete
  
  slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")
  wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA","JPN")
  
  wyniki_slabe <- filter(frame, CNT %in% slabo_rozwiniete)
  wyniki_wysoko <- filter(frame, CNT %in% wysoko_rozwiniete)
  
  wyniki_slabe <- mutate(wyniki_slabe, grupa_rozwoju = "1")
  wyniki_wysoko <- mutate(wyniki_wysoko, grupa_rozwoju = "2")
  
  wyniki_razem <- rbind(wyniki_slabe, wyniki_wysoko)
  
  return(wyniki_razem)
}

#Przygotowujemy dane
library(haven)
dane <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")
library(dplyr)
df <- filter(dane, !is.na(HISCED))
df <- podziel_grupy_PKB(df)

#Zgrupowanie 6 poziomow edukacji do 3, nawiazujac do wykresu rozkladu procentowego 
#wyksztalcenia rodzicow, ktory przedstawiamy na plakacie
df <- mutate(df, parents = 0)
df <- mutate(df, parents = replace(parents, HISCED %in% c(2,3,4), 1))
df <- mutate(df, parents = replace(parents, HISCED %in% c(5,6), 2))

#Obliczenie srednich
library(intsvy)
avg_gr1 <- filter(df, grupa_rozwoju == 1)
avg_gr1 <- pisa2015.mean.pv("MATH", by = "parents", data = avg_gr1)

avg_gr2 <- filter(df, grupa_rozwoju == 2)
avg_gr2 <- pisa2015.mean.pv("MATH", by = "parents", data = avg_gr2)

result <- as.data.frame(cbind(avg_gr1$parents, avg_gr1$Mean, avg_gr2$Mean))
colnames(result) <- c("wyksztalcenie_rodzicow", "srednia_gr1", "srednia_gr2")

#Wykres
library(ggplot2)
library(tidyr)

p <- ggplot(data = result %>% gather(wyksztalcenie, srednia, -wyksztalcenie_rodzicow), 
       aes(x = wyksztalcenie_rodzicow, y = srednia, colour = wyksztalcenie))+
  geom_point(size = 5)+
  coord_flip()+
  ylab("Średni wynik")+
  xlab("Wykształcenie rodziców")+
  scale_color_manual("Grupa rozwoju", labels = c("Rozwijające się", "Wysoko rozwinięte"), values = c("#fb5515", "#66c2a5"))+
  scale_x_discrete(limits = c("Podstawowe", "Średnie", "Wyższe"))+
  ggtitle("Średni wynik w grupach rozwoju względem wykształcenia rodziców uczniów")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(360, 520))+
  theme_light()+
  theme(legend.position = "top")


#Zapis wykresu
# library(svglite)
# svglite("Nierownosci/wyksztalcenie_rodzicow_srednie.svg", width = 10, height = 6)
# p
# dev.off()

