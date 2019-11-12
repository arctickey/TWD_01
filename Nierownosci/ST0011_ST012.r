#Funkcja do kalkulacji roznic pomiedzy grupami krajow rozwinietych i rozwijajacych sie
#Wykres
library(haven)
library(reshape2)
library(ggplot2)
#dane <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")

#Funkcja wykorszystywana do podzielenia krajow na grupy
podziel_grupy_PKB <- function(frame){
  library(dplyr)
  #Podajemy data frame wynikow PISA, otrzynujemy dodatkowa kolumne grupujaca na kraje
  #slabo, srednio i wysoko rozwiniete wzgledem relacji PKB per capita do sredniech wynikow testu
  #(czyli te ktore zaobserwowalismy na wykresie)
  #Grupy: 1 - rozwijajace sie, 2 - rozwiniete
  
  slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")
  
  #Kraje srednie dolaczam do rozwinietych po analizie ostatnich wynikow
  wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA",
                         "HUN", "CZE", "ISR", "RUS", "POL")
  
  wyniki_slabe <- filter(frame, CNT %in% slabo_rozwiniete)
  wyniki_wysoko <- filter(frame, CNT %in% wysoko_rozwiniete)
  
  wyniki_slabe <- mutate(wyniki_slabe, grupa_rozwoju = "1")
  wyniki_wysoko <- mutate(wyniki_wysoko, grupa_rozwoju = "2")
  
  wyniki_razem <- rbind(wyniki_slabe, wyniki_wysoko)
  
  return(wyniki_razem)
}

#Funkcje zwracaja ramki danych z odsetkiem osob, ktore nie posiadaja w domu
#przedmiotow o ktore pyta kwestionariusz w pytaniach ST011 i ST012

pytanie_ST012 <- function(dane, kolumny){
  library(dplyr)
  
  #dane - ramka danych z wynikami PISA
  #kolumny - nazwy kolumn zawierajace dane o np liczbie ksiazek w domu
  #ktore chcesz porownac pomiedzy grupami krajow
  
  dane <- dane[, c("CNT", kolumny)]
  dane <- podziel_grupy_PKB(dane)
  dane <- dane[, -1]
  
  wynik <- group_by(dane, grupa_rozwoju) %>%
    summarise_all(funs(100*sum(. == 1, na.rm = TRUE)/sum(!is.na(.))))
  return(wynik)
}

pytanie_ST011 <- function(dane, kolumny){
  library(dplyr)
  
  #dane - ramka danych z wynikami PISA
  #kolumny - nazwy kolumn zawierajace dane o np liczbie ksiazek w domu
  #ktore chcesz porownac pomiedzy grupami krajow
  
  dane <- dane[, c("CNT", kolumny)]
  dane <- podziel_grupy_PKB(dane)
  dane <- dane[, -1]
  
  wynik <- group_by(dane, grupa_rozwoju) %>%
    summarise_all(funs(100*sum(. == 2, na.rm = TRUE)/sum(!is.na(.))))
  return(wynik)
}

#pytanie o ksiązki
pytanie_ST013 <- function(dane){
  dane1 <- dane[, c("CNT", "ST013Q01TA")]
  dane1 <- podziel_grupy_PKB(dane1)
  dane1 <- dane1[, -1]
  wynik <- group_by(dane1, grupa_rozwoju,ST013Q01TA) %>% count()
  wynik <- na.omit(wynik)
  wynik$ST013Q01TA1[wynik$ST013Q01TA==1] <- "[0,10]"
  wynik$ST013Q01TA1[wynik$ST013Q01TA==2] <- "[11,25]"
  wynik$ST013Q01TA1[wynik$ST013Q01TA==3] <- "[26,100]"
  wynik$ST013Q01TA1[wynik$ST013Q01TA==4] <- "[101,200]"
  wynik$ST013Q01TA1[wynik$ST013Q01TA==5] <- "[201,500]"
  wynik$ST013Q01TA1[wynik$ST013Q01TA==6] <- "[500,inf]"
  
  return(wynik)
} 


kolumny_st012 <- c(#"ST012Q01TA",
             "ST012Q02TA",
             "ST012Q03TA",
             "ST012Q05NA",
             "ST012Q06NA",
             "ST012Q07NA",
             #"ST012Q08NA",
             "ST012Q09NA")

kolumny_st011 <- c(#"ST011Q04TA",
                   "ST011Q01TA",
                   "ST011Q12TA",
                   "ST011Q06TA",
                   "ST011Q03TA",
                   "ST011Q02TA",
                   #"ST011Q08TA",
                   #"ST011Q16NA",
                   #"ST011Q10TA",
                   #"ST011Q07TA",
                   "ST011Q05TA",
                   "ST011Q09TA")


ST011 <- pytanie_ST011(dane, kolumny_st011)

ST012 <- pytanie_ST012(dane, kolumny_st012)

#ST013 <- pytanie_ST013(dane)

# a1 <- ggplot(ST013,aes(x=reorder(ST013Q01TA1,ST013Q01TA),y=n,fill=grupa_rozwoju))+geom_bar(stat='identity',position='dodge')+
#   ggtitle("Ilosc ksiazek w domu w zaleznosci od grupy")+
#   coord_flip()+
#   ylab("Liczebnosc grupy")+
#   xlab("")

#a1 to wykres ilosci ksiazek w zaleznosci od grupy rozwoju


#DO WYKRESU

ST011[1, -1] <- lapply(ST011[1, -1], FUN = function(x){-1*x})
ST012[1, -1] <- lapply(ST012[1, -1], FUN = function(x){-1*x})

to_plot <- cbind(ST011, ST012[,-1])
to_plot_melt <- melt(to_plot, id.vars = "grupa_rozwoju")
colnames(to_plot_melt) <- c("grupa_rozwoju", "pytanie", "wartosc")

os_pytan <- to_plot[1, -1]
os_pytan <- os_pytan[, order(os_pytan[1,])]
os_pytan <- colnames(os_pytan)

podpisy <- c("Dictionary", "Smartphone", "A quiet place to study", "Rooms with a bath or shower",
             "A desk to study at", "A room of your own", "A link to the Internet", "Computer","Car",
             "Work of art", "Musical instrument", "Tablet", "Educational software")

naloz_gr_2 <- to_plot_melt[to_plot_melt$grupa_rozwoju==2,]
naloz_gr_2[2,3] <- 6.306474

naloz_gr_1 <- to_plot_melt[to_plot_melt$grupa_rozwoju==2,]
naloz_gr_1[2,3] <- 8.863257

gr_2 <- to_plot_melt[to_plot_melt$grupa_rozwoju==2,]
gr_2[2,3] <- 6.306474


ggplot(NULL)+
  geom_bar(data = to_plot_melt[to_plot_melt$grupa_rozwoju==1,], aes(x = pytanie, y = wartosc), fill = "red", 
           stat = "identity", width = 0.35)+
  geom_bar(data = naloz_gr_2, aes(x = pytanie, y = -wartosc), fill = "#fc8d62", 
           stat = "identity", width = 0.35)+
  geom_bar(data = naloz_gr_1, aes(x = pytanie, y = wartosc), fill = "blue",
           stat = "identity", width = 0.35)+
  geom_bar(data = gr_2, aes(x = pytanie, y = wartosc), fill = "#66c2a5",
           stat = "identity", width = 0.35)+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.3),
        #panel.ontop = TRUE,
        plot.title = element_text(hjust = 0.5, vjust = 3),
        plot.margin = unit(rep(0.7, 4), "cm"))+
  annotate(geom = "label", x = os_pytan[13:1], y = 0, label = podpisy, vjust = 1.8, size = 3.5)+
  xlab("")+
  ylab("Percentage")+
  scale_y_continuous(breaks = seq(-70, 70, by = 10), limits = c(-70, 70), labels = c(seq(70, 10, -10), seq(0, 70, 10)))+
  scale_x_discrete(limits = os_pytan, expand = expand_scale(0.07))+
  ggtitle("Students who do not have at home...")

