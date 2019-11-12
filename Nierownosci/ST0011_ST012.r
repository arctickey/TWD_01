#Funkcja do kalkulacji roznic pomiedzy grupami krajow rozwinietych i rozwijajacych sie
library(haven)

#Funkcja wykorszystywana do podzielenia krajow na grupy
podziel_grupy_PKB <- function(frame){
  library(dplyr)
  #Podajemy data frame wynikow PISA, otrzynujemy dodatkowa kolumne grupujaca na kraje
  #slabo, srednio i wysoko rozwiniete wzgledem relacji PKB per capita do sredniech wynikow testu
  #(czyli te ktore zaobserwowalismy na wykresie)
  #Grupy: 1 - rozwijajace sie, 2 - rozwiniete
  
  slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")
  #srednio_rozwiniete <- c("HUN", "CZE", "ISR", "RUS", "POL")
  
  #Kraje srednie dolaczam do rozwinietych po analizie ostatnich wynikow
  wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA",
                         "HUN", "CZE", "ISR", "RUS", "POL")
  
  wyniki_slabe <- filter(frame, CNT %in% slabo_rozwiniete)
  #wyniki_srednie <- filter(frame, CNT %in% srednio_rozwiniete)
  wyniki_wysoko <- filter(frame, CNT %in% wysoko_rozwiniete)
  
  wyniki_slabe <- mutate(wyniki_slabe, grupa_rozwoju = "1")
  #wyniki_srednie <- mutate(wyniki_srednie, grupa_rozwoju = "2")
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
} #pytanie o ksiązki

#dane <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")




kolumny_st012 <- c("ST012Q01TA",
             "ST012Q02TA",
             "ST012Q03TA",
             "ST012Q05NA",
             "ST012Q06NA",
             "ST012Q07NA",
             "ST012Q08NA",
             "ST012Q09NA")

kolumny_st011 <- c("ST011Q04TA",
                   "ST011Q01TA",
                   "ST011Q12TA",
                   "ST011Q06TA",
                   "ST011Q03TA",
                   "ST011Q02TA",
                   "ST011Q08TA",
                   "ST011Q16NA",
                   "ST011Q10TA",
                   "ST011Q07TA",
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
