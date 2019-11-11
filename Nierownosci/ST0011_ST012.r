#Funkcja do kalkulacji roznic pomiedzy grupami krajow rozwinietych i rozwijajacych sie

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