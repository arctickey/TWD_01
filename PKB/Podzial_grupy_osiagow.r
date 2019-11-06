
podziel_grupy_PKB <- function(frame){
  library(dplyr)
  #Podajemy data frame wynikow PISA, otrzynujemy dodatkowa kolumne grupujaca na kraje
  #slabo, srednio i wysoko rozwiniete wzgledem relacji PKB per capita do sredniech wynikow testu
  #(czyli te ktore zaobserwowalismy na wykresie)
  #Grupy: 1 - slabe, 2 - srednie, 3 - wysoko rozwiniete
  
  slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")
  srednio_rozwiniete <- c("HUN", "CZE", "ISR", "RUS", "POL")
  wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA")
  
  wyniki_slabe <- filter(frame, CNT %in% slabo_rozwiniete)
  wyniki_srednie <- filter(frame, CNT %in% srednio_rozwiniete)
  wyniki_wysoko <- filter(frame, CNT %in% wysoko_rozwiniete)
  
  wyniki_slabe <- mutate(wyniki_slabe, grupa_rozwoju = "1")
  wyniki_srednie <- mutate(wyniki_srednie, grupa_rozwoju = "2")
  wyniki_wysoko <- mutate(wyniki_wysoko, grupa_rozwoju = "3")
  
  wyniki_razem <- rbind(wyniki_slabe, wyniki_srednie, wyniki_wysoko)
  
  return(wyniki_razem)
}

