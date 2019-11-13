library(haven)
library(tidyverse)
kolumny <- c("CNT","HISCED")
dane <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")
dane1 <- dane[, kolumny]
dane1 <- podziel_grupy_PKB(dane1)
dane1 <- dane1[, -1]

wyksztalcenie <- function(){
  kolumny <- c("CNT","HISCED")
  dane1 <- dane[, kolumny]
  dane1 <- podziel_grupy_PKB(dane1)
  dane1 <- dane1[, -1]
  wynik<- group_by(dane1, grupa_rozwoju,HISCED) %>% count()
  wynik <- na.omit(wynik)
  wynik <- group_by(wynik,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))
  wynik$suma <- NULL
  wynik_podst <- filter(wynik,HISCED %in% c(0,1)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Podstawowe")
  wynik_srednie <- filter(wynik,HISCED %in% c(2,3,4)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Srednie")
  wynik_wyzsze <- filter(wynik,HISCED %in% c(5,6)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Wyzsze")
  wynik <- rbind(wynik_srednie,wynik_podst,wynik_wyzsze)
}
wyksztalcenie_M <- function(){
  kolumny <- c("CNT","FISCED")
  dane1 <- dane[, kolumny]
  dane1 <- podziel_grupy_PKB(dane1)
  dane1 <- dane1[, -1]
  wynik<- group_by(dane1, grupa_rozwoju,FISCED) %>% count()
  wynik <- na.omit(wynik)
  wynik <- group_by(wynik,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))
  wynik$suma <- NULL
  wynik_podst <- filter(wynik,FISCED %in% c(0,1)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Podstawowe")
  wynik_srednie <- filter(wynik,FISCED %in% c(2,3,4)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Srednie")
  wynik_wyzsze <- filter(wynik,FISCED %in% c(5,6)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Wyzsze")
  wynik <- rbind(wynik_wyzsze,wynik_srednie,wynik_podst)
}
wyksztalcenie_K <- function(){
  kolumny <- c("CNT","MISCED")
  dane1 <- dane[, kolumny]
  dane1 <- podziel_grupy_PKB(dane1)
  dane1 <- dane1[, -1]
  wynik<- group_by(dane1, grupa_rozwoju,MISCED) %>% count()
  wynik <- na.omit(wynik)
  wynik <- group_by(wynik,grupa_rozwoju) %>% mutate(suma = sum(n)) %>% mutate(procent=round(n/suma,digits=3))
  wynik$suma <- NULL
  wynik_podst <- filter(wynik,MISCED %in% c(0,1)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Podstawowe")
  wynik_srednie <- filter(wynik,MISCED %in% c(2,3,4)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Srednie")
  wynik_wyzsze <- filter(wynik,MISCED %in% c(5,6)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Wyzsze")
  wynik <- rbind(wynik_wyzsze,wynik_srednie,wynik_podst)
}

najwyzsze <- wyksztalcenie()
kobiety <- wyksztalcenie_K()
faceci <- wyksztalcenie_M()



a <- ggplot(najwyzsze,aes(x=grupa_rozwoju,y=procent))+
  geom_bar(aes(fill=Wyksztalcenie),width=0.3,stat='identity',position = position_fill(reverse = TRUE))+
  theme_classic()+
  coord_flip()+xlab("")+ylab("")+ggtitle("Wyksztalcenie wsrod rodzicow po grupach rozwoju")+
  theme(plot.title = element_text(hjust = 0.5,size=18))

ak <- ggplot(kobiety,aes(x=grupa_rozwoju,y=procent))+
  geom_bar(aes(fill=Wyksztalcenie),width=0.3,stat='identity',position = position_fill(reverse = TRUE))+
  theme_classic()+
  coord_flip()+xlab("")+ylab("")+ggtitle("Wyksztalcenie wsrod mam po grupach rozwoju")+
  theme(plot.title = element_text(hjust = 0.5,size=18),panel.background = element_blank())
  
  

am <- ggplot(faceci,aes(x=grupa_rozwoju,y=procent))+
  geom_bar(aes(fill=Wyksztalcenie),width=0.3,stat='identity',position = position_fill(reverse = TRUE))+
  theme_classic()+
  coord_flip()+xlab("")+ylab("")+ggtitle("Wyksztalcenie wsrod ojców po grupach rozwoju")+
  theme(plot.title = element_text(hjust = 0.5,size=18))

ggsave(file="Wykształczenie_matek.svg", plot=ak, width=10, height=8)
ggsave(file="Wykształczenie_ojcow.svg", plot=am, width=10, height=8)

