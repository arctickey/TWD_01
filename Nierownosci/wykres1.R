library(haven)
library(ggplot2)
library(dplyr)
kolumny <- c("CNT","HISCED")
dane <- read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
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
    mutate(Wyksztalcenie="Średnie")
  wynik_wyzsze <- filter(wynik,FISCED %in% c(5,6)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Wyższe")
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
    mutate(Wyksztalcenie="Średnie")
  wynik_wyzsze <- filter(wynik,MISCED %in% c(5,6)) %>% group_by(grupa_rozwoju) %>% summarise(procent = sum(procent)) %>% 
    mutate(Wyksztalcenie="Wyższe")
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
  geom_bar(aes(fill=Wyksztalcenie),width=0.9,stat='identity',position = position_fill(reverse = TRUE))+
  scale_fill_manual(values=c("#fc8d62","#8da0cb","#ffd92f"))+
  theme_minimal()+
  coord_flip()+xlab("")+ylab("")+ggtitle("Wykształcenie matek wsród grup rozwoju")+
  scale_y_continuous(breaks = seq(0,1,0.25),labels = paste0(seq(0,1,0.25)*100,"%"))+
  theme(plot.title = element_text(hjust = 0.5,size=18),panel.background = element_blank(),
        axis.text.x = element_text(size=12,face ="bold" ),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size=12),
        legend.title = element_blank())
  
  


am <- ggplot(faceci,aes(x=grupa_rozwoju,y=procent))+
  geom_bar(aes(fill=Wyksztalcenie),width=0.9,stat='identity',position = position_fill(reverse = TRUE))+
  scale_fill_manual(values=c("#fc8d62","#8da0cb","#ffd92f"))+
  theme_minimal()+
  coord_flip()+xlab("")+ylab("")+ggtitle("Wykształcenie ojców wsród grup rozwoju")+
  scale_y_continuous(breaks = seq(0,1,0.25),labels = paste0(seq(0,1,0.25)*100,"%"))+
  theme(plot.title = element_text(hjust = 0.5,size=18),panel.background = element_blank(),
        axis.text.x = element_text(size=12,face ="bold" ),
        legend.position = "top",
        legend.direction = "horizontal",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size=12),
        legend.title = element_blank())

ggsave("Wykształcenie_matek1.svg",plot=ak,height=9,width=18,device="svg")
ggsave("Wykształcenie_ojców1.svg",plot=ak,height=9,width=18,device="svg")


