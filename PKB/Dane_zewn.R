library(haven)
library(intsvy)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggrepel)


#dane <- read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
data <- dane
x <- pisa2015.mean.pv(pvlabel = "MATH", by = "CNT", data = data)
pom1 <- codelist %>% select(continent,iso3c)
pom1 <- na.omit(pom1)
x1 <- left_join(x,pom1,by = c("CNT"="iso3c"))
x1 <- na.omit(x1)

# teach <- read.csv("./teachers.csv") %>% filter(TIME==2015,SUBJECT=="LOWSRY") %>% select(LOCATION,Value)
# x3 <- left_join(x1,teach,by=c("CNT"="LOCATION"))
# x3 <- na.omit(x3)

GDP <- read.csv("./PKB/GDP.csv",header = FALSE)
GDP <- select(GDP,V2,V50) %>% slice(4:n())
x2 <- left_join(x1,GDP,by=c("CNT"="V2"))

#Dane do uzyskania realnej sily nabywczej pieniadza
wartosc_pieniadza <- read.csv("big-mac-raw-index.csv")
wartosc_pieniadza <- wartosc_pieniadza[wartosc_pieniadza$date=="2015-07-01", c(2,4,7)]
x2 <- merge(x2, wartosc_pieniadza, by.x="CNT", by.y="iso_a3")
x2 <- x2[,-c(2,4,5,6)]
x2$grupa_rozwoju <- NA
wysoko_rozwiniete <- c("NZL", "CAN", "CHE", "AUS", "SWE", "GBR", "DNK", "NOR","USA","JPN")
slabo_rozwiniete <- c("BRA", "MEX", "CHL", "PER", "COL", "IDN", "CRI", "TUR", "URY", "THA")
wr_indeksy <- ifelse(x2$CNT%in% wysoko_rozwiniete,TRUE,FALSE)
x2$grupa_rozwoju[wr_indeksy] <- "wr"
sr_indeksy <- ifelse(x2$CNT %in% slabo_rozwiniete,TRUE,FALSE)
x2$grupa_rozwoju[sr_indeksy] <- "sr"

kraje_pl <- c("ZEA", "Australia",
              "Brazylia", "Kanada", "Szwajcaria", "Chile", "Kolumbia", "Kostaryka", "Czechy",
              "Dania", "Wielka Brytania" ,"Hongkong" ,"Węgry", "Indonezja" ,"Izrael" ,"Japonia" ,"Korea Południowa" ,"Meksyk",
              "Norwegia", "Nowa Zelandia", "Peru", "Polska", "Rosja", "Singapur", "Szwecja", "Tajlandia", "Turcja",
              "Urugwaj", "Stany Zjednoczone", "Wietnam")

x2$name <- kraje_pl

ineksy <- ifelse(is.na(x2$grupa_rozwoju),TRUE,FALSE)
ineksy

x2$grupa_rozwoju[ineksy] <-  "g1"
  ggplot(x2,aes(x=V50/dollar_price,y=Mean,colour=grupa_rozwoju))+
  geom_point(size=5)+
  stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=V50/dollar_price,y=Mean),alpha =0.2)+
  labs(y = "Średni wynik w kraju", x="PKB per capita (realna siła nabywcza)",color="Grupa Rozwoju",
      title ="Średni wynik w kraju względem PKB per capita",
      subtitle = "Dane zostały przeskalowane przez indeks big-maca aby oddawać faktyczną wartość nabywczą pieniądza"
      )+
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))+
  scale_color_manual(breaks=c("wr","sr"),labels=c("Wysoko rozwinięte","Rozwijające się"),values =c("#636363","#fb5515","#66c2a5"))+
  geom_label_repel(aes(label = name), size = 6, color = "black",force = 1)+
  theme_minimal()+
  scale_y_continuous(limits = c(350 ,575),breaks=seq(350,575,25))+
  scale_x_continuous(breaks = seq(0,12000,1000),limits = c(0,12700),expand = c(0,0),labels = paste0(seq(0,12000,1000),"$"))+
  theme(
    legend.title = element_text(size=15),
    legend.text = element_text(size =15 ),
    plot.title = element_text(hjust = 0.5,size = 20),
    plot.subtitle = element_text(hjust=0.5,size =12),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(face="bold"),
    axis.text.y = element_text(face ="bold"),
    # ustawaine legendy
    #legend.direction = "horizontal",
    #legend.position = "top",
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(1.5, "cm"),
    legend.position = c(0.55,0.3),
    panel.grid.major = element_line(colour = "gray",size=0.3),
    panel.grid.minor = element_line(colour = "gray",size=0.3)
  )
    
  
library(svglite)
svglite("PLAKAT//PKB_legenda_in.svg", width = 10, height = 10)
  
dev.off()  



ggsave("PKB/PKB1.png", width = 18, height = 9)


# b <- ggplot(x3,aes(x=Value,y=Mean,colour=continent))+
#   geom_point(size=4)+
#   stat_smooth(method = "auto",inherit.aes = FALSE,aes(x=Value,y=Mean))+
#   ggtitle("Czas pracy nauczyciela vs sredni wynik")+
#   labs(y = "Średni wynik w kraju", x="Czas pracy nauczyciela",color="Kontynent")+
#   theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
# 
# ggsave("Teach_salaries,png",plot = b,device="png")
