#dane <- haven::read_sas("../cy6_ms_cmb_stu_qqq.sas7bdat")
library(dplyr)
grupy_by_PKG <-  podziel_grupy_PKB (dane)
grupy <- cbind(grupy_by_PKG[,c(1:10,922)],select(grupy_by_PKG,PV1MATH:PV10SCIE))
#dane2 <- haven::read_sas("../cy6_ms_cmb_sch_qqq.sas7bdat")
dane2 <- haven::read_sas("../cy6_ms_cmb_sch_qqq.sas7bdat")
dane2 <- cbind(dane2[,1:3],dane2[,"SC013Q01TA"])



colnames(grupy)[3]
grupy <- merge(grupy,dane2,by="CNTSCHID",all.x=TRUE)
slice(grupy_test)

colnames(grupy)[44] <- "PU_PR"
table(grupy$PU_PR)
library(ggplot2)
grupy <-best_in_all(100,grupy)
grupy$PU_PR <- ifelse(grupy$PU_PR==1,"PU","PR")
grupy <- grupy[!is.na(grupy$PU_PR),]

ggplot(grupy,aes(x=average_pv_all,fill=PU_PR))+geom_density(alpha=0.5)+ggtitle("Gęstośc w zależności od typu szkoły ")+
  labs(y = "Gęstość", x="Średni Wynik")+scale_fill_discrete(name = "Typ Szkoły", labels = c("Prywatne", "Publiczne"))+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), title = element_text(size=20))

ggsave("Wykres_prywatne/all.png",width = 18, height = 9)

ggplot(subset(grupy,grupa_rozwoju==1),aes(x=average_pv_all,fill=PU_PR))+geom_density(alpha=0.5)+ggtitle("Gęstośc w zależności od typu szkoły w krajach rozwijających się ")+
  labs(y = "Gęstość", x="Średni Wynik")+scale_fill_discrete(name = "Typ Szkoły", labels = c("Prywatne", "Publiczne"))+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), title = element_text(size=20))

ggsave("Wykres_prywatne/3świat.png",width = 18, height = 9)

ggplot(subset(grupy,grupa_rozwoju==3),aes(x=average_pv_all,fill=PU_PR))+geom_density(alpha=0.5)+ggtitle("Gęstośc w zależności od typu szkoły w krajach rowiniętych ")+
  labs(y = "Gęstość", x="Średni Wynik")+scale_fill_discrete(name = "Typ Szkoły", labels = c("Prywatne", "Publiczne"))+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), title = element_text(size=20))

ggsave("Wykres_prywatne/1świat.png",width = 18, height = 9)


ggplot(subset(grupy,grupa_rozwoju==2),aes(x=average_pv_all,fill=PU_PR))+geom_density(alpha=0.5)+ggtitle("Gęstośc w zależności od typu szkoły w krajach średnio rozwiniętych ")+
  labs(y = "Gęstość", x="Średni Wynik")+scale_fill_discrete(name = "Typ Szkoły", labels = c("Prywatne", "Publiczne"))+
  theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), title = element_text(size=20))
 
ggsave("Wykres_prywatne/2świat.png",width = 18, height = 9)

library(intsvy)
library(dplyr)

