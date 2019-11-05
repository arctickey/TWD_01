library(haven)
library(intsvy)
library(countrycode)
library(tidyverse)



data <- read_sas("./cy6_ms_cmb_stu_qqq.sas7bdat")
data1 <- data %>% select(CNT,PV1MATH:PV10SCIE)
data1 <- best_in_all(100,data1) 
data1 <- data1 %>% select(CNT,average_pv_all)
ggplot(data1,aes(x=average_pv_all))+geom_density(fill='lightblue')+facet_wrap(~CNT) #plot wszystkich panstw
data2 <- data1 %>% filter(CNT==c("CHL","POL"))
ggplot(data2,aes(x=average_pv_all,fill=CNT))+geom_density()
