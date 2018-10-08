# Tables to be scrapped: 18100004, 18100005, 18100006, 18100256, 18100007

# 1810000401 Consumer Price Index, monthly, not seasonally adjusted
# 1810000501 Consumer Price Index, annual average, not seasonally adjusted
# 1810000601 Consumer Price Indexes, monthly, seasonally adjusted
# 1810025601 Consumer Price Index (CPI) statistics, measures of core inflation and other related statistics - Bank of Canada definitions
# 1810000701 Basket weights of the Consumer Price Index, Canada, provinces, Whitehorse, Yellowknife and Iqaluit


library(CANSIM2R)
library(tidyverse)
library(plyr)

# Functions to use:

growth <- function(x){x/lag(x)*100-100}
growtL <- function(x){x/lag(x, 12)*100-100}

recoding <- function(x){
 recode(x$geo, 
         "Canada" = "CAN",
         "Alberta" = "AB",
         "British Columbia" = "BC",
         "Manitoba" = "MB",
         "New Brunswick" = "NB",
         "Newfoundland and Labrador" = "NL",
         "Nova Scotia" = "NS",
         "Northwes Territories" = "NT",
         "Nunavut" = "NU",
         "Ontario" = "ON",
         "Prince Edward Island" = "PE",
         "Quebec" = "QC",
         "Saskatchewan" = "SK",
         "Yukon" = "YK",
         "St. John's, Newfoundland and Labrador"  ="St. John's, NL", 
         "Charlottetown and Summerside, Prince Edward Island"="Charlottetown, PE",
         "Halifax, Nova Scotia"="Halifax, NS",
         "Saint John, New Brunswick"="Saint John, NB" ,
         "Qu\xe9bec, Quebec"="Quebec, QC" ,
         "Montr\xe9al, Quebec"="Montreal, QC" ,
         "Ottawa-Gatineau, Ontario part, Ontario/Quebec" ="NCR, ON/QC",
         "Toronto, Ontario"="Toronto, ON" ,
         "Thunder Bay, Ontario"="Thunder Bay, ON" ,
         "Winnipeg, Manitoba"="Winnipeg, MB" ,
         "Regina, Saskatchewan"="Regina, SK" ,
         "Saskatoon, Saskatchewan"="Saskatoon, SK",
         "Edmonton, Alberta"="Edmonton, AB" ,
         "Calgary, Alberta"="Calgary, AB" ,
         "Vancouver, British Columbia"="Vancouver, BC" ,
         "Victoria, British Columbia"="Victoria, BC" ,
         "Whitehorse, Yukon"="Whitehorse, YK",
         "Yellowknife, Northwest Territories"="Yellowknife, NT",
         "Iqaluit, Nunavut"="Iqaluit, NU")
}

#### Consumer Price Index, monthly, not seasonally adjusted (2002=100)

table04 <- getCANSIM(18100004, raw = TRUE)

table04 <- table04 %>% 
  filter(UOM %in% "2002=100") %>% 
  select(date=REF_DATE, geo=GEO, item=Products.and.product.groups, value=VALUE) %>%
  mutate(geo=as.factor(geo), item=as.factor(item))%>%
  group_by(item, geo) %>% 
  ddply(.(geo, item),transform,
        mon_growth = round((value*100 / lag(value) - 100),2)) %>%
  ddply(.(geo, item),transform,
        yea_growth = round(growtL(value), 2))

table04["geo"] <-recoding(table04["geo"])
table04$fec <- as.Date(paste( table04$date, '-01',sep =""))


# Creating a new table with CPI monthly growth
table04_mo <- table04 %>% 
  select(date, geo, item, value=mon_growth, fec)%>%
  filter(!is.na(value)) 


# Creating a new table with CPI yearly growth
table04_ye <- table04 %>% 
  select(date, geo, item, value=yea_growth, fec)%>%
  filter(!is.na(value)) 


###### Playing with some plots: 

library(ggplot2)
library(plotly)
a <- table04 %>% filter(geo=="AB", item=="Alcoholic beverages")

p <- plot_ly(x = a$date, y = a$value, type = 'scatter', mode = 'lines')


#### Consumer Price Index, annual average, not seasonally adjusted (2002=100)

table05 <- getCANSIM(18100005,raw = TRUE)

table05 <- table05 %>% 
  filter(UOM %in% "2002=100") %>% 
  select(date=REF_DATE, geo=GEO, item=Products.and.product.groups, value=VALUE)%>%
  mutate(geo=as.factor(geo), item=as.factor(item))%>% 
  group_by(item, geo) %>% 
  ddply(.(geo, item),transform,
        yea_growth = round((value*100 / lag(value) - 100),2)) 

table05["geo"]<-recoding(table05["geo"])
table05$fec <- as.Date(paste('01/01', table05$date, sep = "/"), format='%d/%m/%Y')


# Creating a new table with CPI yearly growth
table05_ye <- table05 %>% 
  select(date, geo, item, value=yea_growth, fec)%>%
  filter(!is.na(value)) 


#### Consumer Price Indexes, monthly, seasonally adjusted (CAN)
table06 <- getCANSIM(18100006,raw = TRUE)

table06 <- table06 %>% 
  filter(UOM %in% "2002=100") %>% 
  select(date=REF_DATE, geo=GEO, item=Products.and.product.groups, value=VALUE)%>%
  mutate(geo=as.factor(geo), item=as.factor(item))%>% 
  group_by(item, geo) %>% 
  ddply(.(geo, item),transform,
        mon_growth = round((value*100 / lag(value) - 100),2))

table06$geo<-recode(table06$geo, "Canada"="CAN")


#### 1810025601 Consumer Price Index (CPI) statistics, measures of core inflation and other related statistics - Bank of Canada definitions
table25 <- getCANSIM(18100256,raw = TRUE)  

table25 <- table25 %>% 
  select(date=REF_DATE, geo=GEO, measure=Alternative.measures, value=VALUE, uni=UOM)%>%
  na.omit("value")%>%
  mutate(geo=as.factor(geo), measure=as.factor(measure))%>% 
  group_by(geo, measure) %>% 
  ddply(.(geo, measure),transform,
        mon_growth = round((value*100 / lag(value) - 100),2)) 


table25$geo<-recode(table25$geo, "Canada"="CAN")

#### 1810000701 Basket weights of the Consumer Price Index, Canada, provinces, Whitehorse, Yellowknife and Iqaluit (percent)
table07 <- getCANSIM(18100007,raw = TRUE)

table07 <- table07 %>% 
  select(date=REF_DATE, geo=GEO, item=Products.and.product.groups, weight=Price.period.of.weight, value=VALUE, distr=Geographic.distribution.of.weight, coor=COORDINATE)%>%
  na.omit("value")%>%
  mutate(geo=as.factor(geo), measure=as.factor(item))%>% 
  group_by(geo, item) 


choices <- list(table04, table05, table06, table07, table25)
choices1 <- list(table04, table04_mo, table04_ye, table05, table05_ye)

