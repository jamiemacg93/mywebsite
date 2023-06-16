



# download libraries ------------------------------------------------------
install.packages("WDIplus")
options(scipen = 999) #disable scientific notation
library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)
library(here)
library(scales)
library(vip)
library(C50)
library(WDI)

# create database ---------------------------------------------------------
#how big is big? the central question of this mini project is "what is a lot?" for a range of commonly understood and used development indicators 

#let's start with GDP per capita, GDP overall, populatio total, Energy use, urgan population, poverty porportion  

indicators <- c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD", "SP.POP.TOTL", "EG.USE.PCAP.KG.OE", "SP.URB.TOTL.IN.ZS", "SI.POV.DDAY")
#make sure we only have UN recognised countries (rather than regions)
real_countries_list <- c("AF","AX","AL","DZ","AS","AD","AO","AI","AQ","AG","AR","AM","AW","AU","AT","AZ","BS","BH","BD","BB","BY","BE","BZ","BJ","BM","BT","BO","BQ","BA","BW","BR","IO","BN","BG","BF","BI","KH","CM","CA","CV","KY","CF","TD","CL","CN","CX","CC","CO","KM","CG","CD","CK","CR","CI","HR","CU","CW","CY","CZ","DK","DJ","DM","DO","EC","EG","SV","GQ","ER","EE","SZ","ET","FK","FO","FJ","FI","FR","GF","PF","TF","GA","GM","GE","DE","GH","GI","GR","GL","GD","GP","GU","GT","GG","GN","GW","GY","HT","HM","VA","HN","HK","HU","IS","IN","ID","XZ","IR","IQ","IE","IM","IL","IT","JM","JP","JE","JO","KZ","KE","KI","KP","KR","KW","KG","LA","LV","LB","LS","LR","LY","LI","LT","LU","MO","MG","MW","MY","MV","ML","MT","MH","MQ","MR","MU","YT","MX","FM","MD","MC","MN","ME","MS","MA","MZ","MM","NA","NR","NP","NL","NC","NZ","NI","NE","NG","NU","NF","MK","MP","NO","OM","PK","PW","PS","PA","PG","PY","PE","PH","PN","PL","PT","PR","QA","RE","RO","RU","RW","BL","SH","KN","LC","MF","PM","VC","WS","SM","ST","SA","SN","RS","SC","SL","SG","SX","SK","SI","SB","SO","ZA","GS","SS","ES","LK","SD","SR","SJ","SE","CH","SY","TW","TJ","TZ","TH","TL","TG","TK","TO","TT","TN","TR","TM","TC","TV","UG","UA","AE","GB","US","UM","UY","UZ","VU","VE","VN","VG","VI","WF","EH","YE","ZM","ZW")

data <- WDI(
  country = real_countries_list,
  indicator = indicators,
  start = 1960,
  end = 2020,)

data$country <- factor(data$country)

bechdel %>% 
  select(test, budget_2013, domgross_2013, intgross_2013, imdb_rating, metascore)%>% 
  ggpairs(aes(colour=test), alpha=0.2)+
  theme_bw()

            