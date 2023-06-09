---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-16"
description: Data Exploration - Energy Output in South Africa # the title that will show up once someone gets to this page
draft: false
image: sun.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: energy # slug is the shorthand URL address... no spaces plz
title: Data Exploration of South Africa's Energy Output
---

# Exploring sources of electricity production, CO2 emissions, and GDP per capita.

There are many sources of data on how countries generate their electricity and their CO2 emissions. 

We will get energy data from the Our World in Data website, and CO2 and GDP per capita emissions from the World Bank, using the `wbstats`package.


```r
#| warning: false
library(blogdown)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.2     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(wbstats)
library(skimr)
library(countrycode)
library(here)
```

```
## here() starts at C:/Users/Jamie/Documents/mydsb2023/mywebsite
```

```r
# Download electricity data
url <- "https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv"

energy <- read_csv(url) %>% 
  filter(year >= 1990) %>% 
  drop_na(iso_code) %>% 
  select(1:3,
         biofuel = biofuel_electricity,
         coal = coal_electricity,
         gas = gas_electricity,
         hydro = hydro_electricity,
         nuclear = nuclear_electricity,
         oil = oil_electricity,
         other_renewable = other_renewable_exc_biofuel_electricity,
         solar = solar_electricity,
         wind = wind_electricity, 
         electricity_demand,
         electricity_generation,
         net_elec_imports,	# Net electricity imports, measured in terawatt-hours
         energy_per_capita,	# Primary energy consumption per capita, measured in kilowatt-hours	Calculated by Our World in Data based on BP Statistical Review of World Energy and EIA International Energy Data
         energy_per_gdp,	# Energy consumption per unit of GDP. This is measured in kilowatt-hours per 2011 international-$.
         per_capita_electricity, #	Electricity generation per capita, measured in kilowatt-hours
  ) 
```

```
## Rows: 21890 Columns: 129
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (2): country, iso_code
## dbl (127): year, population, gdp, biofuel_cons_change_pct, biofuel_cons_chan...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Download data for C02 emissions per capita https://data.worldbank.org/indicator/EN.ATM.CO2E.PC
co2_percap <- wb_data(country = "countries_only", 
                      indicator = "EN.ATM.CO2E.PC", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         co2percap = value)

############################################################################################
############################################################################################


# Download data for GDP per capita  https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
gdp_percap <- wb_data(country = "countries_only", 
                      indicator = "NY.GDP.PCAP.PP.KD", 
                      start_date = 1990, 
                      end_date = 2022,
                      return_wide=FALSE) %>% 
  filter(!is.na(value)) %>% 
  #drop unwanted variables
  select(-c(unit, obs_status, footnote, last_updated)) %>% 
  rename(year = date,
         GDPpercap = value)
```



```r
#convert to long, tidy format by shifting the energy sources into one variable and their respective values into another
energy_tidy <- energy %>% 
  pivot_longer(cols = c(4,5,6,7,8,9,10,11,12), 
               names_to = "energy_source",
               values_to = "energy_use")

#make sure ISO codes column names match
colnames(gdp_percap)[4] <- "iso_code"
colnames(co2_percap)[4] <- "iso_code"
#join the energy_tidy and gdp_percap
merged_data <- left_join(gdp_percap, energy_tidy, by = c("iso_code","year"))

#join co2_percap to the merged data
merged_data <- left_join(merged_data, co2_percap, by = c('iso_code', "year"))
```



```r
#example code given
knitr::include_graphics(here::here("images", "electricity-co2-gdp.png"), error = FALSE)
```

![](../../images/electricity-co2-gdp.png)<!-- -->

```r
#############Question 1################
#######################################
#Question 1: let's first try electricity generation stacked area chart for South Africa --> as expected majority coal

zaf_energy <- merged_data %>% 
  filter(iso_code == "ZAF")

ggplot(zaf_energy, aes(x = year, y = energy_use, fill = energy_source)) + 
  geom_area(colour="grey90", alpha = 0.5, position = "fill")
```

<img src="/blogs/homework2C_files/figure-html/unnamed-chunk-3-2.png" width="672" />



```r
#given South Africa's electricity crisis, I was curious to see whether the increase share in renewables is attributable to dropping output overall, 
# as we can see from the below, energy output overall has declined since 2010 :( 
ggplot(zaf_energy, aes(x = year, y = energy_use, fill = energy_source)) + 
  geom_bar(stat = "identity")
```

<img src="/blogs/homework2C_files/figure-html/unnamed-chunk-4-1.png" width="672" />



```r
#############Question 2################

#playing around with formatting to see what we can map in a 2d space, e.g. it seems like the relationship between GDP per capita and co2 is getting weaker over time
#i.e. hotter colours show a steeper slope than colder colours. the triangles and circles are probably overkill
ggplot(merged_data, aes(x = GDPpercap, y = co2percap, color = year))+
  geom_point(size = 0.5, aes(shape = ifelse(GDPpercap > 50000, "circle", "triangle")))+
  labs(x ="GDP per capita",
       y = "CO2 per capita",
       title = "CO2 vs GDP (per capita)",
       shape = "GDP high (circle) vs low (triangle)")+
  scale_color_gradientn(colours = rainbow(10))+
  scale_shape_manual(values = c("circle"=1, "triangle"=2))
```

<img src="/blogs/homework2C_files/figure-html/unnamed-chunk-5-1.png" width="672" />
