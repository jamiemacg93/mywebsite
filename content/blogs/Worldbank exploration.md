---
title: "Mass shootings"
date: "2023-06-16"
description: Mass shootings in the US
draft: no
image: guns.jpg
keywords: ''
slug: country
categories:
- ''
- ''
---


```r
# download libraries ------------------------------------------------------
options(scipen = 999) #disable scientific notation
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
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.1.0 ──
## ✔ broom        1.0.5     ✔ rsample      1.1.1
## ✔ dials        1.2.0     ✔ tune         1.1.1
## ✔ infer        1.0.4     ✔ workflows    1.1.3
## ✔ modeldata    1.1.0     ✔ workflowsets 1.0.1
## ✔ parsnip      1.1.0     ✔ yardstick    1.2.0
## ✔ recipes      1.0.6     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
## • Learn how to get started at https://www.tidymodels.org/start/
```

```r
library(GGally)
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

```r
library(C50)
library(WDI)
library(cluster)
library(factoextra)
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

```r
library(NbClust)
library(mclust)
```

```
## Package 'mclust' version 6.0.0
## Type 'citation("mclust")' for citing this R package in publications.
## 
## Attaching package: 'mclust'
## 
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(kernlab)
```

```
## 
## Attaching package: 'kernlab'
## 
## The following object is masked from 'package:scales':
## 
##     alpha
## 
## The following object is masked from 'package:purrr':
## 
##     cross
## 
## The following object is masked from 'package:ggplot2':
## 
##     alpha
```
# Create database ---------------------------------------------------------
#how big is big? the central question of this mini project is how should we group the world's countries?

#let's start with GDP per capita, GDP overall, population total, Energy use, urgan population, poverty porportion  

```r
#for the sake of learning, I'm going to keep the variables extremely simple: GDP, population, poverty, urbanisation and energy
indicators <- c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD", "SP.POP.TOTL","SP.URB.TOTL.IN.ZS")
#make sure we only have UN recognised countries (rather than regions)
real_countries_list <- c("AF" ,"AL" ,"DZ" ,"AS" ,"AD" ,"AO" ,"AG" ,"AR" ,"AM" ,"AW" ,"AU" ,"AT" ,"AZ" ,"BS" ,"BH" ,"BD" ,"BB" ,"BY" ,"BE" ,"BZ" ,"BJ" ,"BM" ,"BT","BO" ,"BA" ,"BW" ,"BR" ,"BN" ,"BG" ,"BF" ,"BI" ,"KH" ,"CM" ,"CA" ,"CV" ,"KY" ,"CF" ,"TD" ,"CL" ,"CN" ,"CO" ,"KM" ,"CG" ,"CD" ,"CR" ,"CI","HR" ,"CU" ,"CW" ,"CY" ,"CZ" ,"DK" ,"DJ" ,"DM" ,"DO" ,"EC" ,"EG" ,"SV" ,"GQ" ,"ER" ,"EE" ,"SZ" ,"ET" ,"FO" ,"FJ" ,"FI" ,"FR" ,"PF" ,"GA","GM" ,"GE" ,"DE" ,"GH" ,"GI" ,"GR" ,"GL" ,"GD" ,"GU" ,"GT" ,"GN" ,"GW" ,"GY" ,"HT" ,"HN" ,"HK" ,"HU" ,"IS" ,"IN" ,"ID" ,"IR" ,"IQ" ,"IE","IM" ,"IL" ,"IT" ,"JM" ,"JP" ,"JO" ,"KZ" ,"KE" ,"KI" ,"KP" ,"KR" ,"KW" ,"KG" ,"LA" ,"LV" ,"LB" ,"LS" ,"LR" ,"LY" ,"LI" ,"LT" ,"LU" ,"MO","MG" ,"MW" ,"MY" ,"MV" ,"ML" ,"MT" ,"MH" ,"MR" ,"MU" ,"MX" ,"FM" ,"MD" ,"MC" ,"MN" ,"ME" ,"MA" ,"MZ" ,"MM" ,"NA" ,"NR" ,"NP" ,"NL" ,"NC","NZ" ,"NI" ,"NE" ,"NG" ,"MK" ,"MP" ,"NO" ,"OM" ,"PK" ,"PW" ,"PS" ,"PA" ,"PG" ,"PY" ,"PE" ,"PH" ,"PL" ,"PT" ,"PR" ,"QA" ,"RO" ,"RU" ,"RW","KN" ,"LC" ,"MF" ,"VC" ,"WS" ,"SM" ,"ST" ,"SA" ,"SN" ,"RS" ,"SC" ,"SL" ,"SG" ,"SX" ,"SK" ,"SI" ,"SB" ,"SO" ,"ZA" ,"SS" ,"ES" ,"LK" ,"SD","SR" ,"SE" ,"CH" ,"SY" ,"TW" ,"TJ" ,"TZ" ,"TH" ,"TL" ,"TG" ,"TO" ,"TT" ,"TN" ,"TR" ,"TM" ,"TC" ,"TV" ,"UG" ,"UA" ,"AE" ,"GB" ,"US" ,"UY","UZ" ,"VU" ,"VE" ,"VN" ,"VG" ,"VI" ,"YE" ,"ZM" ,"ZW")
data <- WDI(
  country = real_countries_list,
  indicator = indicators,
  start = 1960,
  end = 2020,)

#rename columns
colnames(data) <- c("country","iso2", "iso3", "year",'gdp_percap','gdp','pop','urban')

#To simplify further, let's just take two years, 50 years apart, and see how much things have changed over that period of time
data %>% 
  filter(year %in% c(2020, 1970)) %>% 
  mutate(year = factor(year)) %>% 
  select(year, gdp, pop, urban, gdp_percap) %>% 
  ggpairs(aes(color = year),size = 3,alpha = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="WORLDB~1/figure-html/unnamed-chunk-2-1.png" width="672" />


  

# Inspect each histogram --------------------------------------------------

```r
library(ggplot2)
library(dplyr)

data_filtered <- data %>%
  filter(year %in% c(1970, 2020)) %>%
  mutate(year = factor(year))

# Variables to plot
vars <- c("gdp", "pop", "urban", "gdp_percap")

# Loop over the variables and create a plot for each with medians of each time
for (var in vars) {
  # Calculate mean for each year
  median_values <- data_filtered %>%
    group_by(year) %>%
    summarise(median_value = median(.data[[var]], na.rm = TRUE))
  
  # Create density plot
  p <- ggplot(data_filtered, aes_string(x = var, fill = "year")) +
    geom_histogram(data = subset(data_filtered, year == 1970), alpha = 1) +
    geom_histogram(data = subset(data_filtered, year == 2020), alpha = 0.6) +
    geom_vline(data = subset(median_values, year == "1970"), 
               aes(xintercept = median_value), 
               linetype = "dashed", color = "red", size = 1) +
    geom_vline(data = subset(median_values, year == "2020"), 
               aes(xintercept = median_value), 
               linetype = "dashed", color = "aquamarine", size = 1) +
    labs(title = paste("Histogram of", var, "for years 1970 and 2020"),
         x = var,
         y = "frequency",
         fill = "Year") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))
  
  # If variable is 'pop' or 'gdp', use scientific notation for x-axis
  if (var %in% c("pop", "gdp")) {
    p <- p + scale_x_continuous(labels = scales::scientific)
  }
  p <- p +
    annotate("text", x = median_values$median_value[1], 
             y = 90, label = round(median_values$median_value[1],0), 
             vjust = 1.5, hjust = 0, color = "red") +
    annotate("text", x = round(median_values$median_value[2],0), 
             y = 60, label = round(median_values$median_value[2],0), 
             vjust = 1.5, hjust = 0, color = "cyan")
  # Print the plot
  print(p)
}
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="WORLDB~1/figure-html/unnamed-chunk-3-1.png" width="672" />

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="WORLDB~1/figure-html/unnamed-chunk-3-2.png" width="672" />

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="WORLDB~1/figure-html/unnamed-chunk-3-3.png" width="672" />

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="WORLDB~1/figure-html/unnamed-chunk-3-4.png" width="672" />



observations: gdp, gdp percap, pop are very right skewed --> there are many poor countries, given more time I'd remove outliers to investigate in more detail

#Unsupervised clustering
Now that we have a sense of the data, let's see how different algorithms would group these countries
library(cluster)
library(factoextra)


```r
# hierarchichal clustering start ------------------------------------------


# Filter data for the year 1970 and 2020
data_1970 <- data %>% filter(year == 1970)
data_2020 <- data %>% filter(year == 2020)

# Keep only necessary variables and handle NA values
data_1970_numeric <- na.omit(data_1970[, c("country", "gdp_percap", "gdp", "pop", "urban")])
data_2020_numeric <- na.omit(data_2020[, c("country", "gdp_percap", "gdp", "pop", "urban")])

# Check if the NA removal now leaves an adequate number of countries --> Looks ok
print(dim(data_1970_numeric))
```

```
## [1] 126   5
```

```r
print(dim(data_2020_numeric))
```

```
## [1] 203   5
```

```r
# Extract the country names so that we can label our graphs later on
country_names_1970 <- data_1970_numeric$country
country_names_2020 <- data_2020_numeric$country

# Remove the country names from the numeric data frames
data_1970_numeric_values <- data_1970_numeric[, !(names(data_1970_numeric) %in% c("country"))]
data_2020_numeric_values <- data_2020_numeric[, !(names(data_2020_numeric) %in% c("country"))]

# Scale the data
data_1970_scaled <- scale(data_1970_numeric_values)
data_2020_scaled <- scale(data_2020_numeric_values)

# Hierarchical Clustering
dist_matrix_1970 <- dist(data_1970_scaled)
hc_1970 <- hclust(dist_matrix_1970, method = "ward.D2")

dist_matrix_2020 <- dist(data_2020_scaled)
hc_2020 <- hclust(dist_matrix_2020, method = "ward.D2")
```



```r
# Plot the dendrograms --> way too many, it looks like there's a lot of variation in the data. 
plot(hc_1970, main = "Hierarchical Clustering for 1970", labels = country_names_1970,cex = 0.7)
```

<img src="WORLDB~1/figure-html/unnamed-chunk-5-1.png" width="672" />

```r
plot(hc_2020, main = "Hierarchical Clustering for 2020", labels = country_names_2020,cex = 0.7)
```

<img src="WORLDB~1/figure-html/unnamed-chunk-5-2.png" width="672" />



```r
# Choose a number of clusters to simplify the analysis
n_clusters <- 5  # Replace with the number of clusters you decide on

# Cut the dendrogram and get cluster assignments
clusters_1970 <- cutree(hc_1970, k = n_clusters)
clusters_2020 <- cutree(hc_2020, k = n_clusters)

# Add cluster assignments back to the original data
data_1970_numeric$cluster <- clusters_1970
data_2020_numeric$cluster <- clusters_2020

# View the cluster assignments for each country
head(data_1970_numeric)
```

```
##        country gdp_percap         gdp      pop  urban cluster
## 1  Afghanistan   162.6422  1748886596 10752971 11.643       1
## 3      Algeria   352.5310  4863487493 13795915 39.500       1
## 5      Andorra  3958.6710    78619206    19860 80.155       2
## 8    Argentina  1324.6853 31584210366 23842803 78.880       2
## 11   Australia  3304.6787 41331616082 12507000 83.999       2
## 12     Austria  2058.7691 15373005557  7467086 65.258       2
```

```r
head(data_2020_numeric)
```

```
##          country gdp_percap          gdp      pop  urban cluster
## 1    Afghanistan   516.8666  20143442151 38972230 26.026       1
## 2        Albania  5332.1605  15131866271  2837849 62.112       2
## 3        Algeria  3337.2525 145009181491 43451666 73.733       2
## 4 American Samoa 15501.5263    716000000    46189 87.153       2
## 5        Andorra 37207.4939   2891022273    77700 87.916       3
## 6         Angola  1603.9935  53619073505 33428486 66.825       2
```



```r
# Create subset of 2020 data
data_2020_numeric_subset <- data_2020_numeric[data_2020_numeric$country %in% data_1970_numeric$country, ]

# Add year indicator
data_1970_numeric$year <- "1970"
data_2020_numeric_subset$year <- "2020"

# Merge datasets
data_combined <- rbind(data_1970_numeric, data_2020_numeric_subset)
```



```r
# Create scatter plot --> try shape of dots to distinguish groups and color to distinguish year --> a bit unclear
ggplot(data_combined, aes(x = gdp_percap, y = pop, color = year)) +
  geom_point(aes(shape = as.factor(cluster)),size = 1) +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5, size = 2) +
  scale_color_manual(values = c("1970" = "grey", "2020" = "red")) +
  labs(x = "GDP per capita", y = "Population, total", title = "1970 vs 2020 data") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-8-1.png" width="672" />



```r
# Create scatter plot --> Let's try use colour for group and use greyscale to distinguish year --> this is a bit better but a bit crowded.
ggplot(data_combined, aes(x = gdp_percap, y = pop, color = as.factor(cluster), alpha = year)) +
  geom_point(size = 1) +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5, size = 2) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "GDP per capita", y = "Population, total", title = "1970 vs 2020 data") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-9-1.png" width="672" />



```r
# Let's take Highlights from each cluster --------------------------------------------


top_countries_each_cluster <- data_combined %>%
  filter(year == 1970) %>% 
  group_by(cluster) %>%
  slice(1:5) 

top_countries_each_cluster_2020 <- data_combined %>% 
  filter(year == 2020)

# Join the data frames on both 'country' and 'year' fields
subset_merged <- top_countries_each_cluster %>%
  bind_rows(top_countries_each_cluster_2020 %>%
              semi_join(top_countries_each_cluster, by = "country"))

# Now create the plot --> gives us a better sense of how groups have changed over time
ggplot(subset_merged, aes(x = gdp_percap, y = pop, color = as.factor(cluster), alpha = factor(year), group = country)) +
  geom_point() +
  geom_line(aes(group = country), alpha = 0.5, color = "grey") +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "GDP per capita", y = "Population, total", title = "First 3 countries in each cluster in 1970 vs 2020") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-10-1.png" width="672" />



```r
#let's try others --> GDP/capita vs Urban Population over time --> we can see greyer countries at the bottom
ggplot(subset_merged, aes(x = gdp_percap, y = urban, color = as.factor(cluster), alpha = factor(year), group = country)) +
  geom_point() +
  geom_line(aes(group = country), alpha = 0.5, color = "grey") +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "GDP per capita", y = "Urban Population %", title = "~3 countries in each cluster in 1970 vs 2020") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-11-1.png" width="672" />



```r
#Total population vs urban population
ggplot(subset_merged, aes(x = pop, y = urban, color = as.factor(cluster), alpha = factor(year), group = country)) +
  geom_point() +
  geom_line(aes(group = country), alpha = 0.5, color = "grey") +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "Total Population", y = "Urban Population", title = "First 3 countries in each cluster in 1970 vs 2020") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-12-1.png" width="672" />



```r
#GDP vs urban population
ggplot(subset_merged, aes(x = gdp, y = urban, color = as.factor(cluster), alpha = factor(year), group = country)) +
  geom_point() +
  geom_line(aes(group = country), alpha = 0.5, color = "grey") +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "GDP", y = "Urban Population", title = "First 3 countries in each cluster in 1970 vs 2020") +
  theme_minimal()
```

<img src="WORLDB~1/figure-html/unnamed-chunk-13-1.png" width="672" />



```r
#GDP vs GDP per capita
ggplot(subset_merged, aes(x = gdp, y = gdp_percap, color = as.factor(cluster), alpha = factor(year), group = country)) +
  geom_point() +
  geom_line(aes(group = country), alpha = 0.5, color = "grey") +
  geom_text(aes(label = country), check_overlap = TRUE, vjust = 1.5) +
  scale_alpha_manual(values = c("1970" = 0.3, "2020" = 1)) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "GDP", y = "GDP_per cap", title = "First 3 countries in each cluster in 1970 vs 2020") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

<img src="WORLDB~1/figure-html/unnamed-chunk-14-1.png" width="672" />

```r
# hierarchical clustering end ---------------------------------------------
```





```r
# k-mean clustering start -------------------------------------------------


# Try k-means with different numbers of clusters and plot total within sum of squares
wss <- numeric(15)
for (k in 1:15) {
  km_1970 <- kmeans(data_1970_scaled, centers = k)
  wss[k] <- km_1970$tot.withinss
}
plot(1:15, wss, type = "b", xlab = "Number of clusters", ylab = "Total within-cluster sum of squares")
```

<img src="WORLDB~1/figure-html/unnamed-chunk-15-1.png" width="672" />

```r
# Visualize the clusters using a scatterplot of the first two principal components --> didn't have neough time to finish this
km_1970 <- kmeans(data_1970_scaled, centers = 4)  # Replace 3 with the number of clusters you decide on
pcomp <- prcomp(data_1970_scaled)
df <- as.data.frame(pcomp$x[,1:2])
df$cluster <- km_1970$cluster
colnames(df) <- c("PC1", "PC2", "Cluster")
ggplot(df, aes(PC1, PC2, color = as.factor(Cluster))) +
  geom_point() +
  labs(color = "Cluster")
```

<img src="WORLDB~1/figure-html/unnamed-chunk-15-2.png" width="672" />



