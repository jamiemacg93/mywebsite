---
title: "Mass shootings"
date: "2023-06-16"
description: Mass shootings in the US
draft: no
image: guns.jpg
keywords: ''
slug: shoot
categories:
- ''
- ''
---




# Mass shootings in the US

In July 2012, in the aftermath of a mass shooting in a movie theater in Aurora, Colorado, [Mother Jones](https://www.motherjones.com/politics/2012/07/mass-shootings-map/) published a report on mass shootings in the United States since 1982. Importantly, they provided the underlying data set as [an open-source database](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/) for anyone interested in studying and understanding this criminal behavior.

## Obtain the data


```
## Rows: 125
## Columns: 14
## $ case                 <chr> "Oxford High School shooting", "San Jose VTA shoo…
## $ year                 <dbl> 2021, 2021, 2021, 2021, 2021, 2021, 2020, 2020, 2…
## $ month                <chr> "Nov", "May", "Apr", "Mar", "Mar", "Mar", "Mar", …
## $ day                  <dbl> 30, 26, 15, 31, 22, 16, 16, 26, 10, 6, 31, 4, 3, …
## $ location             <chr> "Oxford, Michigan", "San Jose, California", "Indi…
## $ summary              <chr> "Ethan Crumbley, a 15-year-old student at Oxford …
## $ fatalities           <dbl> 4, 9, 8, 4, 10, 8, 4, 5, 4, 3, 7, 9, 22, 3, 12, 5…
## $ injured              <dbl> 7, 0, 7, 1, 0, 1, 0, 0, 3, 8, 25, 27, 26, 12, 4, …
## $ total_victims        <dbl> 11, 9, 15, 5, 10, 9, 4, 5, 7, 11, 32, 36, 48, 15,…
## $ location_type        <chr> "School", "Workplace", "Workplace", "Workplace", …
## $ male                 <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
## $ age_of_shooter       <dbl> 15, 57, 19, NA, 21, 21, 31, 51, NA, NA, 36, 24, 2…
## $ race                 <chr> NA, NA, "White", NA, NA, "White", NA, "Black", "B…
## $ prior_mental_illness <chr> NA, "Yes", "Yes", NA, "Yes", NA, NA, NA, NA, NA, …
```

| column(variable)     | description                                                                 |
|--------------------------|----------------------------------------------|
| case                 | short name of incident                                                      |
| year, month, day     | year, month, day in which the shooting occurred                             |
| location             | city and state where the shooting occcurred                                 |
| summary              | brief description of the incident                                           |
| fatalities           | Number of fatalities in the incident, excluding the shooter                 |
| injured              | Number of injured, non-fatal victims in the incident, excluding the shooter |
| total_victims        | number of total victims in the incident, excluding the shooter              |
| location_type        | generic location in which the shooting took place                           |
| male                 | logical value, indicating whether the shooter was male                      |
| age_of_shooter       | age of the shooter when the incident occured                                |
| race                 | race of the shooter                                                         |
| prior_mental_illness | did the shooter show evidence of mental illness prior to the incident?      |

## Explore the data

### Specific questions

-   Generate a data frame that summarizes the number of mass shootings per year.


```r
yearly_shootings <- mass_shootings %>% 
  group_by(year) %>% 
  summarise(count = n())
```

-   Generate a bar chart that identifies the number of mass shooters associated with each race category. The bars should be sorted from highest to lowest and each bar should show its number.


```r
#first let's count race 
race_counts <- mass_shootings %>% 
  na.omit() %>% 
  group_by(race) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

print(race_counts)
```

```
## # A tibble: 5 × 2
##   race            count
##   <chr>           <int>
## 1 White              48
## 2 Black              12
## 3 Asian               7
## 4 Latino              6
## 5 Native American     1
```

```r
#let's then plot this information, by far the majority are White
ggplot(race_counts, aes(x = reorder(race, -count), y = count)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Race", y = "Number of Shooters", title = "Number of Mass Shooters by race") + 
  theme_minimal()
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-3-1.png" width="672" />

-   Generate a boxplot visualizing the number of total victims, by type of location.


```r
ggplot(mass_shootings, aes(x = location_type, y = total_victims)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Type of Location", y = "Total Victims", title = "Total Victims by Type of Location") +
  theme_minimal()
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-4-1.png" width="672" />

-   Redraw the same plot, but remove the Las Vegas Strip massacre from the dataset.


```r
#evidently there is a huge outlier which is the Las Vegas Strip massacre so let's just exclude that datapoint manually
no_outlier_shootings <- mass_shootings %>% 
  filter(total_victims <200)

ggplot(no_outlier_shootings, aes(x = location_type, y = total_victims)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Type of Location", y = "Total Victims", title = "Total Victims by Type of Location") +
  theme_minimal()
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-5-1.png" width="672" />

### More open-ended questions

Address the following questions. Generate appropriate figures/tables to support your conclusions.

-   How many white males with prior signs of mental illness initiated a mass shooting after 2000?


```r
#let's filter the data to match the criteria and then count the number of rows

mental_illness_shootings <- mass_shootings %>%
  filter(prior_mental_illness == "Yes", year > 2000, race == "White", male == TRUE) %>% 
  summarise(count = n())

print(mental_illness_shootings)
```

```
## # A tibble: 1 × 1
##   count
##   <int>
## 1    22
```

-   Which month of the year has the most mass shootings? Generate a bar chart sorted in chronological (natural) order (Jan-Feb-Mar- etc) to provide evidence of your answer.


```r
#first count the number of shootings each month chronogically
danger_month <- mass_shootings %>% 
  group_by(month) %>% 
  summarise(count = n())

#probably there is a more elegant way to do it but I'm going to manually label each month's order and add a column to the dataframe then reorder
danger_month <- danger_month %>% 
  mutate(month_order = case_when(
    month =="Jan" ~"1",
    month =="Feb" ~"2",
    month =="Mar" ~"3",
    month =="Apr" ~"4",
    month =="May" ~"5",
    month =="Jun" ~"6", 
    month =="Jul" ~"7",
    month =="Aug" ~"8", 
    month =="Sep" ~"9", 
    month =="Oct" ~"10",
    month =="Nov" ~"11", 
    month =="Dec" ~"12",
    TRUE ~ ""
  ))
#new column is going to be a string so let's convert it to a number
danger_month <- danger_month %>%  
  mutate(month_order = as.numeric(month_order))

#now we can reorder it
danger_month <- danger_month %>% 
  arrange(month_order)
#check that the ordering is now correct --> yes it is. 
print(danger_month)
```

```
## # A tibble: 12 × 3
##    month count month_order
##    <chr> <int>       <dbl>
##  1 Jan       7           1
##  2 Feb      13           2
##  3 Mar      12           3
##  4 Apr      11           4
##  5 May       8           5
##  6 Jun      12           6
##  7 Jul      10           7
##  8 Aug       8           8
##  9 Sep      10           9
## 10 Oct      11          10
## 11 Nov      12          11
## 12 Dec      11          12
```



```r
#now we print ggplot, looks like February has the most, but not by much, especially this is over 30 years.
ggplot(danger_month, aes(x = reorder(month, month_order) , y = count)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Month", y = "Number of Shootings", title = "Number of Shootings by Month") + 
  theme_minimal()
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-8-1.png" width="672" />

-   How does the distribution of mass shooting fatalities differ between White and Black shooters? What about White and Latino shooters?


```r
str(mass_shootings)
```

```
## spc_tbl_ [125 × 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ case                : chr [1:125] "Oxford High School shooting" "San Jose VTA shooting" "FedEx warehouse shooting" "Orange office complex shooting" ...
##  $ year                : num [1:125] 2021 2021 2021 2021 2021 ...
##  $ month               : chr [1:125] "Nov" "May" "Apr" "Mar" ...
##  $ day                 : num [1:125] 30 26 15 31 22 16 16 26 10 6 ...
##  $ location            : chr [1:125] "Oxford, Michigan" "San Jose, California" "Indianapolis, Indiana" "Orange, California" ...
##  $ summary             : chr [1:125] "Ethan Crumbley, a 15-year-old student at Oxford High School, opened fire with a Sig Sauer 9mm pistol purchased "| __truncated__ "Samuel Cassidy, 57, a Valley Transportation Authorty employee, opened fire at a union meeting at the light rail"| __truncated__ "Brandon Scott Hole, 19, opened fire around 11 p.m. in the parking lot and inside the warehouse, and then shot h"| __truncated__ "Aminadab Gaxiola Gonzalez, 44, allegedly opened fire inside a small business at an office complex, killing at l"| __truncated__ ...
##  $ fatalities          : num [1:125] 4 9 8 4 10 8 4 5 4 3 ...
##  $ injured             : num [1:125] 7 0 7 1 0 1 0 0 3 8 ...
##  $ total_victims       : num [1:125] 11 9 15 5 10 9 4 5 7 11 ...
##  $ location_type       : chr [1:125] "School" "Workplace" "Workplace" "Workplace" ...
##  $ male                : logi [1:125] TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ age_of_shooter      : num [1:125] 15 57 19 NA 21 21 31 51 NA NA ...
##  $ race                : chr [1:125] NA NA "White" NA ...
##  $ prior_mental_illness: chr [1:125] NA "Yes" "Yes" NA ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   case = col_character(),
##   ..   year = col_double(),
##   ..   month = col_character(),
##   ..   day = col_double(),
##   ..   location = col_character(),
##   ..   summary = col_character(),
##   ..   fatalities = col_double(),
##   ..   injured = col_double(),
##   ..   total_victims = col_double(),
##   ..   location_type = col_character(),
##   ..   male = col_logical(),
##   ..   age_of_shooter = col_double(),
##   ..   race = col_character(),
##   ..   prior_mental_illness = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
#we'll just tell ggplot to fiter within ggpot
fatalities_white <- ggplot(mass_shootings[mass_shootings$race == "White",], aes(x = fatalities)) +
  geom_histogram(fill = "steelblue", color = "white") + 
  labs(
    x = "Fatalities per Shooting",
    y = "Fatality frequency",
    title = "Histogram of Fatalities by White"
    ) + 
  xlim(0,120)

fatalities_black <- ggplot(mass_shootings[mass_shootings$race == "Black",], aes(x = fatalities)) +
  geom_histogram(fill = "steelblue", color = "white") + 
  labs(
    x = "Fatalities per Shooting",
    y = "Fatality frequency",
    title = "Histogram of Fatalities by Black"
    ) + 
  xlim(0,15)

print(fatalities_black)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 11 rows containing non-finite values (`stat_bin()`).
```

```
## Warning: Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
print(fatalities_white)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 11 rows containing non-finite values (`stat_bin()`).
## Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-9-2.png" width="672" />



```r
#let's look at the graphs, black people have far lower frequency, and lower fatalities per shooting
print(fatalities_white)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 11 rows containing non-finite values (`stat_bin()`).
```

```
## Warning: Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-10-1.png" width="672" />

```r
print(fatalities_black)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 11 rows containing non-finite values (`stat_bin()`).
## Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-10-2.png" width="672" />

### Very open-ended

-   Are mass shootings with shooters suffering from mental illness different from mass shootings with no signs of mental illness in the shooter?


```r
#remind ourselves how the database is organised
str(mass_shootings)
```

```
## spc_tbl_ [125 × 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ case                : chr [1:125] "Oxford High School shooting" "San Jose VTA shooting" "FedEx warehouse shooting" "Orange office complex shooting" ...
##  $ year                : num [1:125] 2021 2021 2021 2021 2021 ...
##  $ month               : chr [1:125] "Nov" "May" "Apr" "Mar" ...
##  $ day                 : num [1:125] 30 26 15 31 22 16 16 26 10 6 ...
##  $ location            : chr [1:125] "Oxford, Michigan" "San Jose, California" "Indianapolis, Indiana" "Orange, California" ...
##  $ summary             : chr [1:125] "Ethan Crumbley, a 15-year-old student at Oxford High School, opened fire with a Sig Sauer 9mm pistol purchased "| __truncated__ "Samuel Cassidy, 57, a Valley Transportation Authorty employee, opened fire at a union meeting at the light rail"| __truncated__ "Brandon Scott Hole, 19, opened fire around 11 p.m. in the parking lot and inside the warehouse, and then shot h"| __truncated__ "Aminadab Gaxiola Gonzalez, 44, allegedly opened fire inside a small business at an office complex, killing at l"| __truncated__ ...
##  $ fatalities          : num [1:125] 4 9 8 4 10 8 4 5 4 3 ...
##  $ injured             : num [1:125] 7 0 7 1 0 1 0 0 3 8 ...
##  $ total_victims       : num [1:125] 11 9 15 5 10 9 4 5 7 11 ...
##  $ location_type       : chr [1:125] "School" "Workplace" "Workplace" "Workplace" ...
##  $ male                : logi [1:125] TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ age_of_shooter      : num [1:125] 15 57 19 NA 21 21 31 51 NA NA ...
##  $ race                : chr [1:125] NA NA "White" NA ...
##  $ prior_mental_illness: chr [1:125] NA "Yes" "Yes" NA ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   case = col_character(),
##   ..   year = col_double(),
##   ..   month = col_character(),
##   ..   day = col_double(),
##   ..   location = col_character(),
##   ..   summary = col_character(),
##   ..   fatalities = col_double(),
##   ..   injured = col_double(),
##   ..   total_victims = col_double(),
##   ..   location_type = col_character(),
##   ..   male = col_logical(),
##   ..   age_of_shooter = col_double(),
##   ..   race = col_character(),
##   ..   prior_mental_illness = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
#OK I'm just going to look at quantitative data and practice overlaying histograms at the same time. 

#let's try create an overlay of distributions along a few variables. First let's save the filtered df's with mental illness vs no mental illness

mentally_ill_shooters <- mass_shootings %>% 
  filter(prior_mental_illness == "Yes")


mentally_healthy_shooters <- mass_shootings %>% 
  filter(prior_mental_illness == "No")
```



```r
#let's first see how fatalities differ: there are fewer "healthy" shooters and there are fewer shootings which have double digit fatalities
ggplot() +
  geom_histogram(data = mentally_ill_shooters, aes(x = fatalities), fill = "steelblue", alpha = 0.5, binwidth = 1) +
  geom_histogram(data = mentally_healthy_shooters, aes(x = fatalities), fill = "red", alpha = 0.5, binwidth = 1) +
  labs(
    x = "Fatalities per shootings",
    y = "Frequency",
    title = "Overlay of fatality histograms"
  ) +
  xlim(0, 120)
```

```
## Warning: Removed 2 rows containing missing values (`geom_bar()`).
## Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
#let's look at injured next: again fewer injuries and fewer high double digit injuries so in general we can generalise that mentally ill shootings are more common and more deadly
ggplot() +
  geom_histogram(data = mentally_ill_shooters, aes(x = injured), fill = "steelblue", alpha = 0.5, binwidth = 1) +
  geom_histogram(data = mentally_healthy_shooters, aes(x = injured), fill = "red", alpha = 0.5, binwidth = 1) +
  labs(
    x = "Injuries per shooting",
    y = "Frequency",
    title = "Overlay of injury histograms"
  ) +
  xlim(0, 120)
```

```
## Warning: Removed 2 rows containing missing values (`geom_bar()`).
## Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-12-2.png" width="672" />

```r
#finally let's look at the age --> no obvious differences
ggplot() +
  geom_histogram(data = mentally_ill_shooters, aes(x = age_of_shooter), fill = "steelblue", alpha = 0.5, binwidth = 1) +
  geom_histogram(data = mentally_healthy_shooters, aes(x = age_of_shooter), fill = "red", alpha = 0.5, binwidth = 1) +
  labs(
    x = "Age",
    y = "Frequency",
    title = "Overlay of injury histograms"
  ) +
  xlim(0, 120)
```

```
## Warning: Removed 2 rows containing missing values (`geom_bar()`).
## Removed 2 rows containing missing values (`geom_bar()`).
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-12-3.png" width="672" />

-   Assess the relationship between mental illness and total victims, mental illness and location type, and the intersection of all three variables.


```r
#we're just going to visually inspect this: visually, we can see there are more shootings (dots) with prior mentally ill shooters,
#we can see there are more shootings with high number of victims
#we can see thre are relatively more shootings in the "other" location types for non-ill shooters, suggesting more predictability by place
# we can see religious airport and military settings are relatively uncommon, we can see that school shootings are more dangerous on average than workplace settings
# we can see that non-mentally ill shooters are relatively more concentrated in schools than in other location types (apart from "other") 
ggplot(mass_shootings, aes(x = total_victims, y = location_type, color = prior_mental_illness)) + 
  geom_point(data = na.omit(mass_shootings)) + 
  labs(
    x = "total victims",
    y = "prior mental illness",
    title = "total victims, mental illness and location type"
  ) +
  xlim(0,120)
```

<img src="/blogs/homework2A_files/figure-html/unnamed-chunk-13-1.png" width="672" />

