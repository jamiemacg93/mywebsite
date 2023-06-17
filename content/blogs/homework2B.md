---
categories:  
- ""    #the front matter should be like the one found in, e.g., blog2.md. It cannot be like the normal Rmd we used
- ""
date: "2023-06-16"
description: credit card fraud # the title that will show up once someone gets to this page
draft: false
image: card.jpg # save picture in \static\img\blogs. Acceptable formats= jpg, jpeg, or png . Your iPhone pics wont work

keywords: ""
slug: card_fraud # slug is the shorthand URL address... no spaces plz
title: Card Fraud
---

# Exploring credit card fraud

We will be using a dataset with credit card transactions containing legitimate and fraud transactions. Fraud is typically well below 1% of all transactions, so a naive model that predicts that all transactions are legitimate and not fraudulent would have an accuracy of well over 99%-- pretty good, no? (well, not quite as we will see later in the course)

You can read more on credit card fraud on [Credit Card Fraud Detection Using Weighted Support Vector Machine](https://www.scirp.org/journal/paperinformation.aspx?paperid=105944)

The dataset we will use consists of credit card transactions and it includes information about each transaction including customer details, the merchant and category of purchase, and whether or not the transaction was a fraud.
##load libraries


## Obtain the data

The dataset is too large to be hosted on Canvas or Github, so please download it from dropbox https://www.dropbox.com/sh/q1yk8mmnbbrzavl/AAAxzRtIhag9Nc_hODafGV2ka?dl=0 and save it in your `dsb` repo, under the `data` folder


```
## Rows: 671,028
## Columns: 14
## $ trans_date_trans_time <dttm> 2019-02-22 07:32:58, 2019-02-16 15:07:20, 2019-…
## $ trans_year            <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, …
## $ category              <chr> "entertainment", "kids_pets", "personal_care", "…
## $ amt                   <dbl> 7.79, 3.89, 8.43, 40.00, 54.04, 95.61, 64.95, 3.…
## $ city                  <chr> "Veedersburg", "Holloway", "Arnold", "Apison", "…
## $ state                 <chr> "IN", "OH", "MO", "TN", "CO", "GA", "MN", "AL", …
## $ lat                   <dbl> 40.1186, 40.0113, 38.4305, 35.0149, 39.4584, 32.…
## $ long                  <dbl> -87.2602, -80.9701, -90.3870, -85.0164, -106.385…
## $ city_pop              <dbl> 4049, 128, 35439, 3730, 277, 1841, 136, 190178, …
## $ job                   <chr> "Development worker, community", "Child psychoth…
## $ dob                   <date> 1959-10-19, 1946-04-03, 1985-03-31, 1991-01-28,…
## $ merch_lat             <dbl> 39.41679, 39.74585, 37.73078, 34.53277, 39.95244…
## $ merch_long            <dbl> -87.52619, -81.52477, -91.36875, -84.10676, -106…
## $ is_fraud              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

The data dictionary is as follows

| column(variable)      | description                                 |
|-----------------------|---------------------------------------------|
| trans_date_trans_time | Transaction DateTime                        |
| trans_year            | Transaction year                            |
| category              | category of merchant                        |
| amt                   | amount of transaction                       |
| city                  | City of card holder                         |
| state                 | State of card holder                        |
| lat                   | Latitude location of purchase               |
| long                  | Longitude location of purchase              |
| city_pop              | card holder's city population               |
| job                   | job of card holder                          |
| dob                   | date of birth of card holder                |
| merch_lat             | Latitude Location of Merchant               |
| merch_long            | Longitude Location of Merchant              |
| is_fraud              | Whether Transaction is Fraud (1) or Not (0) |

-   In this dataset, how likely are fraudulent transactions? Generate a table that summarizes the number and frequency of fraudulent transactions per year.


```r
fraud_frequency <- card_fraud %>% 
  group_by(is_fraud) %>% 
  summarise(count = n())
#not sure how to efficiently add the column in one go so I'll do it in a few steps, starting by summing the count
fraud_count <- sum(fraud_frequency$count)
#then create a column that inserts the % frequency
fraud_frequency <- fraud_frequency %>% 
  mutate(percent_frequency = ifelse (is_fraud == 1,100*count/fraud_count,100*count/fraud_count))
#check the value: so fraud happen in just over half a percent of transcations
print(fraud_frequency)
```

```
## # A tibble: 2 × 3
##   is_fraud  count percent_frequency
##      <dbl>  <int>             <dbl>
## 1        0 667092            99.4  
## 2        1   3936             0.587
```

-   How much money (in US\$ terms) are fraudulent transactions costing the company? Generate a table that summarizes the total amount of legitimate and fraudulent transactions per year and calculate the % of fraudulent transactions, in US\$ terms.


```r
#let's create the fraud indicator first
fraud_indicator <- card_fraud %>% 
  group_by(is_fraud) 
#then create a total transaction value amount
summary_data <- fraud_indicator %>% 
  summarise(total_amount = sum(amt))

#not sure how to efficiently add the column in one go so I'll do it in a few steps, starting by summing the transaction value
total_amount_both <- sum(summary_data$total_amount)
#then create a column that inserts the % frequency
summary_data <- summary_data %>% 
  mutate(percent_frequency = ifelse (is_fraud == 1,100*total_amount/total_amount_both,100*total_amount/total_amount_both))
#check the value: so even though fraud only accounts for 0.5% of cases, it accounts for 4.39 of total transaction value a 10x increase!
print(summary_data)
```

```
## # A tibble: 2 × 3
##   is_fraud total_amount percent_frequency
##      <dbl>        <dbl>             <dbl>
## 1        0    45108815.             95.6 
## 2        1     2075089.              4.40
```

-   Generate a histogram that shows the distribution of amounts charged to credit card, both for legitimate and fraudulent accounts. Also, for both types of transactions, calculate some quick summary statistics.


```r
#first let's store the data for different fraud states
is_fraud_histo <- card_fraud %>% 
  filter(is_fraud == 1)

is_not_fraud_histo <- card_fraud %>% 
  filter(is_fraud == 0)
#then let's overlay the histograms so we can compare directly, actually to do this effectively I'll use density plot instead
ggplot() +
  geom_density(data = is_fraud_histo, aes(x = amt), fill = "steelblue", alpha = 0.5) +
  geom_density(data = is_not_fraud_histo, aes(x = amt), fill = "red", alpha = 0.5) +
  labs(
    x = "Transaction value",
    y = "Frequency",
    title = "Overlay of transaction value by fraudulent and non-fradulent transactions"
  ) +
  xlim(0,1500)
```

```
## Warning: Removed 680 rows containing non-finite values (`stat_density()`).
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-5-1.png" width="672" />

```r
#summary stats for fraud
mean(is_fraud_histo$amt)
```

```
## [1] 527.2076
```

```r
median(is_fraud_histo$amt)
```

```
## [1] 368.83
```

```r
sd(is_fraud_histo$amt)
```

```
## [1] 391.2915
```

```r
min(is_fraud_histo$amt)
```

```
## [1] 1.06
```

```r
max(is_fraud_histo$amt)
```

```
## [1] 1334.07
```

```r
#summary stats for not fraud
mean(is_not_fraud_histo$amt)
```

```
## [1] 67.62008
```

```r
median(is_not_fraud_histo$amt)
```

```
## [1] 47.17
```

```r
sd(is_not_fraud_histo$amt)
```

```
## [1] 155.2949
```

```r
min(is_not_fraud_histo$amt)
```

```
## [1] 1
```

```r
max(is_not_fraud_histo$amt)
```

```
## [1] 27119.77
```

-   What types of purchases are most likely to be instances of fraud? Consider category of merchants and produce a bar chart that shows % of total fraudulent transactions sorted in order.


```r
#create the table first
fraud_by_category <- card_fraud %>%
  group_by(category) %>%
  summarize(fraud_percentage = sum(is_fraud == 1) /n()) %>%
  mutate(fraud_percentage = fraud_percentage * 100) %>%
  arrange(desc(fraud_percentage))
#reorder it
fraud_by_category <- fraud_by_category %>%
  mutate(category = reorder(category, -fraud_percentage))

# Create a bar chart showing the percentages of total fraudulent transactions
ggplot(data = fraud_by_category, aes(x = category,  y = fraud_percentage, fill = category)) +
  geom_bar(stat = "identity") +
  xlab("Category of Merchants") +
  ylab("% of Total Fraudulent Transactions") +
  ggtitle("Fraudulent Transactions by Category of Merchants") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Category of Merchants")
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-6-1.png" width="672" />

-   When is fraud more prevalent? Which days, months, hours? To create new variables to help you in your analysis, we use the `lubridate` package and the following code

```         
mutate(
  date_only = lubridate::date(trans_date_trans_time),
  month_name = lubridate::month(trans_date_trans_time, label=TRUE),
  hour = lubridate::hour(trans_date_trans_time),
  weekday = lubridate::wday(trans_date_trans_time, label = TRUE)
  )
```

-   Are older customers significantly more likely to be victims of credit card fraud? To calculate a customer's age, we use the `lubridate` package and the following code

```         
  mutate(
   age = interval(dob, trans_date_trans_time) / years(1),
    )
```


```r
library(lubridate)

# Create new variables for analysis
card_fraud <- card_fraud %>%
  mutate(
    date_only = date(trans_date_trans_time),
    month_name = month(trans_date_trans_time, label = TRUE),
    hour = hour(trans_date_trans_time),
    weekday = wday(trans_date_trans_time, label = TRUE)
  )

fraud_by_weekday <- card_fraud %>%
  group_by(weekday) %>%
  summarize(fraud_count = sum(is_fraud == 1), total_count = n()) %>%
  mutate(fraud_percentage = fraud_count / total_count * 100)

# Sort the weekdays in the correct order
fraud_by_weekday$weekday <- factor(fraud_by_weekday$weekday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

# Create a bar chart to visualize the prevalence of fraud by weekday, thursday much more likely than sunday (about 40% more common)
ggplot(data = fraud_by_weekday, aes(x = weekday, y = fraud_percentage, fill = weekday)) +
  geom_bar(stat = "identity") +
  xlab("Weekday") +
  ylab("% of Fraudulent Transactions") +
  ggtitle("Prevalence of Fraud by Weekday")
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r
#now by month
fraud_by_month <- card_fraud %>%
  group_by(month_name) %>%
  summarize(fraud_count = sum(is_fraud == 1), total_count = n()) %>%
  mutate(fraud_percentage = fraud_count / total_count * 100)

# Create a bar chart to visualize the prevalence of fraud by month, Jan and Feb ~50% more frequent than July or December
ggplot(data = fraud_by_month, aes(x = month_name, y = fraud_percentage, fill = month_name)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("% of Fraudulent Transactions") +
  ggtitle("Prevance of Fraud by Month")
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-7-2.png" width="672" />

```r
#now by hour
fraud_by_hour <- card_fraud %>%
  group_by(hour) %>%
  summarize(fraud_count = sum(is_fraud == 1), total_count = n()) %>%
  mutate(fraud_percentage = fraud_count / total_count * 100)

# Create a line plot to visualize the prevalence of fraud by hour, far more likely to happen late in the evening 10 - 30 times more likely.  
ggplot(data = fraud_by_hour, aes(x = hour, y = fraud_percentage)) +
  geom_bar(stat = "identity") +
  xlab("Hour") +
  ylab("% of Fraudulent Transactions") +
  ggtitle("Prevalence of Fraud by Hour")
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-7-3.png" width="672" />

-   Is fraud related to distance? The distance between a card holder's home and the location of the transaction can be a feature that is related to fraud. To calculate distance, we need the latidue/longitude of card holders's home and the latitude/longitude of the transaction, and we will use the [Haversine formula](https://en.wikipedia.org/wiki/Haversine_formula) to calculate distance. I adapted code to [calculate distance between two points on earth](https://www.geeksforgeeks.org/program-distance-two-points-earth/amp/) which you can find below


```r
# distance between card holder's home and transaction
# code adapted from https://www.geeksforgeeks.org/program-distance-two-points-earth/amp/


card_fraud <- card_fraud %>%
  mutate(
    
    # convert latitude/longitude to radians
    lat1_radians = lat / 57.29577951,
    lat2_radians = merch_lat / 57.29577951,
    long1_radians = long / 57.29577951,
    long2_radians = merch_long / 57.29577951,
    
    # calculate distance in miles
    distance_miles = 3963.0 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians)),

    # calculate distance in km
    distance_km = 6377.830272 * acos((sin(lat1_radians) * sin(lat2_radians)) + cos(lat1_radians) * cos(lat2_radians) * cos(long2_radians - long1_radians))

  )

#plot the boxplot
ggplot(data = card_fraud, aes(x = is_fraud, y = distance_km)) +
  geom_boxplot() +
  xlab("Fraud Indicator") +
  ylab("Distance (km)") +
  ggtitle("Relationship between Distance and Fraud") +
  theme_minimal()
```

```
## Warning: Continuous x aesthetic
## ℹ did you forget `aes(group = ...)`?
```

<img src="/blogs/homework2B_files/figure-html/unnamed-chunk-8-1.png" width="672" />
