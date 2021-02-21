---
title: "NYC Dog Licsense Data"
author: "Joe DiNoto"
date: "2/20/2021"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
library(dplyr)
library(ggplot2)
```


```r
dogs <- read.csv("NYC_Dog_Licensing_Dataset.csv")
```

Get a feel for the data set

```r
glimpse(dogs)
```

```
## Rows: 345,727
## Columns: 10
## $ RowNumber          <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1...
## $ AnimalName         <chr> "PAIGE", "YOGI", "ALI", "QUEEN", "LOLA", "IAN", ...
## $ AnimalGender       <chr> "F", "M", "M", "F", "F", "M", "M", "F", "F", "M"...
## $ AnimalBirthMonth   <int> 2014, 2010, 2014, 2013, 2009, 2006, 2008, 2012, ...
## $ BreedName          <chr> "American Pit Bull Mix / Pit Bull Mix", "Boxer",...
## $ Borough            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ ZipCode            <int> 10035, 10465, 10013, 10013, 10028, 10013, 10025,...
## $ LicenseIssuedDate  <chr> "09/12/2014", "09/12/2014", "09/12/2014", "09/12...
## $ LicenseExpiredDate <chr> "09/12/2017", "10/02/2017", "09/12/2019", "09/12...
## $ Extract.Year       <int> 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, ...
```

Most pouplar breed?


```r
dogs %>%
  count(BreedName, sort=TRUE) %>%
  head()
```

```
##            BreedName     n
## 1            Unknown 38785
## 2  Yorkshire Terrier 21922
## 3           Shih Tzu 19631
## 4          Chihuahua 15647
## 5            Maltese 11391
## 6 Labrador Retriever 11327
```

Longest animal names?

```r
long_names <- dogs %>%
  mutate(name_length = nchar(AnimalName)) %>%
  filter(name_length>20) %>%
  select(AnimalName, name_length)
long_names
```

```
##                        AnimalName name_length
## 1         2008-10-22T00:00:00.000          23
## 2         2004-10-22T00:00:00.000          23
## 3         2003-10-22T00:00:00.000          23
## 4           DANGERFIELDS-MR.BOBBY          21
## 5  CARLYAPPLEWHITECRAWFORDCOLEMAN          30
## 6  SAMSONMAXWELLWALTERZANE(SAMMY)          30
## 7         2005-08-19T00:00:00.000          23
## 8         2003-08-13T00:00:00.000          23
## 9         2003-08-13T00:00:00.000          23
## 10         SHAWN-MICHAEL-VINCIENT          22
## 11 JEFFERSONBARNARDRAMSEYDONNELLY          30
## 12      BUDDYVONYANKEEDESHORTHAIR          25
## 13          EUNICETHOMPSON-STROUD          21
## 14 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 15          PANDORA MARIE COLLIER          21
## 16          FLYNN-(BILLYGSWANNBE)          21
## 17 JEFFERSONBARNARDRAMSEYDONNELLY          30
## 18          FLYNN-(BILLYGSWANNBE)          21
## 19      BUDDYVONYANKEEDESHORTHAIR          25
## 20          EUNICETHOMPSON-STROUD          21
## 21 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 22          PANDORA MARIE COLLIER          21
## 23 BUDEREAUXBUDERONIMUSBUDEROWSKI          30
## 24         SHAWN-MICHAEL-VINCIENT          22
## 25          FLYNN-(BILLYGSWANNBE)          21
## 26          EUNICETHOMPSON-STROUD          21
## 27 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 28 BUDEREAUXBUDERONIMUSBUDEROWSKI          30
## 29         SHAWN-MICHAEL-VINCIENT          22
## 30      BUDDYVONYANKEEDESHORTHAIR          25
## 31          PANDORA MARIE COLLIER          21
```


```r
dogs %>%
  mutate(name_length=nchar(AnimalName))%>%
  ggplot(aes(x=name_length))+
  geom_histogram()+
  scale_y_log10()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 444 rows containing non-finite values (stat_bin).
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Removed 5 rows containing missing values (geom_bar).
```

![](NY_dog_data_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
long_names %>%
  filter(name_length>29)
```

```
##                       AnimalName name_length
## 1 CARLYAPPLEWHITECRAWFORDCOLEMAN          30
## 2 SAMSONMAXWELLWALTERZANE(SAMMY)          30
## 3 JEFFERSONBARNARDRAMSEYDONNELLY          30
## 4 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 5 JEFFERSONBARNARDRAMSEYDONNELLY          30
## 6 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 7 BUDEREAUXBUDERONIMUSBUDEROWSKI          30
## 8 PIPLONGFELLOWBUTTERFIELDFROUDE          30
## 9 BUDEREAUXBUDERONIMUSBUDEROWSKI          30
```

