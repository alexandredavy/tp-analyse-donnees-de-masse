TP1 - Criminalité à Toronto
================
groupe B

### Chargement librairies

``` r
library(tidyverse) 
library(dsbox) 
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 4.2.2

### Chargement datasets

``` r
budget <- read_csv("data/Budget_2022.csv")
Major_Crime <- read_csv("data/Major_Crime_Indicators.csv")
properties <- read_csv("data/properties.csv")
```

### Résummer des données

``` r
summary(budget)
```

    ##   Fiscal_Year   Budget_Type        Organization_Entity Command_Name      
    ##  Min.   :2022   Length:6129        Length:6129         Length:6129       
    ##  1st Qu.:2022   Class :character   Class :character    Class :character  
    ##  Median :2022   Mode  :character   Mode  :character    Mode  :character  
    ##  Mean   :2022                                                            
    ##  3rd Qu.:2022                                                            
    ##  Max.   :2022                                                            
    ##  Pillar_Name        District_Name       Unit_Name         Feature_Category  
    ##  Length:6129        Length:6129        Length:6129        Length:6129       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   Cost_Element  Cost_Element_Long_Name     Amount             ObjectId   
    ##  Min.   :1501   Length:6129            Min.   :-43810200   Min.   :   1  
    ##  1st Qu.:1578   Class :character       1st Qu.:        0   1st Qu.:1533  
    ##  Median :2082   Mode  :character       Median :        0   Median :3065  
    ##  Mean   :3094                          Mean   :   191066   Mean   :3065  
    ##  3rd Qu.:4312                          3rd Qu.:     3400   3rd Qu.:4597  
    ##  Max.   :9453                          Max.   : 57161100   Max.   :6129

``` r
summary(Major_Crime)
```

    ##        X                  Y               Index_       event_unique_id   
    ##  Min.   :-8910331   Min.   :      0   Min.   :     1   Length:301233     
    ##  1st Qu.:-8846681   1st Qu.:5412946   1st Qu.: 75309   Class :character  
    ##  Median :-8838015   Median :5419000   Median :150617   Mode  :character  
    ##  Mean   :-8724562   Mean   :5350375   Mean   :150617                     
    ##  3rd Qu.:-8829868   3rd Qu.:5426986   3rd Qu.:225925                     
    ##  Max.   :       0   Max.   :5517228   Max.   :301233                     
    ##                                                                          
    ##    Division         occurrencedate     reporteddate       location_type     
    ##  Length:301233      Length:301233      Length:301233      Length:301233     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  premises_type         ucr_code       ucr_ext        offence         
    ##  Length:301233      Min.   :1410   Min.   :100.0   Length:301233     
    ##  Class :character   1st Qu.:1430   1st Qu.:100.0   Class :character  
    ##  Mode  :character   Median :1450   Median :100.0   Mode  :character  
    ##                     Mean   :1702   Mean   :146.6                     
    ##                     3rd Qu.:2120   3rd Qu.:200.0                     
    ##                     Max.   :2135   Max.   :230.0                     
    ##                                                                      
    ##   reportedyear  reportedmonth       reportedday    reporteddayofyear
    ##  Min.   :2014   Length:301233      Min.   : 1.00   Min.   :  1.0    
    ##  1st Qu.:2016   Class :character   1st Qu.: 8.00   1st Qu.: 92.0    
    ##  Median :2018   Mode  :character   Median :16.00   Median :178.0    
    ##  Mean   :2018                      Mean   :15.74   Mean   :180.8    
    ##  3rd Qu.:2020                      3rd Qu.:23.00   3rd Qu.:270.0    
    ##  Max.   :2022                      Max.   :31.00   Max.   :366.0    
    ##                                                                     
    ##  reporteddayofweek   reportedhour   occurrenceyear occurrencemonth   
    ##  Length:301233      Min.   : 0.00   Min.   :2000   Length:301233     
    ##  Class :character   1st Qu.: 8.00   1st Qu.:2016   Class :character  
    ##  Mode  :character   Median :13.00   Median :2018   Mode  :character  
    ##                     Mean   :12.81   Mean   :2018                     
    ##                     3rd Qu.:18.00   3rd Qu.:2020                     
    ##                     Max.   :23.00   Max.   :2022                     
    ##                                     NA's   :100                      
    ##  occurrenceday   occurrencedayofyear occurrencedayofweek occurrencehour
    ##  Min.   : 1.00   Min.   :  1.0       Length:301233       Min.   : 0.0  
    ##  1st Qu.: 8.00   1st Qu.: 92.0       Class :character    1st Qu.: 7.0  
    ##  Median :15.00   Median :178.0       Mode  :character    Median :14.0  
    ##  Mean   :15.46   Mean   :180.5                           Mean   :12.6  
    ##  3rd Qu.:23.00   3rd Qu.:270.0                           3rd Qu.:19.0  
    ##  Max.   :31.00   Max.   :366.0                           Max.   :23.0  
    ##  NA's   :100     NA's   :100                                           
    ##  mci_category         Hood_ID          Neighbourhood        Longitude     
    ##  Length:301233      Length:301233      Length:301233      Min.   :-80.04  
    ##  Class :character   Class :character   Class :character   1st Qu.:-79.47  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :-79.39  
    ##                                                           Mean   :-78.37  
    ##                                                           3rd Qu.:-79.32  
    ##                                                           Max.   :  0.00  
    ##                                                                           
    ##     Latitude        ObjectId     
    ##  Min.   : 0.00   Min.   :     1  
    ##  1st Qu.:43.66   1st Qu.: 75309  
    ##  Median :43.70   Median :150617  
    ##  Mean   :43.14   Mean   :150617  
    ##  3rd Qu.:43.75   3rd Qu.:225925  
    ##  Max.   :44.33   Max.   :301233  
    ## 

``` r
summary(properties)
```

    ##       ...1          Address            AreaName           Price ($)       
    ##  Min.   :     0   Length:25351       Length:25351       Min.   :       0  
    ##  1st Qu.: 19940   Class :character   Class :character   1st Qu.:  219900  
    ##  Median : 34037   Mode  :character   Mode  :character   Median :  371900  
    ##  Mean   : 42879                                         Mean   :  564544  
    ##  3rd Qu.: 61502                                         3rd Qu.:  619900  
    ##  Max.   :124929                                         Max.   :32500000  
    ##       lat               lng          
    ##  Min.   :-999.00   Min.   :-999.000  
    ##  1st Qu.:  43.40   1st Qu.: -80.308  
    ##  Median :  43.72   Median : -79.503  
    ##  Mean   :  37.33   Mean   : -85.218  
    ##  3rd Qu.:  44.47   3rd Qu.: -79.107  
    ##  Max.   :  53.85   Max.   :   1.075
