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

``` r
library(dplyr)
```

### Chargement datasets

``` r
budget <- read_csv("data/Budget_2022.csv")
Major_Crime <- read_csv("data/Major_Crime_Indicators.csv")
properties <- read_csv("data/properties.csv")
```

### Résummer des données

``` r
my_summary <- function(x) {
  if(is.character(x) == TRUE)
  {
    uniqv <- unique(x)
    output1 = uniqv[which.max(tabulate(match(x, uniqv)))]
    names(output1) <- "mode"
    output2 = table(x)[output1]/length(x)
    names(output2) <- "freq"
    
    y <- x[x!=output1]
    uniqv <- unique(y)
    output3 = uniqv[which.max(tabulate(match(y, uniqv)))]
    names(output3) <- "mode 2"
    output4 = table(y)[output3]/length(x)
    names(output4) <- "freq 2"
    
    if(!is.na(output3))
    {
      return(c(output1,output2,output3,output4))
    }
    return(c(output1,output2))
  }
  if(is.numeric(x) == TRUE)
  {
    output1 <- summary(x)
    output2 <- sd(x)
    names(output2) <- "Ecart type"
    if(is.na(output2))
    {
        return(c(output1, output2))
    }
    output3 <- quantile(x)[4] - quantile(x)[2]
    names(output3) <- "IQR"
    
    output4 <- range(x)[2] - range(x)[1]
    names(output4) <- "Etendue"
    
    output5 <- var(x)
    names(output5) <- "Variance"
    
    output6 <- output2 / mean(x)
    
    return(c(output1, output2, output3,output4, output5, output6))
  }
  }
```

``` r
sapply(budget,my_summary)
```

    ## $Fiscal_Year
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. Ecart type 
    ##       2022       2022       2022       2022       2022       2022          0 
    ##        IQR    Etendue   Variance Ecart type 
    ##          0          0          0          0 
    ## 
    ## $Budget_Type
    ##              mode              freq 
    ## "Approved Budget"               "1" 
    ## 
    ## $Organization_Entity
    ##                         mode                         freq 
    ## "1 - Toronto Police Service"          "0.971936694403655" 
    ##                       mode 2                       freq 2 
    ##    "3 - Parking Enforcement"         "0.0199053679229891" 
    ## 
    ## $Command_Name
    ##                                       mode 
    ##       "Community Safety Command (Command)" 
    ##                                       freq 
    ##                         "0.30347528144885" 
    ##                                     mode 2 
    ## "Specialized Operations Command (Command)" 
    ##                                     freq 2 
    ##                        "0.159895578397781" 
    ## 
    ## $Pillar_Name
    ##                          mode                          freq 
    ## "East Field Command (Pillar)"           "0.115516397454723" 
    ##                        mode 2                        freq 2 
    ## "West Field Command (Pillar)"           "0.112905857399249" 
    ## 
    ## $District_Name
    ##                              mode                              freq 
    ##                "People & Culture"              "0.0822320117474303" 
    ##                            mode 2                            freq 2 
    ## "Information Technology Services"              "0.0510686898352097" 
    ## 
    ## $Unit_Name
    ##                           mode                           freq 
    ##  "Centralized Service Charges"           "0.0267580355686083" 
    ##                         mode 2                         freq 2 
    ## "Detective Ops (CONFIDENTIAL)"           "0.0172948278675151" 
    ## 
    ## $Feature_Category
    ##                mode                freq              mode 2              freq 2 
    ##          "Services" "0.325501713166911"          "Salaries" "0.190406265296133" 
    ## 
    ## $Cost_Element
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.501000e+03 1.578000e+03 2.082000e+03 3.094035e+03 4.312000e+03 9.453000e+03 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 1.754408e+03 2.734000e+03 7.952000e+03 3.077946e+06 5.670291e-01 
    ## 
    ## $Cost_Element_Long_Name
    ##                   mode                   freq                 mode 2 
    ## "Computers - hardware"   "0.0231685429923315" "Computers - software" 
    ##                 freq 2 
    ##   "0.0231685429923315" 
    ## 
    ## $Amount
    ##          Min.       1st Qu.        Median          Mean       3rd Qu. 
    ## -4.381020e+07  0.000000e+00  0.000000e+00  1.910661e+05  3.400000e+03 
    ##          Max.    Ecart type           IQR       Etendue      Variance 
    ##  5.716110e+07  2.049826e+06  3.400000e+03  1.009713e+08  4.201786e+12 
    ##    Ecart type 
    ##  1.072836e+01 
    ## 
    ## $ObjectId
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.000000e+00 1.533000e+03 3.065000e+03 3.065000e+03 4.597000e+03 6.129000e+03 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 1.769434e+03 3.064000e+03 6.128000e+03 3.130898e+06 5.773032e-01

``` r
sapply(Major_Crime,my_summary)
```

    ## $X
    ##          Min.       1st Qu.        Median          Mean       3rd Qu. 
    ## -8.910331e+06 -8.846681e+06 -8.838015e+06 -8.724562e+06 -8.829868e+06 
    ##          Max.    Ecart type           IQR       Etendue      Variance 
    ##  0.000000e+00  9.966746e+05  1.681260e+04  8.910331e+06  9.933602e+11 
    ##    Ecart type 
    ## -1.142378e-01 
    ## 
    ## $Y
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 0.000000e+00 5.412946e+06 5.419000e+06 5.350375e+06 5.426986e+06 5.517228e+06 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 6.112283e+05 1.403951e+04 5.517228e+06 3.736000e+11 1.142403e-01 
    ## 
    ## $Index_
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.000000e+00 7.530900e+04 1.506170e+05 1.506170e+05 2.259250e+05 3.012330e+05 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 8.695862e+04 1.506160e+05 3.012320e+05 7.561802e+09 5.773493e-01 
    ## 
    ## $event_unique_id
    ##                   mode                   freq                 mode 2 
    ##       "GO-20151785704" "7.96725458366115e-05"         "GO-201967831" 
    ##                 freq 2 
    ## "7.63528564267527e-05" 
    ## 
    ## $Division
    ##                 mode                 freq               mode 2 
    ##                "D51" "0.0855948717437996"                "D32" 
    ##               freq 2 
    ## "0.0759080180458316" 
    ## 
    ## $occurrencedate
    ##                     mode                     freq                   mode 2 
    ## "2018/01/01 05:00:00+00"   "0.000902955519481597" "2015/01/01 05:00:00+00" 
    ##                   freq 2 
    ##   "0.000856479867743574" 
    ## 
    ## $reporteddate
    ##                     mode                     freq                   mode 2 
    ## "2019/07/23 04:00:00+00"   "0.000557707820856281" "2019/11/18 05:00:00+00" 
    ##                   freq 2 
    ##   "0.000541109373806987" 
    ## 
    ## $location_type
    ##                                                  mode 
    ##                    "Apartment (Rooming House, Condo)" 
    ##                                                  freq 
    ##                                   "0.239220138563836" 
    ##                                                mode 2 
    ## "Single Home, House (Attach Garage, Cottage, Mobile)" 
    ##                                                freq 2 
    ##                                   "0.179515524527525" 
    ## 
    ## $premises_type
    ##                mode                freq              mode 2              freq 2 
    ##           "Outside" "0.265837408252084"         "Apartment" "0.239220138563836" 
    ## 
    ## $ucr_code
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.410000e+03 1.430000e+03 1.450000e+03 1.702423e+03 2.120000e+03 2.135000e+03 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 3.262601e+02 6.900000e+02 7.250000e+02 1.064457e+05 1.916446e-01 
    ## 
    ## $ucr_ext
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ##  100.0000000  100.0000000  100.0000000  146.5655854  200.0000000  230.0000000 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ##   51.9889665  100.0000000  130.0000000 2702.8526402    0.3547147 
    ## 
    ## $offence
    ##                mode                freq              mode 2              freq 2 
    ##           "Assault" "0.366918631092875"               "B&E" "0.166588653965535" 
    ## 
    ## $reportedyear
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 2.014000e+03 2.016000e+03 2.018000e+03 2.017877e+03 2.020000e+03 2.022000e+03 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 2.436940e+00 4.000000e+00 8.000000e+00 5.938676e+00 1.207675e-03 
    ## 
    ## $reportedmonth
    ##                 mode                 freq               mode 2 
    ##                "May" "0.0919421178954497"               "June" 
    ##               freq 2 
    ## "0.0910756789594765" 
    ## 
    ## $reportedday
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. Ecart type 
    ##   1.000000   8.000000  16.000000  15.736659  23.000000  31.000000   8.766766 
    ##        IQR    Etendue   Variance Ecart type 
    ##  15.000000  30.000000  76.856194   0.557092 
    ## 
    ## $reporteddayofyear
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.000000e+00 9.200000e+01 1.780000e+02 1.807658e+02 2.700000e+02 3.660000e+02 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 1.037172e+02 1.780000e+02 3.650000e+02 1.075726e+04 5.737659e-01 
    ## 
    ## $reporteddayofweek
    ##                mode                freq              mode 2              freq 2 
    ##            "Monday" "0.147862286004521"            "Friday" "0.146152645958444" 
    ## 
    ## $reportedhour
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. Ecart type 
    ##  0.0000000  8.0000000 13.0000000 12.8087859 18.0000000 23.0000000  6.5025623 
    ##        IQR    Etendue   Variance Ecart type 
    ## 10.0000000 23.0000000 42.2833167  0.5076642 
    ## 
    ## $occurrenceyear
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
    ##   2000.000   2016.000   2018.000   2017.823   2020.000   2022.000    100.000 
    ## Ecart type 
    ##         NA 
    ## 
    ## $occurrencemonth
    ##                 mode                 freq               mode 2 
    ##               "June"  "0.091205146846461"                "May" 
    ##               freq 2 
    ## "0.0911918680888216" 
    ## 
    ## $occurrenceday
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
    ##    1.00000    8.00000   15.00000   15.45648   23.00000   31.00000  100.00000 
    ## Ecart type 
    ##         NA 
    ## 
    ## $occurrencedayofyear
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
    ##     1.0000    92.0000   178.0000   180.5448   270.0000   366.0000   100.0000 
    ## Ecart type 
    ##         NA 
    ## 
    ## $occurrencedayofweek
    ##                mode                freq              mode 2              freq 2 
    ##            "Friday" "0.151148778520282"          "Saturday" "0.148220812460786" 
    ## 
    ## $occurrencehour
    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. Ecart type 
    ##  0.0000000  7.0000000 14.0000000 12.6015908 19.0000000 23.0000000  7.2437527 
    ##        IQR    Etendue   Variance Ecart type 
    ## 12.0000000 23.0000000 52.4719529  0.5748284 
    ## 
    ## $mci_category
    ##                mode                freq              mode 2              freq 2 
    ##           "Assault" "0.537235296265681"   "Break and Enter" "0.197322338522008" 
    ## 
    ## $Hood_ID
    ##                 mode                 freq               mode 2 
    ##                 "77" "0.0375191297102243"                 "75" 
    ##               freq 2 
    ## "0.0330043521128163" 
    ## 
    ## $Neighbourhood
    ##                                mode                                freq 
    ## "Waterfront Communities-The Island"                "0.0375191297102243" 
    ##                              mode 2                              freq 2 
    ##             "Church-Yonge Corridor"                "0.0330043521128163" 
    ## 
    ## $Longitude
    ##        Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
    ## -80.0428663 -79.4710859 -79.3932382 -78.3740712 -79.3200558   0.0000000 
    ##  Ecart type         IQR     Etendue    Variance  Ecart type 
    ##   8.9532798   0.1510301  80.0428663  80.1612199  -0.1142378 
    ## 
    ## $Latitude
    ##        Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
    ##  0.00000000 43.65980581 43.69913160 43.14387406 43.75097753 44.33369129 
    ##  Ecart type         IQR     Etendue    Variance  Ecart type 
    ##  4.92861266  0.09117172 44.33369129 24.29122275  0.11423667 
    ## 
    ## $ObjectId
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 1.000000e+00 7.530900e+04 1.506170e+05 1.506170e+05 2.259250e+05 3.012330e+05 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 8.695862e+04 1.506160e+05 3.012320e+05 7.561802e+09 5.773493e-01

``` r
sapply(properties,my_summary)
```

    ## $...1
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 0.000000e+00 1.994000e+04 3.403700e+04 4.287852e+04 6.150150e+04 1.249290e+05 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 3.145625e+04 4.156150e+04 1.249290e+05 9.894955e+08 7.336132e-01 
    ## 
    ## $Address
    ##                              mode                              freq 
    ##     "223 Erb Street Waterloo, ON"            "0.000631138811092265" 
    ##                            mode 2                            freq 2 
    ## "311 Dundas Street Cambridge, ON"            "0.000433907932625932" 
    ## 
    ## $AreaName
    ##                 mode                 freq               mode 2 
    ##           "Downtown" "0.0327403258254112"        "Mississauga" 
    ##               freq 2 
    ## "0.0296635241213364" 
    ## 
    ## $`Price ($)`
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## 0.000000e+00 2.199000e+05 3.719000e+05 5.645438e+05 6.199000e+05 3.250000e+07 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ## 8.475962e+05 4.000000e+05 3.250000e+07 7.184193e+11 1.501383e+00 
    ## 
    ## $lat
    ##        Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
    ## -999.000000   43.401087   43.715977   37.326614   44.466711   53.851017 
    ##  Ecart type         IQR     Etendue    Variance  Ecart type 
    ##   82.858347    1.065624 1052.851017 6865.505690    2.219820 
    ## 
    ## $lng
    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## -999.0000000  -80.3081590  -79.5033420  -85.2183791  -79.1073265    1.0745190 
    ##   Ecart type          IQR      Etendue     Variance   Ecart type 
    ##   73.0935722    1.2008325 1000.0745190 5342.6703017   -0.8577208

``` r
select_numeric_cols <- function(df) {
  numeric_cols <- sapply(df, is.numeric) # Détermine les colonnes numériques
  df_numeric <- df[, numeric_cols] # Sélectionne les colonnes numériques
  return(df_numeric) # Retourne le dataframe avec les colonnes numériques uniquement
}
```

``` r
propertie_num <- select_numeric_cols(properties)
print(propertie_num)
```

    ## # A tibble: 25,351 × 4
    ##     ...1 `Price ($)`   lat   lng
    ##    <dbl>       <dbl> <dbl> <dbl>
    ##  1     0      999888  43.7 -79.5
    ##  2     1      399900  43.2 -79.9
    ##  3     2      479000  43.3 -79.9
    ##  4     3      285900  43.2 -79.8
    ##  5     6      362000  43.7 -79.4
    ##  6     7     1488000  43.7 -79.3
    ##  7     8          25  43.7 -79.5
    ##  8     9      254900  43.3 -79.9
    ##  9    10      364900  43.2 -79.9
    ## 10    12      304900  43.2 -79.8
    ## # … with 25,341 more rows

``` r
cor(propertie_num)
```

    ##                    ...1  Price ($)           lat         lng
    ## ...1       1.0000000000 0.02241174 -0.0003230475 -0.00218434
    ## Price ($)  0.0224117427 1.00000000  0.0119423894  0.01292994
    ## lat       -0.0003230475 0.01194239  1.0000000000  0.99960926
    ## lng       -0.0021843399 0.01292994  0.9996092577  1.00000000

``` r
cor_pro_num = cor(propertie_num)
```

``` r
corrplot(cor_pro_num, type = "upper", order = "hclust", tl.col = 'black', tl.srt = 45)
```

![](tp_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
budget_num <- select_numeric_cols(budget)
```

``` r
remove_variable <- function(data, var_name) {
  data[, var_name] <- NULL
  return(data)
}

budget_num2<- remove_variable(budget_num, "Fiscal_Year")
```

``` r
cor(budget_num2)
```

    ##              Cost_Element      Amount    ObjectId
    ## Cost_Element    1.0000000 -0.10893911  0.09189220
    ## Amount         -0.1089391  1.00000000 -0.03224158
    ## ObjectId        0.0918922 -0.03224158  1.00000000

``` r
cor_bud2_num = cor(budget_num2)
```

``` r
corrplot(cor_bud2_num, type = "upper", order = "hclust", tl.col = 'black', tl.srt = 45)
```

![](tp_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
Major_Crime_num <- select_numeric_cols(Major_Crime)
```

``` r
Major_Crime_num2<- remove_variable(Major_Crime_num, "occurrenceyear")
Major_Crime_num3<- remove_variable(Major_Crime_num2, "occurrenceday")
Major_Crime_num4<- remove_variable(Major_Crime_num3, "occurrencedayofyear")
```

``` r
cor(Major_Crime_num4)
```

    ##                              X            Y       Index_      ucr_code
    ## X                  1.000000000 -0.999790441  0.012188459 -0.0259913897
    ## Y                 -0.999790441  1.000000000 -0.012829881  0.0256110482
    ## Index_             0.012188459 -0.012829881  1.000000000  0.0356478706
    ## ucr_code          -0.025991390  0.025611048  0.035647871  1.0000000000
    ## ucr_ext           -0.019855096  0.019522198  0.019140561  0.9204898802
    ## reportedyear       0.012776850 -0.013409732  0.992881577  0.0343238358
    ## reportedday       -0.002564812  0.002578124  0.010293079 -0.0006646436
    ## reporteddayofyear -0.005550511  0.005523672  0.016778595  0.0023881891
    ## reportedhour       0.006849295 -0.005969796 -0.011112272 -0.0772506188
    ## occurrencehour     0.002730976 -0.002246185  0.006039805 -0.0410911782
    ## Longitude          1.000000000 -0.999790441  0.012188459 -0.0259913897
    ## Latitude          -0.999832102  0.999996615 -0.012767149  0.0255049154
    ## ObjectId           0.012188608 -0.012830036  0.999989399  0.0356394676
    ##                        ucr_ext  reportedyear   reportedday reporteddayofyear
    ## X                 -0.019855096  0.0127768504 -0.0025648116      -0.005550511
    ## Y                  0.019522198 -0.0134097322  0.0025781242       0.005523672
    ## Index_             0.019140561  0.9928815768  0.0102930793       0.016778595
    ## ucr_code           0.920489880  0.0343238358 -0.0006646436       0.002388189
    ## ucr_ext            1.000000000  0.0181347489 -0.0014862173       0.001658966
    ## reportedyear       0.018134749  1.0000000000  0.0000919956      -0.099195067
    ## reportedday       -0.001486217  0.0000919956  1.0000000000       0.088157921
    ## reporteddayofyear  0.001658966 -0.0991950670  0.0881579211       1.000000000
    ## reportedhour      -0.062726136 -0.0105469905 -0.0019201097      -0.004991329
    ## occurrencehour    -0.022445011  0.0054540182 -0.0015096877       0.003729766
    ## Longitude         -0.019855096  0.0127768504 -0.0025648116      -0.005550511
    ## Latitude           0.019417461 -0.0133473053  0.0025791826       0.005523256
    ## ObjectId           0.019128534  0.9928696288  0.0102545769       0.016789622
    ##                   reportedhour occurrencehour    Longitude     Latitude
    ## X                  0.006849295    0.002730976  1.000000000 -0.999832102
    ## Y                 -0.005969796   -0.002246185 -0.999790441  0.999996615
    ## Index_            -0.011112272    0.006039805  0.012188459 -0.012767149
    ## ucr_code          -0.077250619   -0.041091178 -0.025991390  0.025504915
    ## ucr_ext           -0.062726136   -0.022445011 -0.019855096  0.019417461
    ## reportedyear      -0.010546991    0.005454018  0.012776850 -0.013347305
    ## reportedday       -0.001920110   -0.001509688 -0.002564812  0.002579183
    ## reporteddayofyear -0.004991329    0.003729766 -0.005550511  0.005523256
    ## reportedhour       1.000000000    0.564409654  0.006849295 -0.006090994
    ## occurrencehour     0.564409654    1.000000000  0.002730976 -0.002364204
    ## Longitude          0.006849295    0.002730976  1.000000000 -0.999832102
    ## Latitude          -0.006090994   -0.002364204 -0.999832102  1.000000000
    ## ObjectId          -0.011147915    0.006029707  0.012188608 -0.012767292
    ##                       ObjectId
    ## X                  0.012188608
    ## Y                 -0.012830036
    ## Index_             0.999989399
    ## ucr_code           0.035639468
    ## ucr_ext            0.019128534
    ## reportedyear       0.992869629
    ## reportedday        0.010254577
    ## reporteddayofyear  0.016789622
    ## reportedhour      -0.011147915
    ## occurrencehour     0.006029707
    ## Longitude          0.012188608
    ## Latitude          -0.012767292
    ## ObjectId           1.000000000

``` r
cor_maj4_num = cor(Major_Crime_num4)
```

``` r
corrplot(cor_maj4_num, type = "upper", order = "hclust", tl.col = 'black', tl.srt = 45)
```

![](tp_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
