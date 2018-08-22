KS Test
================
Paul Jeffries
22 August, 2018

**NOTE: this is an early work in progress. Check back shortly for new additions**

Sources:

<https://www.kaggle.com/kemical/kickstarter-projects/home>

<https://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/>

<http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization>

``` r
# first a few general set-up items / housekeeping items
# setting scipen options to kill all use of scientific notation
options(scipen = 999)

# basic packages needed throughout
library(dplyr) # for piping
library(ggplot2) # for visualization
library(ggthemes) # for custom visualization
library(broom) # needing for tidying model summary output into a df
```

``` r
# need to get the other newer portion of this dataset
test_df <- read.csv("data/ks-projects-201612.csv")
str(test_df)
```

    ## 'data.frame':    323750 obs. of  17 variables:
    ##  $ ID           : int  1000002330 1000004038 1000007540 1000011046 1000014025 1000023410 1000030581 1000034518 100004195 100004721 ...
    ##  $ name         : Factor w/ 321615 levels "    IT\x92S A HOT CAPPUCCINO NIGHT  ",..: 284763 312452 295415 66754 176403 250723 59924 242779 248366 191283 ...
    ##  $ category     : Factor w/ 771 levels "","  A Faerie's Tale.",..: 717 700 697 660 733 664 646 722 643 702 ...
    ##  $ main_category: Factor w/ 120 levels " 50 Years in the Making",..: 93 49 72 49 52 52 52 35 49 93 ...
    ##  $ currency     : Factor w/ 37 levels " Be active!",..: 20 37 37 37 37 37 37 37 37 8 ...
    ##  $ deadline     : Factor w/ 294813 levels " Esoteric","2009-05-03 08:59:59",..: 225635 72930 40729 218250 253969 167692 251333 126077 138622 98811 ...
    ##  $ goal         : Factor w/ 8188 levels "0.01","0.15",..: 8 5453 5820 2160 5822 8 3572 674 6669 3570 ...
    ##  $ launched     : Factor w/ 322798 levels "100","1000","10000",..: 242859 80289 46502 235519 278110 187153 273534 139120 153491 109895 ...
    ##  $ pledged      : Factor w/ 55598 levels "0","1","1.01",..: 1 20546 2 6800 41077 5186 37598 51544 45397 1 ...
    ##  $ state        : Factor w/ 410 levels "0","1","10","100",..: 406 406 406 405 408 408 406 405 405 406 ...
    ##  $ backers      : Factor w/ 3592 levels "0","1","10","100",..: 1 1853 2 520 1370 748 2306 2830 2411 1 ...
    ##  $ country      : Factor w/ 162 levels "0","1","10","107",..: 149 162 162 162 162 162 162 162 162 142 ...
    ##  $ usd.pledged  : Factor w/ 94377 levels "","0","0.566314",..: 2 34611 565 12318 69164 9880 63347 87104 76210 2 ...
    ##  $ X            : Factor w/ 417 levels "","0","0.75115847",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ X.1          : Factor w/ 9 levels "","0","1","128534.587723664",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ X.2          : Factor w/ 4 levels "","0","9854",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ X.3          : int  NA NA NA NA NA NA NA NA NA NA ...

``` r
head(test_df)
```

    ##           ID                                                         name
    ## 1 1000002330                              The Songs of Adelaide & Abullah
    ## 2 1000004038                                               Where is Hank?
    ## 3 1000007540            ToshiCapital Rekordz Needs Help to Complete Album
    ## 4 1000011046   Community Film Project: The Art of Neighborhood Filmmaking
    ## 5 1000014025                                         Monarch Espresso Bar
    ## 6 1000023410 Support Solar Roasted Coffee & Green Energy!  SolarCoffee.co
    ##         category main_category currency            deadline  goal
    ## 1         Poetry    Publishing      GBP 2015-10-09 11:36:00  1000
    ## 2 Narrative Film  Film & Video      USD 2013-02-26 00:20:50 45000
    ## 3          Music         Music      USD 2012-04-16 04:24:11  5000
    ## 4   Film & Video  Film & Video      USD 2015-08-29 01:00:00 19500
    ## 5    Restaurants          Food      USD 2016-04-01 13:38:27 50000
    ## 6           Food          Food      USD 2014-12-21 18:30:44  1000
    ##              launched pledged      state backers country usd.pledged X X.1
    ## 1 2015-08-11 12:12:28       0     failed       0      GB           0      
    ## 2 2013-01-12 00:20:50     220     failed       3      US         220      
    ## 3 2012-03-17 03:24:11       1     failed       1      US           1      
    ## 4 2015-07-04 08:35:03    1283   canceled      14      US        1283      
    ## 5 2016-02-26 13:38:27   52375 successful     224      US       52375      
    ## 6 2014-12-01 18:30:44    1205 successful      16      US        1205      
    ##   X.2 X.3
    ## 1      NA
    ## 2      NA
    ## 3      NA
    ## 4      NA
    ## 5      NA
    ## 6      NA

The distributional tests we can run here span a wide variety of options. Right now, I'm thinking the below:

-   How do the distributions of $ amount funded (dollar-denominated) differ by offering country?
-   Does this phenomenon vary based on the category or sub-category of the offering?

``` r
# quick rounding up of the pledged amount
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

``` r
round_any(132.1, 10)               # returns 130
```

    ## [1] 130

``` r
round_any(132.1, 10, f = ceiling)  # returns 140
```

    ## [1] 140

``` r
round_any(132.1, 5, f = ceiling)   # returns 135
```

    ## [1] 135

Things to think about:

-   should I round to nearest high 10, as done below?
-   any other filters I'm leaving out?

``` r
test_df_forviz <- test_df %>% 
  # filters to just dollar or pound-denominated offerings
  dplyr::filter(country %in% c('US','GB')) %>%
  # filter to only the cases where the USD-denominated pledged amount was less than 5000
  # this dampens the long tails problem in a non-fancy way that I'll change later
  dplyr::filter(as.numeric(as.character(usd.pledged)) < 10000) %>%
  mutate(rounded_usdpledged = plyr::round_any(as.numeric(as.character(usd.pledged)), 10, f=ceiling)) %>%
  # exploratorily dropping instances where the funding amount is set to 0 
  dplyr::filter(as.numeric(as.character(usd.pledged)) > 0)
```

``` r
ggplot(test_df_forviz, aes(x=as.numeric(as.character(usd.pledged)), color=country)) +
  geom_density()
```

![](ks_test_files/figure-markdown_github/unnamed-chunk-6-1.png)
