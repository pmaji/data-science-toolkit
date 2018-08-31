Exploratory Data Analysis (EDA) and Visualization
================
Paul Jeffries
30 August, 2018

-   [Introduction](#introduction)
    -   [Setup](#setup)
-   [Importing, Exploring, and Cleaning the Data](#importing-exploring-and-cleaning-the-data)
    -   [Importing the Data](#importing-the-data)
    -   [Exploring and Cleaning the Data](#exploring-and-cleaning-the-data)
-   [Summary Statistics](#summary-statistics)
-   [Histograms](#histograms)
-   [Density Plots](#density-plots)

**NOTE: this is an early work in progress. Check back shortly for new additions**

Introduction
============

The purpose of this document is to serve as a smorgasbord of EDA techniques and visualization tools.

Setup
-----

``` r
# first a few general setup / housekeeping items
# setting scipen options to kill all use of scientific notation
options(scipen = 999)

# basic packages needed throughout:
library(plyr) # always load prior to dplyr / tidyverse if needed
library(tidyverse) # for all things tidy 
library(janitor) # for data cleaning and some utility functions
library(DataExplorer) # allows for creation of missing values map
library(RCurl) # Provides functions to allow one to compose general HTTP requests, etc. in R
library(broom) # for tidy modeling and displaying of model / test results 
library(ggthemes) # for more custom ggplot themes

# If I reference functions that are more niche, I will call them explicitly in-line as well
```

Importing, Exploring, and Cleaning the Data
===========================================

Importing the Data
------------------

The data used in this document come from a [Kaggle post](https://www.kaggle.com/kemical/kickstarter-projects/home) focused on Kickstarter campaigns. If unfamiliar with the notion of a Kickstarter campaign (henceforth just campaign), I would recommend reading [this FAQ here](https://help.kickstarter.com/hc/en-us/categories/115000499013-Kickstarter-basics). I will not spend a great deal of time explaining the data, so for more information on the data specifically, I recommend reading the detailed exploration on the [data page for this Kaggle](https://www.kaggle.com/kemical/kickstarter-projects).

``` r
# importing the dataset from the CSV
base_df <- read.csv("./hypothesis_tests/data/ks-projects-201801.csv")
```

``` r
# taking a preliminary look at the structure of the dataset
glimpse(base_df)
```

    ## Observations: 378,661
    ## Variables: 15
    ## $ ID               <int> 1000002330, 1000003930, 1000004038, 100000754...
    ## $ name             <fct> The Songs of Adelaide & Abullah, Greeting Fro...
    ## $ category         <fct> Poetry, Narrative Film, Narrative Film, Music...
    ## $ main_category    <fct> Publishing, Film & Video, Film & Video, Music...
    ## $ currency         <fct> GBP, USD, USD, USD, USD, USD, USD, USD, USD, ...
    ## $ deadline         <fct> 2015-10-09, 2017-11-01, 2013-02-26, 2012-04-1...
    ## $ goal             <dbl> 1000, 30000, 45000, 5000, 19500, 50000, 1000,...
    ## $ launched         <fct> 2015-08-11 12:12:28, 2017-09-02 04:43:57, 201...
    ## $ pledged          <dbl> 0.00, 2421.00, 220.00, 1.00, 1283.00, 52375.0...
    ## $ state            <fct> failed, failed, failed, failed, canceled, suc...
    ## $ backers          <int> 0, 15, 3, 1, 14, 224, 16, 40, 58, 43, 0, 100,...
    ## $ country          <fct> GB, US, US, US, US, US, US, US, US, US, CA, U...
    ## $ usd.pledged      <dbl> 0.00, 100.00, 220.00, 1.00, 1283.00, 52375.00...
    ## $ usd_pledged_real <dbl> 0.00, 2421.00, 220.00, 1.00, 1283.00, 52375.0...
    ## $ usd_goal_real    <dbl> 1533.95, 30000.00, 45000.00, 5000.00, 19500.0...

Exploring and Cleaning the Data
-------------------------------

### Dealing with NULLs

First, we'll conduct some broad cleaning. Using the [janitor package](https://github.com/sfirke/janitor) I will clean up the variable names (in this case not necssarily because the CSV is pristinely formatted), and drop any rows or columns where all observations all null.

``` r
# tidying variable names and dropping any useless rows / columns
base_df <- base_df %>%
  # converts to underscore case and cleans up column names
  janitor::clean_names() %>% 
  # drops all rows and columns that are entirely empty
  janitor::remove_empty(which = c("rows","cols")) 
```

Next, we'll move on to dealing with the trickier instances of NULLs: cases where there are singular NULL observations scattered in our data. In order to deal with these, we'll first plot out their occurence, and thereafter decide what to do with them.

``` r
DataExplorer::plot_missing(base_df) # shows % of NAs within each variable
```

![](eda_and_visualization_files/figure-markdown_github/unnamed-chunk-5-1.png)

From the chart above, we can see that there is only one variable--"usd\_pledged"--that has missing data. It has a missing rate of 1%, which isn't terrible, but given that we have a large amount of data (as shown via the row count returned by the previous glimpse() call), **we'll drop any instances of nulls entirely** to ensure we have the cleanest of data. This is by no means necessary in all cases, and the treatment of nulls should be decided on a case-by-case basis pursuant to the requirements of the project and quality / size of the data at hand.

``` r
# dropping any and all NULLs entirely, and rechecking our missing map to double check
base_df <- base_df[complete.cases(base_df),]
DataExplorer::plot_missing(base_df) # shows % of NAs within each variable
```

![](eda_and_visualization_files/figure-markdown_github/unnamed-chunk-6-1.png)

Summary Statistics
==================

``` r
psych::describe(base_df)
```

    ##                  vars      n          mean           sd        median
    ## id                  1 374864 1074651369.45 619135733.97 1075277164.00
    ## name*               2 374864     187978.66    108462.96     188071.50
    ## category*           3 374864         81.70        45.32         88.00
    ## main_category*      4 374864          8.49         3.92          8.00
    ## currency*           5 374864         12.01         3.94         14.00
    ## deadline*           6 374864       1944.15       703.30       2011.00
    ## goal                7 374864      49523.10   1189355.29       5500.00
    ## launched*           8 374864     188687.48    109543.47     187864.50
    ## pledged             9 374864       9750.44     96010.43        620.00
    ## state*             10 374864          2.63         1.09          2.00
    ## backers            11 374864        106.69       911.71         12.00
    ## country*           12 374864         19.88         6.30         23.00
    ## usd_pledged        13 374864       7036.73     78639.75        394.72
    ## usd_pledged_real   14 374864       9120.80     91319.21        624.41
    ## usd_goal_real      15 374864      45863.18   1158761.22       5500.00
    ##                        trimmed          mad     min        max      range
    ## id               1074796426.08 794703948.08 5971.00 2147476221 2147470250
    ## name*                187993.93    139308.80    1.00     375765     375764
    ## category*                82.26        57.82    1.00        159        158
    ## main_category*            8.68         4.45    1.00         15         14
    ## currency*                12.89         0.00    1.00         14         13
    ## deadline*              1973.29       779.85    1.00       3164       3163
    ## goal                   9772.49      6671.70    0.01  100000000  100000000
    ## launched*            188596.11    141334.03    1.00     378089     378088
    ## pledged                2118.77       919.21    0.00   20338986   20338986
    ## state*                    2.66         0.00    1.00          5          4
    ## backers                  29.34        17.79    0.00     219382     219382
    ## country*                 21.37         0.00    1.00         23         22
    ## usd_pledged            1564.03       585.21    0.00   20338986   20338986
    ## usd_pledged_real       2092.57       925.75    0.00   20338986   20338986
    ## usd_goal_real          9500.39      6671.70    0.01  166361391  166361391
    ##                    skew kurtosis         se
    ## id                 0.00    -1.20 1011227.81
    ## name*              0.00    -1.20     177.15
    ## category*         -0.06    -1.24       0.07
    ## main_category*    -0.23    -0.81       0.01
    ## currency*         -1.63     1.02       0.01
    ## deadline*         -0.33    -0.71       1.15
    ## goal              70.44  5519.08    1942.56
    ## launched*          0.01    -1.21     178.92
    ## pledged           74.96  9952.63     156.81
    ## state*             0.31    -1.41       0.00
    ## backers           86.34 13818.11       1.49
    ## country*          -1.71     1.31       0.01
    ## usd_pledged      105.90 18960.57     128.44
    ## usd_pledged_real  82.00 11733.81     149.15
    ## usd_goal_real     77.83  7011.92    1892.59

``` r
Hmisc::describe(base_df)
```

    ## base_df 
    ## 
    ##  15  Variables      374864  Observations
    ## ---------------------------------------------------------------------------
    ## id 
    ##          n    missing   distinct       Info       Mean        Gmd 
    ##     374864          0     374864          1 1074651369  714916565 
    ##        .05        .10        .25        .50        .75        .90 
    ##  108666103  216157052  538070778 1075277164 1610137351 1932069740 
    ##        .95 
    ## 2039705817 
    ## 
    ## lowest :       5971      18520      21109      21371      24380
    ## highest: 2147455254 2147460119 2147466649 2147472329 2147476221
    ## ---------------------------------------------------------------------------
    ## name 
    ##        n  missing distinct 
    ##   374864        0   372069 
    ## 
    ## lowest :                                                                  IT’S A HOT CAPPUCCINO NIGHT                                "Pastriology": a global documentary                          15   A Feature Length Film (Canceled)                       'A Taste of Happiness'   a new CD of songs by Lisa Richards
    ## highest: µGALE (Canceled)                                             µPeek - The Professional Microscope that Fits in Your Wallet µRuler - Measure size in the micro-world under Microscope    µTøøl - The Ultimate pocket tool! Credit card size!          µTracker - extremly Power Saving GPS + GSM Board            
    ## ---------------------------------------------------------------------------
    ## category 
    ##        n  missing distinct 
    ##   374864        0      159 
    ## 
    ## lowest : 3D Printing Academic    Accessories Action      Animals    
    ## highest: Woodworking Workshops   World Music Young Adult Zines      
    ## ---------------------------------------------------------------------------
    ## main_category 
    ##        n  missing distinct 
    ##   374864        0       15 
    ## 
    ## Art (28153, 0.075), Comics (10819, 0.029), Crafts (8809, 0.023), Dance
    ## (3767, 0.010), Design (30067, 0.080), Fashion (22812, 0.061), Film & Video
    ## (62697, 0.167), Food (24599, 0.066), Games (35226, 0.094), Journalism
    ## (4754, 0.013), Music (49530, 0.132), Photography (10778, 0.029),
    ## Publishing (39379, 0.105), Technology (32562, 0.087), Theater (10912,
    ## 0.029)
    ## ---------------------------------------------------------------------------
    ## currency 
    ##        n  missing distinct 
    ##   374864        0       14 
    ##                                                                          
    ## Value         AUD    CAD    CHF    DKK    EUR    GBP    HKD    JPY    MXN
    ## Frequency    7839  14756    761   1113  17219  33672    618     40   1752
    ## Proportion  0.021  0.039  0.002  0.003  0.046  0.090  0.002  0.000  0.005
    ##                                              
    ## Value         NOK    NZD    SEK    SGD    USD
    ## Frequency     708   1447   1757    555 292627
    ## Proportion  0.002  0.004  0.005  0.001  0.781
    ## ---------------------------------------------------------------------------
    ## deadline 
    ##        n  missing distinct 
    ##   374864        0     3164 
    ## 
    ## lowest : 2009-05-03 2009-05-16 2009-05-20 2009-05-22 2009-05-26
    ## highest: 2018-02-27 2018-02-28 2018-03-01 2018-03-02 2018-03-03
    ## ---------------------------------------------------------------------------
    ## goal 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0     8312    0.999    49523    88060      400      650 
    ##      .25      .50      .75      .90      .95 
    ##     2000     5500    16500    50000    90000 
    ## 
    ## lowest :         0.01         0.15         0.50         1.00         1.85
    ## highest:  73000000.00  75000000.00  80000000.00  99000000.00 100000000.00
    ## ---------------------------------------------------------------------------
    ## launched 
    ##        n  missing distinct 
    ##   374864        0   374302 
    ## 
    ## lowest : 1970-01-01 01:00:00 2009-04-21 21:02:48 2009-04-23 00:07:53 2009-04-24 21:52:03 2009-04-25 17:36:21
    ## highest: 2018-01-02 14:13:09 2018-01-02 14:15:38 2018-01-02 14:17:46 2018-01-02 14:38:17 2018-01-02 15:02:31
    ## ---------------------------------------------------------------------------
    ## pledged 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0    61936    0.997     9750    17404        0        0 
    ##      .25      .50      .75      .90      .95 
    ##       31      620     4080    14344    30010 
    ## 
    ## lowest :        0.00        1.00        1.01        1.02        1.03
    ## highest: 10266845.74 12393139.69 12779843.49 13285226.36 20338986.27
    ## ---------------------------------------------------------------------------
    ## state 
    ##        n  missing distinct 
    ##   374864        0        5 
    ##                                                                  
    ## Value        canceled     failed       live successful  suspended
    ## Frequency       38757     197614       2798     133851       1844
    ## Proportion      0.103      0.527      0.007      0.357      0.005
    ## ---------------------------------------------------------------------------
    ## backers 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0     3963    0.996    106.7    183.6        0        0 
    ##      .25      .50      .75      .90      .95 
    ##        2       12       57      168      338 
    ## 
    ## lowest :      0      1      2      3      4, highest:  87142  91585 105857 154926 219382
    ## ---------------------------------------------------------------------------
    ## country 
    ##        n  missing distinct 
    ##   374864        0       22 
    ## 
    ## lowest : AT AU BE CA CH, highest: NO NZ SE SG US
    ## ---------------------------------------------------------------------------
    ## usd_pledged 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0    95455    0.994     7037    12556     0.00     0.00 
    ##      .25      .50      .75      .90      .95 
    ##    16.98   394.72  3034.09 10859.70 22432.85 
    ## 
    ## lowest :        0.00        0.47        0.48        0.51        0.52
    ## highest:  9192055.66 10266845.74 12779843.49 13285226.36 20338986.27
    ## ---------------------------------------------------------------------------
    ## usd_pledged_real 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0   105383    0.997     9121    16187      0.0      0.0 
    ##      .25      .50      .75      .90      .95 
    ##     31.0    624.4   4050.8  13872.7  28375.7 
    ## 
    ## lowest :        0.00        0.45        0.47        0.48        0.49
    ## highest: 10266845.74 12393139.69 12779843.49 13285226.36 20338986.27
    ## ---------------------------------------------------------------------------
    ## usd_goal_real 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   374864        0    50032    0.999    45863    81004      400      700 
    ##      .25      .50      .75      .90      .95 
    ##     2000     5500    16000    46049    80000 
    ## 
    ## lowest :         0.01         0.15         0.49         0.50         0.55
    ## highest: 104057189.83 107369867.72 110169771.62 151395869.92 166361390.71
    ## ---------------------------------------------------------------------------

Histograms
==========

``` r
base_df %>%
  dplyr::select(goal, country) %>%
  dplyr::filter(
    country %in% c("AU","DE","FR"),
    goal <= 100000
    ) %>%
  # base ggplot call
  ggplot(., aes(x=goal, fill=country)) +
    # specifying the histogram
    geom_histogram(color = "black", position="dodge", bins=20) +
    # picking a colorblind-friendly color scheme and theme
    ggthemes::scale_fill_tableau() +
    ggthemes::theme_economist() +
    theme(
      legend.position = "top",
      legend.title = element_text(size=12),
      legend.text = element_text(size=12)
      ) +
    # takes care of all labeling
    labs(
      title = paste0("Histogram of Selected Countries' Goal Distribution"),
      y = "Number of Campaigns",
      x = "Campaign Fundraising Goal (USD)",
      fill = "Country of Origin"
    )
```

![](eda_and_visualization_files/figure-markdown_github/unnamed-chunk-8-1.png)

Density Plots
=============

``` r
base_df %>%
  dplyr::select(goal, country) %>%
  dplyr::filter(
    country %in% c("GB","FR"),
    goal <= 25000
    ) %>%
  # base ggplot call
  ggplot(., aes(x=goal, fill=country)) +
    # specifying the histogram
    geom_density(color = "black", alpha = 0.8) +
    # picking a colorblind-friendly color scheme and theme
    ggthemes::scale_fill_tableau() +
    ggthemes::theme_economist() +
    theme(
      legend.position = "top",
      legend.title = element_text(size=12),
      legend.text = element_text(size=12)
      ) +
    # takes care of all labeling
    labs(
      title = paste0("Density Plot of Selected Countries' Goal Distribution"),
      y = "Concentration Density",
      x = "Campaign Fundraising Goal (USD)",
      fill = "Country of Origin"
    )
```

![](eda_and_visualization_files/figure-markdown_github/unnamed-chunk-9-1.png)
