KS Test
================
Paul Jeffries
24 August, 2018

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
library(plyr) # always load prior to dplyr if needed
library(dplyr) # for piping
library(ggplot2) # for visualization
library(ggthemes) # for custom visualization
```

``` r
# need to get the other newer portion of this dataset
test_df <- read.csv("data/ks-projects-201612.csv")
```

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

Things to think about:

-   should I round to nearest high 10, as done below?
-   any other filters I'm leaving out?

``` r
test_df_forviz <- test_df %>% 
  # filters to just kickstarters from the US and GB
  dplyr::filter(country %in% c('US','GB')) %>%
  # filters to just kickstarters under a certain category
  dplyr::filter(category == 'Music') %>%
  # filter to only the cases where the USD-denominated pledged amount was less than 10000
  # this dampens the long tails problem in a non-fancy way that I'll change later
  # dplyr::filter(as.numeric(as.character(usd.pledged)) < 10000) %>%
  mutate(
    # rounded pledged dollar values to nearest 10
    rounded_usdpledged = plyr::round_any(as.numeric(as.character(usd.pledged)), 10)) 
```

``` r
# ggplot code for the basid density plot
ggplot(data = test_df_forviz, aes(x=as.numeric(as.character(usd.pledged)))) +
  geom_density(aes(fill=factor(country)),alpha = 0.4) +
  scale_x_continuous(limits = c(0,5000)) +
  theme(legend.position = "top") +
  labs(
    title = paste0("PDF for music category kickstarters"),
    y = "Concentration Density",
    x = "Amount Pledged (converted to USD)",
    fill = "Country of origin for kickstarter"
  )
```

    ## Warning: Removed 2664 rows containing non-finite values (stat_density).

![](ks_test_files/figure-markdown_github/unnamed-chunk-5-1.png)
