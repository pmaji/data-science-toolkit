Low Incidence Binary Classification
================

Data taken from: <https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/>

Importing, Exploring, and Cleaning the Data
===========================================

Importing the Data
------------------

``` r
setwd("~/Desktop/Personal/personal_code/classification/")

# setting scientific notation options to kill all scipen
options(scipen = 999)

# basic packages
library(dplyr) # for piping
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2) # for visualization 
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     vars

Exploring the Data
------------------

A description of the various variables can be found here: <https://archive.ics.uci.edu/ml/datasets/automobile>

``` r
base_red <- read.csv("winequality-red.csv",sep=";")
base_white <- read.csv("winequality-white.csv",sep=";")

# check to see if we have missing values
library(Amelia) # allows for creation of missmap--missings values map
```

    ## Loading required package: Rcpp

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone 'zone/tz/2018c.1.0/
    ## zoneinfo/America/Chicago'

    ## ## 
    ## ## Amelia II: Multiple Imputation
    ## ## (Version 1.7.4, built: 2015-12-05)
    ## ## Copyright (C) 2005-2018 James Honaker, Gary King and Matthew Blackwell
    ## ## Refer to http://gking.harvard.edu/amelia/ for more information
    ## ##

``` r
# luckily it doesn't look like we have an missing values, but we'll use janitor to be sure
Amelia::missmap(base_red, main = "Missing values vs observed")
```

![](wine_exploration_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# lots of useful information about the dataset
glimpse(base_red) 
```

    ## Observations: 1,599
    ## Variables: 12
    ## $ fixed.acidity        <dbl> 7.4, 7.8, 7.8, 11.2, 7.4, 7.4, 7.9, 7.3, ...
    ## $ volatile.acidity     <dbl> 0.700, 0.880, 0.760, 0.280, 0.700, 0.660,...
    ## $ citric.acid          <dbl> 0.00, 0.00, 0.04, 0.56, 0.00, 0.00, 0.06,...
    ## $ residual.sugar       <dbl> 1.9, 2.6, 2.3, 1.9, 1.9, 1.8, 1.6, 1.2, 2...
    ## $ chlorides            <dbl> 0.076, 0.098, 0.092, 0.075, 0.076, 0.075,...
    ## $ free.sulfur.dioxide  <dbl> 11, 25, 15, 17, 11, 13, 15, 15, 9, 17, 15...
    ## $ total.sulfur.dioxide <dbl> 34, 67, 54, 60, 34, 40, 59, 21, 18, 102, ...
    ## $ density              <dbl> 0.9978, 0.9968, 0.9970, 0.9980, 0.9978, 0...
    ## $ pH                   <dbl> 3.51, 3.20, 3.26, 3.16, 3.51, 3.51, 3.30,...
    ## $ sulphates            <dbl> 0.56, 0.68, 0.65, 0.58, 0.56, 0.56, 0.46,...
    ## $ alcohol              <dbl> 9.4, 9.8, 9.8, 9.8, 9.4, 9.4, 9.4, 10.0, ...
    ## $ quality              <int> 5, 5, 5, 6, 5, 5, 5, 7, 7, 5, 5, 5, 5, 5,...

``` r
glimpse(base_white) 
```

    ## Observations: 4,898
    ## Variables: 12
    ## $ fixed.acidity        <dbl> 7.0, 6.3, 8.1, 7.2, 7.2, 8.1, 6.2, 7.0, 6...
    ## $ volatile.acidity     <dbl> 0.27, 0.30, 0.28, 0.23, 0.23, 0.28, 0.32,...
    ## $ citric.acid          <dbl> 0.36, 0.34, 0.40, 0.32, 0.32, 0.40, 0.16,...
    ## $ residual.sugar       <dbl> 20.70, 1.60, 6.90, 8.50, 8.50, 6.90, 7.00...
    ## $ chlorides            <dbl> 0.045, 0.049, 0.050, 0.058, 0.058, 0.050,...
    ## $ free.sulfur.dioxide  <dbl> 45, 14, 30, 47, 47, 30, 30, 45, 14, 28, 1...
    ## $ total.sulfur.dioxide <dbl> 170, 132, 97, 186, 186, 97, 136, 170, 132...
    ## $ density              <dbl> 1.0010, 0.9940, 0.9951, 0.9956, 0.9956, 0...
    ## $ pH                   <dbl> 3.00, 3.30, 3.26, 3.19, 3.19, 3.26, 3.18,...
    ## $ sulphates            <dbl> 0.45, 0.49, 0.44, 0.40, 0.40, 0.44, 0.47,...
    ## $ alcohol              <dbl> 8.8, 9.5, 10.1, 9.9, 9.9, 10.1, 9.6, 8.8,...
    ## $ quality              <int> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 7,...

``` r
# prints the first 5 rows
head(base_red) 
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.4             0.70        0.00            1.9     0.076
    ## 2           7.8             0.88        0.00            2.6     0.098
    ## 3           7.8             0.76        0.04            2.3     0.092
    ## 4          11.2             0.28        0.56            1.9     0.075
    ## 5           7.4             0.70        0.00            1.9     0.076
    ## 6           7.4             0.66        0.00            1.8     0.075
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  11                   34  0.9978 3.51      0.56     9.4
    ## 2                  25                   67  0.9968 3.20      0.68     9.8
    ## 3                  15                   54  0.9970 3.26      0.65     9.8
    ## 4                  17                   60  0.9980 3.16      0.58     9.8
    ## 5                  11                   34  0.9978 3.51      0.56     9.4
    ## 6                  13                   40  0.9978 3.51      0.56     9.4
    ##   quality
    ## 1       5
    ## 2       5
    ## 3       5
    ## 4       6
    ## 5       5
    ## 6       5

``` r
head(base_white)
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.0             0.27        0.36           20.7     0.045
    ## 2           6.3             0.30        0.34            1.6     0.049
    ## 3           8.1             0.28        0.40            6.9     0.050
    ## 4           7.2             0.23        0.32            8.5     0.058
    ## 5           7.2             0.23        0.32            8.5     0.058
    ## 6           8.1             0.28        0.40            6.9     0.050
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  45                  170  1.0010 3.00      0.45     8.8
    ## 2                  14                  132  0.9940 3.30      0.49     9.5
    ## 3                  30                   97  0.9951 3.26      0.44    10.1
    ## 4                  47                  186  0.9956 3.19      0.40     9.9
    ## 5                  47                  186  0.9956 3.19      0.40     9.9
    ## 6                  30                   97  0.9951 3.26      0.44    10.1
    ##   quality
    ## 1       6
    ## 2       6
    ## 3       6
    ## 4       6
    ## 5       6
    ## 6       6

The red and white wines have identical features, but they might have intrinsic differences to them. As such, we will fit one model for red and one for white. The red wine poses the bigger data paucity problem, so let's start there.

``` r
# let's look at the crosstab of our outcome variable of interest
base_red %>% janitor::tabyl(quality)
```

    ##  quality   n     percent
    ##        3  10 0.006253909
    ##        4  53 0.033145716
    ##        5 681 0.425891182
    ##        6 638 0.398999375
    ##        7 199 0.124452783
    ##        8  18 0.011257036

``` r
# take a look at the distribution of our target variable
hist(base_red$quality, breaks=unique(base_red$quality), col="red") 
```

![](wine_exploration_files/figure-markdown_github/unnamed-chunk-3-1.png)

It looks from this like a wine quality rating of 8 is the rarest--what we might deem exceptional. Lets use this as our definition of an "excellent" wine.

Cleaning the Data
-----------------

``` r
library(janitor) #for data cleaning and tabular exploration
# Janitor also has a great tabular function (tabyl) that we'll use later https://github.com/sfirke/janitor

cleaned_red <- base_red %>%
  janitor::clean_names() %>% #converts to underscore case and cleans; already is in this instance %>%
  janitor::remove_empty(which = c("rows","cols")) %>% # drops all rows and columns that are entirely empty
  mutate(
    high_qual_flag = factor(ifelse(quality >= 8,1,0)) # creates flag for binary outcome
    )

head(cleaned_red)
```

    ##   fixed_acidity volatile_acidity citric_acid residual_sugar chlorides
    ## 1           7.4             0.70        0.00            1.9     0.076
    ## 2           7.8             0.88        0.00            2.6     0.098
    ## 3           7.8             0.76        0.04            2.3     0.092
    ## 4          11.2             0.28        0.56            1.9     0.075
    ## 5           7.4             0.70        0.00            1.9     0.076
    ## 6           7.4             0.66        0.00            1.8     0.075
    ##   free_sulfur_dioxide total_sulfur_dioxide density  p_h sulphates alcohol
    ## 1                  11                   34  0.9978 3.51      0.56     9.4
    ## 2                  25                   67  0.9968 3.20      0.68     9.8
    ## 3                  15                   54  0.9970 3.26      0.65     9.8
    ## 4                  17                   60  0.9980 3.16      0.58     9.8
    ## 5                  11                   34  0.9978 3.51      0.56     9.4
    ## 6                  13                   40  0.9978 3.51      0.56     9.4
    ##   quality high_qual_flag
    ## 1       5              0
    ## 2       5              0
    ## 3       5              0
    ## 4       6              0
    ## 5       5              0
    ## 6       5              0

Prepping Data
-------------

``` r
# split the data into training and testing sets
library(caret) # needed to createDataPartitions
```

    ## Loading required package: lattice

``` r
# Partition data: 80% of the data to train the model
set.seed(777)
in_train <- createDataPartition(y=cleaned_red$high_qual_flag, p=0.80, list=FALSE)

# splits the data into training and testing sets
training <- cleaned_red[in_train,]
testing<-cleaned_red[-in_train,]
# shows the row count and column count of the training set
dim(training)
```

    ## [1] 1280   13

Correlation Checks
==================

<https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html>

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
# should probably trim some size here
pairs(cleaned_red,col=cleaned_red$high_qual_flag)
```

![](wine_exploration_files/figure-markdown_github/unnamed-chunk-6-1.png)

Logistic Regression
===================

<https://www.datacamp.com/community/tutorials/logistic-regression-R>

``` r
# simple logistic regression
logit_fit <- glm(high_qual_flag ~ . -quality, 
                 data = training, 
                 family = binomial)

summary(logit_fit)
```

    ## 
    ## Call:
    ## glm(formula = high_qual_flag ~ . - quality, family = binomial, 
    ##     data = training)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.16819  -0.11104  -0.05019  -0.02573   2.86543  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)           151.48792  371.73731   0.408  0.68363   
    ## fixed_acidity          -0.13366    0.45287  -0.295  0.76789   
    ## volatile_acidity        1.48821    2.51311   0.592  0.55373   
    ## citric_acid             2.21394    2.71250   0.816  0.41439   
    ## residual_sugar          0.04560    0.32314   0.141  0.88777   
    ## chlorides             -35.46187   19.58838  -1.810  0.07024 . 
    ## free_sulfur_dioxide     0.01609    0.04356   0.369  0.71191   
    ## total_sulfur_dioxide   -0.02141    0.01736  -1.233  0.21762   
    ## density              -152.86604  379.89499  -0.402  0.68740   
    ## p_h                    -4.99194    3.43638  -1.453  0.14631   
    ## sulphates               4.76736    1.64868   2.892  0.00383 **
    ## alcohol                 1.07718    0.45431   2.371  0.01774 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 163.22  on 1279  degrees of freedom
    ## Residual deviance: 115.46  on 1268  degrees of freedom
    ## AIC: 139.46
    ## 
    ## Number of Fisher Scoring iterations: 9

``` r
# building a vector of probabilties that a certain wine is high quality 
logit_fit_probs <- predict(logit_fit,
                           newdata = testing,
                           type = "response")

head(logit_fit_probs)
```

    ##           21           22           24           29           30 
    ## 0.0002191739 0.0001463603 0.0002275093 0.0001680044 0.0005842347 
    ##           46 
    ## 0.0035124210

``` r
# building a vector of labels for high quality vs. not high quality 
logit_fit_predictions <- factor(ifelse(logit_fit_probs > 0.5, 1, 0),levels=c('0','1'))
head(logit_fit_predictions)
```

    ## 21 22 24 29 30 46 
    ##  0  0  0  0  0  0 
    ## Levels: 0 1

``` r
confusionMatrix(logit_fit_predictions,testing$high_qual_flag, positive='1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 316   3
    ##          1   0   0
    ##                                           
    ##                Accuracy : 0.9906          
    ##                  95% CI : (0.9728, 0.9981)
    ##     No Information Rate : 0.9906          
    ##     P-Value [Acc > NIR] : 0.6472          
    ##                                           
    ##                   Kappa : 0               
    ##  Mcnemar's Test P-Value : 0.2482          
    ##                                           
    ##             Sensitivity : 0.000000        
    ##             Specificity : 1.000000        
    ##          Pos Pred Value :      NaN        
    ##          Neg Pred Value : 0.990596        
    ##              Prevalence : 0.009404        
    ##          Detection Rate : 0.000000        
    ##    Detection Prevalence : 0.000000        
    ##       Balanced Accuracy : 0.500000        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

The problem is that we have a high success right but no successful positive predictions. We should be able to fix that with some fancier sampling methods.

``` r
up_train <- caret::upSample(select(training, -high_qual_flag), training$high_qual_flag)
up_train %>% janitor::tabyl(Class)
```

    ##  Class    n percent
    ##      0 1265     0.5
    ##      1 1265     0.5

``` r
# upsampled logistic regression
up_logit_fit <- glm(Class ~ . -quality, 
                 data = up_train, 
                 family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(up_logit_fit)
```

    ## 
    ## Call:
    ## glm(formula = Class ~ . - quality, family = binomial, data = up_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.1633  -0.0669   0.0446   0.5141   0.8951  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value             Pr(>|z|)
    ## (Intercept)           302.61294  112.46413   2.691             0.007129
    ## fixed_acidity           0.06989    0.11286   0.619             0.535726
    ## volatile_acidity        4.54732    0.95397   4.767 0.000001872203740035
    ## citric_acid             7.32607    1.12471   6.514 0.000000000073303070
    ## residual_sugar         -0.32572    0.11956  -2.724             0.006442
    ## chlorides            -104.00769    9.13120 -11.390 < 0.0000000000000002
    ## free_sulfur_dioxide     0.04664    0.01207   3.865             0.000111
    ## total_sulfur_dioxide   -0.05328    0.00659  -8.085 0.000000000000000624
    ## density              -300.46596  114.45853  -2.625             0.008662
    ## p_h                    -9.08072    1.07848  -8.420 < 0.0000000000000002
    ## sulphates               8.62106    0.71074  12.130 < 0.0000000000000002
    ## alcohol                 2.11314    0.17785  11.882 < 0.0000000000000002
    ##                         
    ## (Intercept)          ** 
    ## fixed_acidity           
    ## volatile_acidity     ***
    ## citric_acid          ***
    ## residual_sugar       ** 
    ## chlorides            ***
    ## free_sulfur_dioxide  ***
    ## total_sulfur_dioxide ***
    ## density              ** 
    ## p_h                  ***
    ## sulphates            ***
    ## alcohol              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3507.3  on 2529  degrees of freedom
    ## Residual deviance: 1301.3  on 2518  degrees of freedom
    ## AIC: 1325.3
    ## 
    ## Number of Fisher Scoring iterations: 8

``` r
# building a vector of probabilties that a certain wine is high quality 
up_logit_fit_probs <- predict(up_logit_fit,
                           newdata = testing,
                           type = "response")

head(up_logit_fit_probs)
```

    ##            21            22            24            29            30 
    ## 0.00019495944 0.00002523492 0.00003527563 0.00003221242 0.00038952816 
    ##            46 
    ## 0.01236818097

``` r
# building a vector of labels for high quality vs. not high quality 
up_logit_fit_predictions <- factor(ifelse(up_logit_fit_probs > 0.5, 1, 0),levels=c('0','1'))
head(up_logit_fit_predictions)
```

    ## 21 22 24 29 30 46 
    ##  0  0  0  0  0  0 
    ## Levels: 0 1

``` r
confusionMatrix(up_logit_fit_predictions,testing$high_qual_flag, positive='1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 271   0
    ##          1  45   3
    ##                                           
    ##                Accuracy : 0.8589          
    ##                  95% CI : (0.8158, 0.8952)
    ##     No Information Rate : 0.9906          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.1017          
    ##  Mcnemar's Test P-Value : 0.00000000005412
    ##                                           
    ##             Sensitivity : 1.000000        
    ##             Specificity : 0.857595        
    ##          Pos Pred Value : 0.062500        
    ##          Neg Pred Value : 1.000000        
    ##              Prevalence : 0.009404        
    ##          Detection Rate : 0.009404        
    ##    Detection Prevalence : 0.150470        
    ##       Balanced Accuracy : 0.928797        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Trying SMOTE
------------

``` r
library(DMwR)
```

    ## Loading required package: grid

``` r
# both upsampling and downsampling via SMOTE
smote_train <- DMwR::SMOTE(high_qual_flag ~ ., data=as.data.frame(training))
table(smote_train$high_qual_flag)
```

    ## 
    ##  0  1 
    ## 60 45

``` r
# logistic regression built using smote data
smote_logit_fit <- glm(high_qual_flag ~ . -quality, 
                 data = smote_train, 
                 family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(smote_logit_fit)
```

    ## 
    ## Call:
    ## glm(formula = high_qual_flag ~ . - quality, family = binomial, 
    ##     data = smote_train)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.39740  -0.02865  -0.00003   0.17492   1.43687  
    ## 
    ## Coefficients:
    ##                         Estimate  Std. Error z value Pr(>|z|)  
    ## (Intercept)           1525.47208   853.03244   1.788   0.0737 .
    ## fixed_acidity            0.85407     0.75460   1.132   0.2577  
    ## volatile_acidity        15.38786     8.72275   1.764   0.0777 .
    ## citric_acid             19.48623    11.41973   1.706   0.0879 .
    ## residual_sugar          -1.10464     0.73705  -1.499   0.1339  
    ## chlorides             -140.42880    71.39596  -1.967   0.0492 *
    ## free_sulfur_dioxide      0.04070     0.10615   0.383   0.7014  
    ## total_sulfur_dioxide    -0.10457     0.08192  -1.276   0.2018  
    ## density              -1560.65903   873.67598  -1.786   0.0740 .
    ## p_h                    -11.65035     6.17850  -1.886   0.0593 .
    ## sulphates               21.81782     8.80736   2.477   0.0132 *
    ## alcohol                  3.82380     1.50986   2.533   0.0113 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 143.411  on 104  degrees of freedom
    ## Residual deviance:  35.802  on  93  degrees of freedom
    ## AIC: 59.802
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
# testing the smote fit logit
# building a vector of probabilties that a certain wine is high quality 
smote_logit_fit_probs <- predict(smote_logit_fit,
                           newdata = testing,
                           type = "response")

head(smote_logit_fit_probs)
```

    ##                  21                  22                  24 
    ## 0.00000000268516400 0.00000000003329466 0.00000000005354336 
    ##                  29                  30                  46 
    ## 0.00000000013386663 0.00000003798847945 0.00007234466969143

``` r
# building a vector of labels for high quality vs. not high quality 
smote_logit_fit_predictions <- factor(ifelse(smote_logit_fit_probs > 0.5, 1, 0),levels=c('0','1'))
head(smote_logit_fit_predictions)
```

    ## 21 22 24 29 30 46 
    ##  0  0  0  0  0  0 
    ## Levels: 0 1

``` r
confusionMatrix(smote_logit_fit_predictions,testing$high_qual_flag, positive='1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 278   1
    ##          1  38   2
    ##                                           
    ##                Accuracy : 0.8777          
    ##                  95% CI : (0.8367, 0.9116)
    ##     No Information Rate : 0.9906          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.0769          
    ##  Mcnemar's Test P-Value : 0.000000008185  
    ##                                           
    ##             Sensitivity : 0.666667        
    ##             Specificity : 0.879747        
    ##          Pos Pred Value : 0.050000        
    ##          Neg Pred Value : 0.996416        
    ##              Prevalence : 0.009404        
    ##          Detection Rate : 0.006270        
    ##    Detection Prevalence : 0.125392        
    ##       Balanced Accuracy : 0.773207        
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Now trying ROSE
---------------

``` r
library(ROSE)
```

    ## Loaded ROSE 0.0-3

``` r
# using ROSE to generate training set 
rose_train <- ROSE::ROSE(high_qual_flag~., data=training)$data
table(rose_train$high_qual_flag)
```

    ## 
    ##   0   1 
    ## 622 658

``` r
# logistic regression built using ROSE data
rose_logit_fit <- glm(high_qual_flag ~ . -quality, 
                 data = rose_train, 
                 family = binomial)

summary(rose_logit_fit)
```

    ## 
    ## Call:
    ## glm(formula = high_qual_flag ~ . - quality, family = binomial, 
    ##     data = rose_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9583  -0.5416   0.1976   0.7074   2.1687  
    ## 
    ## Coefficients:
    ##                         Estimate  Std. Error z value             Pr(>|z|)
    ## (Intercept)           190.436753   36.750638   5.182      0.0000002196811
    ## fixed_acidity           0.060847    0.040240   1.512             0.130503
    ## volatile_acidity       -1.687415    0.475481  -3.549             0.000387
    ## citric_acid             2.406792    0.434549   5.539      0.0000000304903
    ## residual_sugar         -0.082461    0.053571  -1.539             0.123732
    ## chlorides             -22.152611    3.222259  -6.875      0.0000000000062
    ## free_sulfur_dioxide    -0.004963    0.006284  -0.790             0.429650
    ## total_sulfur_dioxide   -0.008990    0.002660  -3.379             0.000726
    ## density              -197.480770   36.771372  -5.371      0.0000000785172
    ## p_h                    -0.908751    0.414945  -2.190             0.028520
    ## sulphates               4.097056    0.492344   8.322 < 0.0000000000000002
    ## alcohol                 0.715567    0.064353  11.119 < 0.0000000000000002
    ##                         
    ## (Intercept)          ***
    ## fixed_acidity           
    ## volatile_acidity     ***
    ## citric_acid          ***
    ## residual_sugar          
    ## chlorides            ***
    ## free_sulfur_dioxide     
    ## total_sulfur_dioxide ***
    ## density              ***
    ## p_h                  *  
    ## sulphates            ***
    ## alcohol              ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1773.4  on 1279  degrees of freedom
    ## Residual deviance: 1092.3  on 1268  degrees of freedom
    ## AIC: 1116.3
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# testing the rose fit logit
# building a vector of probabilties that a certain wine is high quality 
rose_logit_fit_probs <- predict(rose_logit_fit,
                           newdata = testing,
                           type = "response")

head(rose_logit_fit_probs)
```

    ##         21         22         24         29         30         46 
    ## 0.14101059 0.07646048 0.04158978 0.02311704 0.05988361 0.53829858

``` r
# building a vector of labels for high quality vs. not high quality 
rose_logit_fit_predictions <- factor(ifelse(rose_logit_fit_probs > 0.5, 1, 0),levels=c('0','1'))
head(rose_logit_fit_predictions)
```

    ## 21 22 24 29 30 46 
    ##  0  0  0  0  0  1 
    ## Levels: 0 1

``` r
confusionMatrix(rose_logit_fit_predictions,testing$high_qual_flag, positive='1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 253   0
    ##          1  63   3
    ##                                               
    ##                Accuracy : 0.8025              
    ##                  95% CI : (0.7545, 0.8448)    
    ##     No Information Rate : 0.9906              
    ##     P-Value [Acc > NIR] : 1                   
    ##                                               
    ##                   Kappa : 0.0702              
    ##  Mcnemar's Test P-Value : 0.000000000000005662
    ##                                               
    ##             Sensitivity : 1.000000            
    ##             Specificity : 0.800633            
    ##          Pos Pred Value : 0.045455            
    ##          Neg Pred Value : 1.000000            
    ##              Prevalence : 0.009404            
    ##          Detection Rate : 0.009404            
    ##    Detection Prevalence : 0.206897            
    ##       Balanced Accuracy : 0.900316            
    ##                                               
    ##        'Positive' Class : 1                   
    ## 

Penalized Logistic Regression
=============================

<http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/>

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-13

``` r
# Dumy code categorical predictor variables
x <- model.matrix(high_qual_flag~.-quality, smote_train)
# Convert the outcome (class) to a numerical variable
y <- smote_train$high_qual_flag

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

# Display regression coefficients
coef(model)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                                 s0
    ## (Intercept)           211.43941063
    ## (Intercept)             .         
    ## fixed_acidity           .         
    ## volatile_acidity        .         
    ## citric_acid             0.55444734
    ## residual_sugar         -0.09045859
    ## chlorides             -44.21037381
    ## free_sulfur_dioxide    -0.02507029
    ## total_sulfur_dioxide    .         
    ## density              -210.88863473
    ## p_h                    -5.72341224
    ## sulphates               9.56676238
    ## alcohol                 1.19118973

``` r
# Make predictions on the test data
x.test <- model.matrix(high_qual_flag ~.-quality, testing)
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- testing$high_qual_flag
mean(predicted.classes == observed.classes)
```

    ## [1] 0.8840125

``` r
# Find the optimal value of lambda that minimizes the cross-validation error:
library(glmnet)
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
```

![](wine_exploration_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
# we have two common choices for lambda here, lambda min and lambda lse
# lambda min is the value that minimizes the prediction error
cv.lasso$lambda.min
```

    ## [1] 0.01584114

``` r
# showing coefficients using lambda min
coef(cv.lasso, cv.lasso$lambda.min)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                                  1
    ## (Intercept)           212.19166532
    ## (Intercept)             .         
    ## fixed_acidity           .         
    ## volatile_acidity        .         
    ## citric_acid             0.54028635
    ## residual_sugar         -0.08955781
    ## chlorides             -44.12274667
    ## free_sulfur_dioxide    -0.02510283
    ## total_sulfur_dioxide    .         
    ## density              -211.60709740
    ## p_h                    -5.73482662
    ## sulphates               9.57262301
    ## alcohol                 1.19063466

``` r
# lambda lse gives the simplest model but also lies within one SE of the optimal value of lambda
cv.lasso$lambda.1se
```

    ## [1] 0.05826974

``` r
# showing coefficients using lambda min
coef(cv.lasso, cv.lasso$lambda.1se)
```

    ## 13 x 1 sparse Matrix of class "dgCMatrix"
    ##                                1
    ## (Intercept)           28.4948815
    ## (Intercept)            .        
    ## fixed_acidity          .        
    ## volatile_acidity       .        
    ## citric_acid            .        
    ## residual_sugar         .        
    ## chlorides            -18.8514211
    ## free_sulfur_dioxide    .        
    ## total_sulfur_dioxide   .        
    ## density              -36.4789665
    ## p_h                   -1.0734750
    ## sulphates              7.0984640
    ## alcohol                0.6484565

``` r
# compute model using lambda min
# Final model with lambda.min
lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
# Make prediction on test data
x.test <- model.matrix(high_qual_flag ~.-quality, testing)
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Model accuracy
observed.classes <- testing$high_qual_flag
mean(predicted.classes == observed.classes)
```

    ## [1] 0.8840125

``` r
confusionMatrix(predicted.classes,observed.classes, positive='1')
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 280   1
    ##          1  36   2
    ##                                          
    ##                Accuracy : 0.884          
    ##                  95% CI : (0.8437, 0.917)
    ##     No Information Rate : 0.9906         
    ##     P-Value [Acc > NIR] : 1              
    ##                                          
    ##                   Kappa : 0.0816         
    ##  Mcnemar's Test P-Value : 0.00000002276  
    ##                                          
    ##             Sensitivity : 0.666667       
    ##             Specificity : 0.886076       
    ##          Pos Pred Value : 0.052632       
    ##          Neg Pred Value : 0.996441       
    ##              Prevalence : 0.009404       
    ##          Detection Rate : 0.006270       
    ##    Detection Prevalence : 0.119122       
    ##       Balanced Accuracy : 0.776371       
    ##                                          
    ##        'Positive' Class : 1              
    ##
