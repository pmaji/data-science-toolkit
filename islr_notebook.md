Paul's ISLR Notebook
================
Paul Jeffries

2018-03-05



Simple Linear Regression
========================

-   Master list of slides and videos: <http://www.dataschool.io/15-hours-of-expert-machine-learning-videos/>
-   YT video for this section: <https://www.youtube.com/watch?v=WjyuiK5taS8&list=PL5-da3qGB5IDvuFPNoSqheihPOQNJpzyy>
-   Slides accompany each set of videos.

This section focuses on simple linear regression, multivariate linear regression, and fancier variations thereof.

For this section we're going to use the *Boston* dataset. We'll build a variety of simple (and slightly more complicated linear models). We'll build some plots that help us to better understand the utiility of our modls, and look at some options worth exploring and ways of functionalizing linear regression code.

``` r
require(MASS)
```

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
require(ISLR)
```

    ## Loading required package: ISLR

``` r
names(Boston) #shows variable list of dataset
```

    ##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
    ##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

<br> Next we'll move to do some preliminary plotting of the data.

``` r
par(mfrow=c(1,1)) #ensures only one chart is plotted per page.
# par sets the parameers for the graphics display. The default is mfrow = c(2,2)
# (medv is the dependent variable and lstat is the independnt variable)
plot(medv~lstat,Boston) #plots medv modeled by lstat
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
fit1 <- lm(medv~lstat,Boston) #fits the linear model 'lm' with same variables as above
summary(fit1) #shows p-values, coefficients, st.error, t.value, etc.
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.168  -3.990  -1.318   2.034  24.500 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
    ## lstat       -0.95005    0.03873  -24.53   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.216 on 504 degrees of freedom
    ## Multiple R-squared:  0.5441, Adjusted R-squared:  0.5432 
    ## F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2)) #sets display back to default
plot(fit1) # plots all standard regression plots
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
names(fit1) #shows elements of model summary that can be called indvidually
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "xlevels"       "call"          "terms"         "model"

``` r
fit1$coefficients #shows example of how to call individual elemnets from model
```

    ## (Intercept)       lstat 
    ##  34.5538409  -0.9500494

``` r
confint(fit1) #prints only confidence interval info for model
```

    ##                 2.5 %     97.5 %
    ## (Intercept) 33.448457 35.6592247
    ## lstat       -1.026148 -0.8739505

``` r
# uses the model to predict outcome variable for new inputs
# confidence interval below for 'lstat' input of 5,10,15, etc.
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
```

    ##        fit      lwr      upr
    ## 1 29.80359 29.00741 30.59978
    ## 2 25.05335 24.47413 25.63256
    ## 3 20.30310 19.73159 20.87461

<br>

Multivariate Linear Regression
==============================

``` r
# 2-variable multiple linear regression
# dependent var = medv, ind. vars = lsta, age
fit2 <- lm(medv~lstat+age,data=Boston)
summary(fit2)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat + age, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.981  -3.978  -1.283   1.968  23.158 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
    ## lstat       -1.03207    0.04819 -21.416  < 2e-16 ***
    ## age          0.03454    0.01223   2.826  0.00491 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.173 on 503 degrees of freedom
    ## Multiple R-squared:  0.5513, Adjusted R-squared:  0.5495 
    ## F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16

``` r
# now we can fit the model with all variables in Boston dataset
# the period is short-hand for all variables in the dataset
fit3 <- lm(medv~.,Boston)
summary(fit3)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ ., data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.595  -2.730  -0.518   1.777  26.199 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
    ## crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
    ## zn           4.642e-02  1.373e-02   3.382 0.000778 ***
    ## indus        2.056e-02  6.150e-02   0.334 0.738288    
    ## chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
    ## nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
    ## rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
    ## age          6.922e-04  1.321e-02   0.052 0.958229    
    ## dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
    ## rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
    ## tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
    ## ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
    ## black        9.312e-03  2.686e-03   3.467 0.000573 ***
    ## lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.745 on 492 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7338 
    ## F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2)) #ensures the graphical layout is as we want for our window
plot (fit3) #show our summary regression plots for the all-variable model
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
# Now we can update the model to remove variables with low explanatory power
# From our summary, age and indus have higher p-values, so they can be dropped
# update function updates model by getting rid of variables and keeping all others
fit4 <- update(fit3,~.-age-indus)
summary(fit4)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
    ##     tax + ptratio + black + lstat, data = Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.5984  -2.7386  -0.5046   1.7273  26.2373 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  36.341145   5.067492   7.171 2.73e-12 ***
    ## crim         -0.108413   0.032779  -3.307 0.001010 ** 
    ## zn            0.045845   0.013523   3.390 0.000754 ***
    ## chas          2.718716   0.854240   3.183 0.001551 ** 
    ## nox         -17.376023   3.535243  -4.915 1.21e-06 ***
    ## rm            3.801579   0.406316   9.356  < 2e-16 ***
    ## dis          -1.492711   0.185731  -8.037 6.84e-15 ***
    ## rad           0.299608   0.063402   4.726 3.00e-06 ***
    ## tax          -0.011778   0.003372  -3.493 0.000521 ***
    ## ptratio      -0.946525   0.129066  -7.334 9.24e-13 ***
    ## black         0.009291   0.002674   3.475 0.000557 ***
    ## lstat        -0.522553   0.047424 -11.019  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.736 on 494 degrees of freedom
    ## Multiple R-squared:  0.7406, Adjusted R-squared:  0.7348 
    ## F-statistic: 128.2 on 11 and 494 DF,  p-value: < 2.2e-16

<br>

Nonlinear Terms and Interactions
================================

``` r
# model below fits a model with same dependent variable: medv
# indepent vars: age, lstat, and an lstat/age interaction term
fit5 <- lm(medv~lstat*age,Boston)
summary(fit5)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat * age, data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.806  -4.045  -1.333   2.085  27.552 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
    ## lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
    ## age         -0.0007209  0.0198792  -0.036   0.9711    
    ## lstat:age    0.0041560  0.0018518   2.244   0.0252 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.149 on 502 degrees of freedom
    ## Multiple R-squared:  0.5557, Adjusted R-squared:  0.5531 
    ## F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

``` r
# we can also model ind. variables in quadratic form
# here we one of our ind. vars to a 2nd degree polynomial
fit6 <- lm(medv~lstat + I(lstat^2),Boston)
summary(fit6)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ lstat + I(lstat^2), data = Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.2834  -3.8313  -0.5295   2.3095  25.4148 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 42.862007   0.872084   49.15   <2e-16 ***
    ## lstat       -2.332821   0.123803  -18.84   <2e-16 ***
    ## I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.524 on 503 degrees of freedom
    ## Multiple R-squared:  0.6407, Adjusted R-squared:  0.6393 
    ## F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16

``` r
# standard regression plots for the quadratic model
par(mfrow=c(2,2))
plot(fit6)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
par(mfrow=c(1,1)) # makes it so that all four charts produced next output to same window
#below is the ploted fit6 quadratic model overlayed on the scatterplot of mdev modeled by lstat
#as can be seen, the fit appears to be quite accurate
plot(Boston$medv~Boston$lstat)
# we'll plot the points in red for visibility
points(Boston$lstat,fitted(fit6),col="red",pch=20)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
# there are easier ways to fit higher degree polynomial models
# the function poly() does this and includes descending polynomials too
fit6 <-lm(medv~poly(lstat,5),data=Boston)
summary(fit6)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ poly(lstat, 5), data = Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5433  -3.1039  -0.7052   2.0844  27.1153 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       22.5328     0.2318  97.197  < 2e-16 ***
    ## poly(lstat, 5)1 -152.4595     5.2148 -29.236  < 2e-16 ***
    ## poly(lstat, 5)2   64.2272     5.2148  12.316  < 2e-16 ***
    ## poly(lstat, 5)3  -27.0511     5.2148  -5.187 3.10e-07 ***
    ## poly(lstat, 5)4   25.4517     5.2148   4.881 1.42e-06 ***
    ## poly(lstat, 5)5  -19.2524     5.2148  -3.692 0.000247 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.215 on 500 degrees of freedom
    ## Multiple R-squared:  0.6817, Adjusted R-squared:  0.6785 
    ## F-statistic: 214.2 on 5 and 500 DF,  p-value: < 2.2e-16

``` r
# we can also use logged independent variables rather simply too, as below
# here we have logged rm as our ind. varible, medv as our dependent 
fit7 <- lm(medv~log(rm),data=Boston)
summary(fit7)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ log(rm), data = Boston)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -19.487  -2.875  -0.104   2.837  39.816 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -76.488      5.028  -15.21   <2e-16 ***
    ## log(rm)       54.055      2.739   19.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.915 on 504 degrees of freedom
    ## Multiple R-squared:  0.4358, Adjusted R-squared:  0.4347 
    ## F-statistic: 389.3 on 1 and 504 DF,  p-value: < 2.2e-16

Now we can also include qualitative predictors in our model as well

``` r
# for this we'll use a different dataset "Carseats"
summary(Carseats)
```

    ##      Sales          CompPrice       Income        Advertising    
    ##  Min.   : 0.000   Min.   : 77   Min.   : 21.00   Min.   : 0.000  
    ##  1st Qu.: 5.390   1st Qu.:115   1st Qu.: 42.75   1st Qu.: 0.000  
    ##  Median : 7.490   Median :125   Median : 69.00   Median : 5.000  
    ##  Mean   : 7.496   Mean   :125   Mean   : 68.66   Mean   : 6.635  
    ##  3rd Qu.: 9.320   3rd Qu.:135   3rd Qu.: 91.00   3rd Qu.:12.000  
    ##  Max.   :16.270   Max.   :175   Max.   :120.00   Max.   :29.000  
    ##    Population        Price        ShelveLoc        Age       
    ##  Min.   : 10.0   Min.   : 24.0   Bad   : 96   Min.   :25.00  
    ##  1st Qu.:139.0   1st Qu.:100.0   Good  : 85   1st Qu.:39.75  
    ##  Median :272.0   Median :117.0   Medium:219   Median :54.50  
    ##  Mean   :264.8   Mean   :115.8                Mean   :53.32  
    ##  3rd Qu.:398.5   3rd Qu.:131.0                3rd Qu.:66.00  
    ##  Max.   :509.0   Max.   :191.0                Max.   :80.00  
    ##    Education    Urban       US     
    ##  Min.   :10.0   No :118   No :142  
    ##  1st Qu.:12.0   Yes:282   Yes:258  
    ##  Median :14.0                      
    ##  Mean   :13.9                      
    ##  3rd Qu.:16.0                      
    ##  Max.   :18.0

``` r
# the model below fits a multivaraite regression model including all ind. vars
# it also includes two interaction effects variables
fit8 <- lm(Sales~.+Income:Advertising+Age:Price,Carseats)
# in the output below we can see that in some cases R automatically creates dummy vars
# for example ShelveLocGood being a dummy variable for qualitative ranking of "good"
summary(fit8)
```

    ## 
    ## Call:
    ## lm(formula = Sales ~ . + Income:Advertising + Age:Price, data = Carseats)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9208 -0.7503  0.0177  0.6754  3.3413 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         6.5755654  1.0087470   6.519 2.22e-10 ***
    ## CompPrice           0.0929371  0.0041183  22.567  < 2e-16 ***
    ## Income              0.0108940  0.0026044   4.183 3.57e-05 ***
    ## Advertising         0.0702462  0.0226091   3.107 0.002030 ** 
    ## Population          0.0001592  0.0003679   0.433 0.665330    
    ## Price              -0.1008064  0.0074399 -13.549  < 2e-16 ***
    ## ShelveLocGood       4.8486762  0.1528378  31.724  < 2e-16 ***
    ## ShelveLocMedium     1.9532620  0.1257682  15.531  < 2e-16 ***
    ## Age                -0.0579466  0.0159506  -3.633 0.000318 ***
    ## Education          -0.0208525  0.0196131  -1.063 0.288361    
    ## UrbanYes            0.1401597  0.1124019   1.247 0.213171    
    ## USYes              -0.1575571  0.1489234  -1.058 0.290729    
    ## Income:Advertising  0.0007510  0.0002784   2.698 0.007290 ** 
    ## Price:Age           0.0001068  0.0001333   0.801 0.423812    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.011 on 386 degrees of freedom
    ## Multiple R-squared:  0.8761, Adjusted R-squared:  0.8719 
    ## F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

``` r
# in order to see how coding for qualitative variables is done we can use "contrasts"
contrasts(Carseats$ShelveLoc)
```

    ##        Good Medium
    ## Bad       0      0
    ## Good      1      0
    ## Medium    0      1

``` r
# finally, we can use what we learned to functionalize regression plot creation
regplot <- function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...) # allows for user-entered options here
  abline(fit,col="red")
}

# below we use this simple function, along with a few added options
regplot(Carseats$Price,Carseats$Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
# we can change up the various plot point types for our charts easily, a sample is below
# below are some of the main pch types used in simple plots 
plot(1:20,1:20,pch=1:20,cex=2)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-24-1.png)

<br>

Logistic Regression
===================

``` r
# Loading packages
require(ISLR) # grabs the datasets we need
require(MASS) # statistics and econometrics package
require(class) # used primarily for KNN analysis
```

    ## Loading required package: class

``` r
# Exploring the data we'll work with (Stock Market Data)
# other methods of dataset exploration include names(), summary(), and dim()
head(Smarket)
```

    ##   Year   Lag1   Lag2   Lag3   Lag4   Lag5 Volume  Today Direction
    ## 1 2001  0.381 -0.192 -2.624 -1.055  5.010 1.1913  0.959        Up
    ## 2 2001  0.959  0.381 -0.192 -2.624 -1.055 1.2965  1.032        Up
    ## 3 2001  1.032  0.959  0.381 -0.192 -2.624 1.4112 -0.623      Down
    ## 4 2001 -0.623  1.032  0.959  0.381 -0.192 1.2760  0.614        Up
    ## 5 2001  0.614 -0.623  1.032  0.959  0.381 1.2057  0.213        Up
    ## 6 2001  0.213  0.614 -0.623  1.032  0.959 1.3491  1.392        Up

``` r
# To further explore the data, we can build a correlation matrix
# We drop the last column becauuse it is string and will otherwise throw an error
cor(Smarket[,-9])
```

    ##              Year         Lag1         Lag2         Lag3         Lag4
    ## Year   1.00000000  0.029699649  0.030596422  0.033194581  0.035688718
    ## Lag1   0.02969965  1.000000000 -0.026294328 -0.010803402 -0.002985911
    ## Lag2   0.03059642 -0.026294328  1.000000000 -0.025896670 -0.010853533
    ## Lag3   0.03319458 -0.010803402 -0.025896670  1.000000000 -0.024051036
    ## Lag4   0.03568872 -0.002985911 -0.010853533 -0.024051036  1.000000000
    ## Lag5   0.02978799 -0.005674606 -0.003557949 -0.018808338 -0.027083641
    ## Volume 0.53900647  0.040909908 -0.043383215 -0.041823686 -0.048414246
    ## Today  0.03009523 -0.026155045 -0.010250033 -0.002447647 -0.006899527
    ##                Lag5      Volume        Today
    ## Year    0.029787995  0.53900647  0.030095229
    ## Lag1   -0.005674606  0.04090991 -0.026155045
    ## Lag2   -0.003557949 -0.04338321 -0.010250033
    ## Lag3   -0.018808338 -0.04182369 -0.002447647
    ## Lag4   -0.027083641 -0.04841425 -0.006899527
    ## Lag5    1.000000000 -0.02200231 -0.034860083
    ## Volume -0.022002315  1.00000000  0.014591823
    ## Today  -0.034860083  0.01459182  1.000000000

``` r
# now we fit a logistic regression model w/ Direction as the outcome variable
# the argument (family = binomial) yields the logit
logit_fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(logit_fit) #shows p values, error, coefs, etc.
```

    ## 
    ## Call:
    ## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    ##     Volume, family = binomial, data = Smarket)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.446  -1.203   1.065   1.145   1.326  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -0.126000   0.240736  -0.523    0.601
    ## Lag1        -0.073074   0.050167  -1.457    0.145
    ## Lag2        -0.042301   0.050086  -0.845    0.398
    ## Lag3         0.011085   0.049939   0.222    0.824
    ## Lag4         0.009359   0.049974   0.187    0.851
    ## Lag5         0.010313   0.049511   0.208    0.835
    ## Volume       0.135441   0.158360   0.855    0.392
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1731.2  on 1249  degrees of freedom
    ## Residual deviance: 1727.6  on 1243  degrees of freedom
    ## AIC: 1741.6
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
# if we want just the coefficients, we can use (coef(logit_fit))
# if we want all the info about the coefficients, we use summary('model')$coef
summary(logit_fit)$coef
```

    ##                 Estimate Std. Error    z value  Pr(>|z|)
    ## (Intercept) -0.126000257 0.24073574 -0.5233966 0.6006983
    ## Lag1        -0.073073746 0.05016739 -1.4565986 0.1452272
    ## Lag2        -0.042301344 0.05008605 -0.8445733 0.3983491
    ## Lag3         0.011085108 0.04993854  0.2219750 0.8243333
    ## Lag4         0.009358938 0.04997413  0.1872757 0.8514445
    ## Lag5         0.010313068 0.04951146  0.2082966 0.8349974
    ## Volume       0.135440659 0.15835970  0.8552723 0.3924004

``` r
# we can use standard subsetting syntax to show particular coefficient information
# for example, to just show the p-values, we use the code below
summary(logit_fit)$coef[,4]
```

    ## (Intercept)        Lag1        Lag2        Lag3        Lag4        Lag5 
    ##   0.6006983   0.1452272   0.3983491   0.8243333   0.8514445   0.8349974 
    ##      Volume 
    ##   0.3924004

``` r
logit_probs <- predict(logit_fit,type="response")
# shows the first 10 predictions made by the logistic regression model
logit_probs[1:10]
```

    ##         1         2         3         4         5         6         7 
    ## 0.5070841 0.4814679 0.4811388 0.5152224 0.5107812 0.5069565 0.4926509 
    ##         8         9        10 
    ## 0.5092292 0.5176135 0.4888378

``` r
# We know that the outcome variable can only take the form Up or Down
# As such, we can transform our logit_probs into an UP / DOWN prediction
# First we created a fector filled with no's to overwrite with model results
logit_pred <- rep("Down",1250)
# Were model prediction is >.5, overwrite "Down" with "Up"
logit_pred[logit_probs>.5] = "Up" 
# Now we create a table with model results vs. actual results
table(logit_pred,Smarket$Direction)
```

    ##           
    ## logit_pred Down  Up
    ##       Down  145 141
    ##       Up    457 507

``` r
# The overall successful classification rate can be found by the mean
mean(logit_pred == Smarket$Direction)
```

    ## [1] 0.5216

The previous version of the logit model was run on all of our data, with no splitting into testing and training sets. This can lead to a problem of overfitting. For a more robust model, we need to re-estimate our model just on the training data (as done below), and then use our test set as validation.

``` r
# Now we begin testing the logit model more rigorously
# We need to test it across time with a training and a test set
train <- (Smarket$Year<2005) # training set is all years before 2005
smarket_2005 <- Smarket[!train,] # creates our test set (which is just the year 2005)
direction_2005 <- Smarket$Direction[!train] # vector of response variables for test set
```

``` r
# Now we'll fit the same logit model from before but only using the training set
logit_fit2 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                  data = Smarket, family=binomial,subset=train)
# Now we can use the logit model fit on the training data to predict the 2005 response variable
# The general pattern is: fit on training set, test predictive power on test set
logit_probs2 <- predict(logit_fit2,smarket_2005,type="response")
# now we have our predictions
head(logit_probs2)
```

    ##       999      1000      1001      1002      1003      1004 
    ## 0.5282195 0.5156688 0.5226521 0.5138543 0.4983345 0.5010912

``` r
# Analyzing Our Predictions
dim(smarket_2005) # gets us row and column counts for 2005 (test) stock market data
```

    ## [1] 252   9

``` r
# we know from this result that we are going to need 252 responses to check
```

``` r
logit_pred2 <- rep("Down",252) # row number we need given dim() called previously
# changes "down" to "yes" whenever predicted value greater than .5
logit_pred2[logit_probs2>0.5] <- "Up" 
# now we'll show the predicted vs. actual response variables
table(logit_pred2,direction_2005)
```

    ##            direction_2005
    ## logit_pred2 Down Up
    ##        Down   77 97
    ##        Up     34 44

``` r
# finally, the success rate for the new model (trained and tested more robustly)
mean(logit_pred2==direction_2005)
```

    ## [1] 0.4801587

``` r
# here we see the model actually degraded (likey due to removal of some overfitting)
```

``` r
# if the model still isnt' performing well, then we can consider fewer ind. variables (as below)
# fewer explanatory terms may prove more effective (hence techniques like ridge regression)
logit_fit3 <- glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
summary(logit_fit3)
```

    ## 
    ## Call:
    ## glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = Smarket, 
    ##     subset = train)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.345  -1.188   1.074   1.164   1.326  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  0.03222    0.06338   0.508    0.611
    ## Lag1        -0.05562    0.05171  -1.076    0.282
    ## Lag2        -0.04449    0.05166  -0.861    0.389
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1383.3  on 997  degrees of freedom
    ## Residual deviance: 1381.4  on 995  degrees of freedom
    ## AIC: 1387.4
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
# once we fit the model, we perform the same steps as done previously to test model accuracy
# we form our predictions based on the model, classify our test set observations, and check them
logit_probs3 <- predict(logit_fit3,smarket_2005,type="response")
logit_pred3 <- rep("Down",252)
logit_pred3[logit_probs3>0.5]<-"Up"
table(logit_pred3,direction_2005)
```

    ##            direction_2005
    ## logit_pred3 Down  Up
    ##        Down   35  35
    ##        Up     76 106

``` r
# we can see as well that this smaller model with fewer terms yields a higher success rate
mean(logit_pred3==direction_2005)
```

    ## [1] 0.5595238

``` r
# now let's predict our outocme variable for particuarl new input data (beyond test set)
# we can manually predict Direction using our model for any value of our ind. variables
# below we predict Direction for 2 pairings of Lag1 and Lag 2
# as we can see, both would be predicted as Down days.
predict(logit_fit3,newdata=data.frame(Lag1=c(1.2,1.4),Lag2=c(1.1,-0.8)),type="response")
```

    ##         1         2 
    ## 0.4791462 0.4974843

<br>

Linear Discrimant Analysis
==========================

-   Linear discriminant analysis is generally more sensitive to outliers than logistic regression.

``` r
# First we'll fit the LDA model on the same data as before
lda_fit <- lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
# note that with LDA / QDA calling summary(model name) is not nearly as helpful
(lda_fit)
```

    ## Call:
    ## lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##     Down       Up 
    ## 0.491984 0.508016 
    ## 
    ## Group means:
    ##             Lag1        Lag2
    ## Down  0.04279022  0.03389409
    ## Up   -0.03954635 -0.03132544
    ## 
    ## Coefficients of linear discriminants:
    ##             LD1
    ## Lag1 -0.6420190
    ## Lag2 -0.5135293

``` r
# plots the LDA model details in histogram
plot(lda_fit)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-44-1.png)

``` r
lda_pred <- predict(lda_fit, smarket_2005) # uses LDA to predict for test set of 2005
# names(lda_pred) # this would show the names of the variables output by the prediction 
lda_class <- lda_pred$class # creates a vector of the LDA model's classifications
# now we can see the same predicted vs. actual table as previously done with regression
table(lda_class,direction_2005)
```

    ##          direction_2005
    ## lda_class Down  Up
    ##      Down   35  35
    ##      Up     76 106

``` r
# shows the accuracy perecent of the basic LDA model 
mean(lda_class == direction_2005)
```

    ## [1] 0.5595238

``` r
# We can see by quick comparison that the LDA and logistic regression predictions are identical
mean(logit_pred3==direction_2005)
```

    ## [1] 0.5595238

``` r
# now we can look more deeply into the posterior probabilities
# the posterior probability output by the model = probability of market decrease
sum(lda_pred$posterior[,1]<.5) / nrow(lda_pred$posterior) # number of predictions of market increase
```

    ## [1] 0.7222222

``` r
# the method on the line below shows the probabilty of the market being up / down by observation
lda_pred$posterior[1:20,1] # shows the first 20 probabilities of teh market going down
```

    ##       999      1000      1001      1002      1003      1004      1005 
    ## 0.4901792 0.4792185 0.4668185 0.4740011 0.4927877 0.4938562 0.4951016 
    ##      1006      1007      1008      1009      1010      1011      1012 
    ## 0.4872861 0.4907013 0.4844026 0.4906963 0.5119988 0.4895152 0.4706761 
    ##      1013      1014      1015      1016      1017      1018 
    ## 0.4744593 0.4799583 0.4935775 0.5030894 0.4978806 0.4886331

``` r
lda_class[1:20] #viewing the first 20 days of the market going down 
```

    ##  [1] Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Up   Down Up   Up  
    ## [15] Up   Up   Up   Down Up   Up  
    ## Levels: Down Up

``` r
max(lda_pred$posterior[,1])
```

    ## [1] 0.520235

``` r
# the greatest posterior probabilty of decrease in all 2005 was 52.05%
```

``` r
sum(lda_pred$posterior[,1]>.9) 
```

    ## [1] 0

``` r
# no single observation > .9 (i.e. the model is never very confident)
```

<br>

Quadratic Discriminant Analysis
===============================

``` r
# QDA involves a quadratic rather than a linear model
qda_fit <- qda(Direction~Lag1+Lag2,data=Smarket,subset = train)
qda_fit
```

    ## Call:
    ## qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
    ## 
    ## Prior probabilities of groups:
    ##     Down       Up 
    ## 0.491984 0.508016 
    ## 
    ## Group means:
    ##             Lag1        Lag2
    ## Down  0.04279022  0.03389409
    ## Up   -0.03954635 -0.03132544

``` r
qda_class <- predict(qda_fit,smarket_2005)$class
# having formed our predictions, we can look at the tables success rate (skipping table)
mean(qda_class==direction_2005)
```

    ## [1] 0.5992063

``` r
# here we see a slightly higher hit rate on our test set than with linear methods 
```

K - Nearest Neighbors
=====================

``` r
# creationg out test and training sets using a slightly different method
train_x <- cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test_x <- cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train_direction <- Smarket$Direction[train] # outcome variable in training set
set.seed(1) # setting our seed for reproducibilty of any decisions due to randomization
knn_pred <- knn(train_x,test_x,train_direction,k=2) # training model with 2 classes # there are empirical methods of picking classes, but 2 makes sense here (up/down)
# picking a value of k=1 wouldn't make sense because we have 2 possible outcomesd
# as can be seen, the knn model perform slightly better than average
mean(knn_pred==direction_2005)
```

    ## [1] 0.4960317

``` r
# Now we can try retraining the model with a higher value of k
# We'll build a function that tests success rates for various values of k
# It will test all values of k up to and including the argument input (k_count)

k_val_test = function(k_count){
  k_results = data.frame(1:k_count,rep(0,k_count))
  colnames(k_results) <- (c("K-value","Success Rate"))
  
  for(k in 1:k_count){
    knn_pred=knn(train_x,test_x,train_direction,k=k_count)
    k_results[k,2] = mean(knn_pred==direction_2005)
  }
  
  return(k_results) # returns a dataframe of all sucess rates for each value of k
  
}

k_val_test(10)
```

    ##    K-value Success Rate
    ## 1        1    0.4563492
    ## 2        2    0.5000000
    ## 3        3    0.5396825
    ## 4        4    0.5158730
    ## 5        5    0.4920635
    ## 6        6    0.4880952
    ## 7        7    0.5277778
    ## 8        8    0.5079365
    ## 9        9    0.5198413
    ## 10      10    0.5119048

<br>

Cross-validation
================

This section compares the utility of different types of cross-validation (leave-one-out vs. 10-fold). The YT video for this section can be found here:

``` r
# bring in the necessary packages
require(ISLR)
require(boot) 
```

    ## Loading required package: boot

``` r
# basic plot of mpv vs. horsepower to remind us of data we'll be playing with
plot(mpg~horsepower, data=Auto)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-57-1.png)

<br>

Leave-One-Out Cross Validation (LOOCV)
--------------------------------------

``` r
# ?cv.glm shows details for the cross validation function from the glm package

# simple model of mpg using horsepower that we will cross-validate 
glm_fit <- glm(mpg~horsepower, data=Auto) 

# now we estiamte K-fold cross-validation prediction error for our glm_fit model
# the below call is pretty slow; we'll improve on it later 
cv.glm(Auto,glm_fit)$delta 
```

    ## [1] 24.23151 24.23114

``` r
# 2 outputs of cv.glm: RAW LOOCV error, and bias-corrected LOOCV error
# bias correction accounts for fact that the LOO training set is smaller than the full dataset
# the impact of this kind of bias correction is obviously more important in k-fold vs. LOOCV

# We can now write a slightly faster function for LOOCV
# It is faster because it takes into account the self-influence of each value
# (see video for additional explanation)

# Formula 5.2 from ISLR:

loocv <- function(fitted_model){
  h = lm.influence(fitted_model)$h
  LOOCV_prediction_error = mean((residuals(fitted_model)/(1-h))^2)
  answer = as.data.frame(LOOCV_prediction_error)
  return(answer)
}

# Now we test our formula
loocv(glm_fit)
```

    ##   LOOCV_prediction_error
    ## 1               24.23151

``` r
# Continuing with the same example data:

# 1st I create an empty vector with 5 values = 0 to change later with a loop
cv_error <- rep(0,5) 

# Then I create a 2nd vector with values 1:5 (also for upcoming loop)
degree <- 1:5

# Loop iterats through each degree of polynomial, fitting a different degree model (1:5)
# During each loop, it takes the LOOCV error and outputs it to the cv_error vector

for(d in degree){
  glm_fit = glm(mpg~poly(horsepower, d), data=Auto)
  cv_error[d] = loocv(glm_fit)
}

# Now I take the result of the loop in order to plot the error for each degree polynomial
plot(degree,cv_error,type="b", main = "LOOCV Error by Degree of Polynomial in Model")
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-60-1.png)

``` r
# In this case we can see that the model prediction error drops off at poly=2
```

``` r
# Now we can do the same thing that we did above but for 10-fold Cross-Validation 

cv_error10 <- rep(0,5) # making a new blank vector for our 10-fold CV error

for(d in degree){
  glm_fit = glm(mpg~poly(horsepower,d), data=Auto)
  cv_error10[d] = cv.glm(Auto,glm_fit,K=10)$delta[1]
}

# plots the 10-fold CV error on top of the LOOCV error rates
plot(degree,cv_error,type="b", main = "LOOCV Error by Degree of Polynomial in Model")
lines(degree,cv_error10,type="b",col="red")
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-61-1.png)

``` r
# we can see that the two methods track almost identically in term of their error
# that said, the 10-fold is much less computationally intensive
# as such, especially in large-N cases, we prefer to use 10-fold CV techniques
```

<br>

Boostrap
========

This sections examines the utility of the boostrap method of estimating modle prediction error. The YT video for this section can be found here.

The example case that we'll use here will be picking the optimal combination of 2 investments (investment X and investment Y).

``` r
# we begin by brining in the standard necessary packages
require(ISLR)
require(boot)

# The function below is not the textbook financial alpha function
# Its purpose is to give us some function that we can then use with a boostrap method

alpha <- function(x,y){
  vx=var(x) # variance of x
  vy=var(y) # variance of y
  cxy=cov(x,y) # covariance of x and y
  alpha=((vy-cxy)/(vx+vy-2*cxy)) # computes alpha
  return(alpha)
  
}

alpha(Portfolio$X, Portfolio$Y)
```

    ## [1] 0.5758321

``` r
# Now we want to investigate the standard error of alpha:
# We create a function that slices as dataset and calculates the alpha within that range

alpha_fn <- function(data, index){
  with(data[index,], alpha(X,Y))
}

alpha_fn(Portfolio,1:100)
```

    ## [1] 0.5758321

``` r
# we set the seed here because boostrapping relies on some element of randomization
# always set seed if we want to maintain reproducibility 
set.seed(1315)

# here we calculate alpha for a random sample (with replacement) of 100 observations
# this version samples only from the first 100 observations 
# using all 100 observations to get alpha:
alpha_fn(Portfolio,sample(1:100,100,replace=TRUE))
```

    ## [1] 0.5567061

``` r
# if we wanted to sample from the entire dataset, we drop the first argument 
alpha_fn(Portfolio,sample(100,replace=TRUE))
```

    ## [1] 0.6749852

``` r
# now we move on to the actual boostrapping
boot_out <- boot(Portfolio,alpha_fn,R=1000)
boot_out
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Portfolio, statistic = alpha_fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original      bias    std. error
    ## t1* 0.5758321 0.004624873  0.09083667

``` r
plot(boot_out)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-66-1.png)

Estimating the Accuracy of a Linear Regression Model via Bootstrapping
----------------------------------------------------------------------

``` r
boot_fn <- function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}

#simply compute coefficient estimates
boot_fn(Auto,1:392)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
set.seed(1)

#one bootstrap round
boot_fn(Auto,sample(392,392,replace=T))
```

    ## (Intercept)  horsepower 
    ##  38.7387134  -0.1481952

``` r
#now do a thousand!
boot(Auto,boot_fn,1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot_fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original        bias    std. error
    ## t1* 39.9358610  0.0296667441 0.860440524
    ## t2* -0.1578447 -0.0003113047 0.007411218

``` r
#however in the simple case of linear regression, we can also get these
# estimates with the summary() function from the fit itself
# as was described in section 3.1.2
summary(lm(mpg~horsepower,data=Auto))$coef
```

    ##               Estimate  Std. Error   t value      Pr(>|t|)
    ## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
    ## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

<br>

Tree-Based Methods
==================

(Bagging, random forest, and boosting)

<https://www.youtube.com/watch?v=0wZUXtvAtDc&index=6&list=PL5-da3qGB5IB23TLuA8ZgVGC8hV8ZAdGh>

Decision Trees

``` r
library(ISLR)
library(tree)
attach(Carseats)
hist(Sales)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-71-1.png)

``` r
High=ifelse(Carseats$Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High) #adds the variable to dataframe
```

Now we fit a tree to these data, summarize, and plot it. Notice that we have to *exclude* 'Sales' from the right-hand side of the formula, because the response is derived from it.

``` r
# model high as a subset of everything except Sales
tree.carseats=tree(High~CompPrice+Income+Advertising+Population+Price,data=Carseats)
summary(tree.carseats)
```

    ## 
    ## Classification tree:
    ## tree(formula = High ~ CompPrice + Income + Advertising + Population + 
    ##     Price, data = Carseats)
    ## Variables actually used in tree construction:
    ## [1] "Price"       "Advertising" "CompPrice"   "Income"     
    ## Number of terminal nodes:  19 
    ## Residual mean deviance:  0.7637 = 291 / 381 
    ## Misclassification error rate: 0.175 = 70 / 400

``` r
plot(tree.carseats)
text(tree.carseats,pretty = 0) # enables tree annotations
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-73-1.png)

``` r
tree.carseats #gives more detailed print out of all terminal nodes
```

    ## node), split, n, deviance, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##   1) root 400 541.500 No ( 0.59000 0.41000 )  
    ##     2) Price < 92.5 62  66.240 Yes ( 0.22581 0.77419 ) *
    ##     3) Price > 92.5 338 434.800 No ( 0.65680 0.34320 )  
    ##       6) Advertising < 6.5 181 177.800 No ( 0.80663 0.19337 )  
    ##        12) CompPrice < 129.5 95  49.980 No ( 0.92632 0.07368 )  
    ##          24) Income < 74.5 55   0.000 No ( 1.00000 0.00000 ) *
    ##          25) Income > 74.5 40  37.100 No ( 0.82500 0.17500 )  
    ##            50) Income < 78.5 5   6.730 Yes ( 0.40000 0.60000 ) *
    ##            51) Income > 78.5 35  24.880 No ( 0.88571 0.11429 ) *
    ##        13) CompPrice > 129.5 86 108.500 No ( 0.67442 0.32558 )  
    ##          26) Price < 127 39  54.040 Yes ( 0.48718 0.51282 ) *
    ##          27) Price > 127 47  42.890 No ( 0.82979 0.17021 )  
    ##            54) CompPrice < 147.5 32   8.900 No ( 0.96875 0.03125 ) *
    ##            55) CompPrice > 147.5 15  20.730 No ( 0.53333 0.46667 )  
    ##             110) Price < 151.5 10  12.220 Yes ( 0.30000 0.70000 )  
    ##               220) Price < 137.5 5   6.730 No ( 0.60000 0.40000 ) *
    ##               221) Price > 137.5 5   0.000 Yes ( 0.00000 1.00000 ) *
    ##             111) Price > 151.5 5   0.000 No ( 1.00000 0.00000 ) *
    ##       7) Advertising > 6.5 157 217.500 Yes ( 0.48408 0.51592 )  
    ##        14) Price < 136.5 129 174.000 Yes ( 0.40310 0.59690 )  
    ##          28) CompPrice < 112.5 27  32.820 No ( 0.70370 0.29630 )  
    ##            56) Income < 101 20  13.000 No ( 0.90000 0.10000 ) *
    ##            57) Income > 101 7   5.742 Yes ( 0.14286 0.85714 ) *
    ##          29) CompPrice > 112.5 102 128.400 Yes ( 0.32353 0.67647 )  
    ##            58) Price < 126.5 69  74.730 Yes ( 0.23188 0.76812 )  
    ##             116) CompPrice < 137.5 58  68.320 Yes ( 0.27586 0.72414 ) *
    ##             117) CompPrice > 137.5 11   0.000 Yes ( 0.00000 1.00000 ) *
    ##            59) Price > 126.5 33  45.720 No ( 0.51515 0.48485 )  
    ##             118) CompPrice < 146.5 28  37.520 No ( 0.60714 0.39286 )  
    ##               236) CompPrice < 125.5 11  12.890 Yes ( 0.27273 0.72727 ) *
    ##               237) CompPrice > 125.5 17  15.840 No ( 0.82353 0.17647 )  
    ##                 474) Advertising < 13.5 12   0.000 No ( 1.00000 0.00000 ) *
    ##                 475) Advertising > 13.5 5   6.730 Yes ( 0.40000 0.60000 ) *
    ##             119) CompPrice > 146.5 5   0.000 Yes ( 0.00000 1.00000 ) *
    ##        15) Price > 136.5 28  22.970 No ( 0.85714 0.14286 )  
    ##          30) CompPrice < 136.5 14   0.000 No ( 1.00000 0.00000 ) *
    ##          31) CompPrice > 136.5 14  16.750 No ( 0.71429 0.28571 ) *

Now let's make our methodology more robust by including a training and a test set (split: 250,150), grow the tree on the training set, and evaluate its performance on the test set, as per the norm.

``` r
set.seed(1000)
# take a random sample of 250 for training set
train=sample(1:nrow(Carseats),250)
# estimate the model on the training subset
tree.carseats=tree(High~CompPrice+Income+Advertising+Population+Price,Carseats,subset=train)
# plot the tree
plot(tree.carseats);text(tree.carseats,pretty=0)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-75-1.png)

``` r
tree.pred=predict(tree.carseats,Carseats[-train,],type="class")
with(Carseats[-train,],table(tree.pred,High))
```

    ##          High
    ## tree.pred No Yes
    ##       No  67  28
    ##       Yes 22  33

The tree was grown to full depth, and might be too variable. We now use Cross-Validation to prune it.

``` r
cv.carseats=cv.tree(tree.carseats,FUN = prune.misclass)
cv.carseats
```

    ## $size
    ## [1] 22 16 11  9  4  3  2  1
    ## 
    ## $dev
    ## [1]  88  85  84  75  72  90 104 112
    ## 
    ## $k
    ## [1] -Inf  0.0  1.0  2.0  2.4 10.0 14.0 20.0
    ## 
    ## $method
    ## [1] "misclass"
    ## 
    ## attr(,"class")
    ## [1] "prune"         "tree.sequence"

``` r
plot(cv.carseats)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-78-1.png)

``` r
prune.carseats=prune.misclass(tree.carseats,best=13)
plot(prune.carseats);text(prune.carseats,pretty=0)
```

![](pauls_stats_published_files/figure-markdown_github/unnamed-chunk-79-1.png)

Finally, we can evaluate this pruned tree on the test data.

``` r
tree.pred=predict(prune.carseats, Carseats[-train,],type="class")
with(Carseats[-train,],table(tree.pred,High))
```

    ##          High
    ## tree.pred No Yes
    ##       No  67  28
    ##       Yes 22  33

<https://www.youtube.com/watch?v=IY7oWGXb77o&list=PL5-da3qGB5IB23TLuA8ZgVGC8hV8ZAdGh&index=7>
