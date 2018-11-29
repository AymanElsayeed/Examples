Classification
================

Read CSV
--------

``` r
df <- read.csv("~/Desktop/Examples/ccdefault.csv", header = T)
```

Normalize Data
--------------

If ranges for variables are very different, then it's a good idea to normalize the variables, which puts them in similar ranges. Use custom function for now.

Define function for normalizing data
------------------------------------

``` r
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}
```

### Apply function to data frame (but not index or outcome)

``` r
dfn <- as.data.frame(lapply(df[, 2:24], normalize))
head(dfn)
```

    ##    LIMIT_BAL SEX EDUCATION  MARRIAGE        AGE PAY_0 PAY_2 PAY_3 PAY_4
    ## 1 0.01010101   1 0.3333333 0.3333333 0.05172414   0.4   0.4   0.1   0.1
    ## 2 0.11111111   1 0.3333333 0.6666667 0.08620690   0.1   0.4   0.2   0.2
    ## 3 0.08080808   1 0.3333333 0.6666667 0.22413793   0.2   0.2   0.2   0.2
    ## 4 0.04040404   1 0.3333333 0.3333333 0.27586207   0.2   0.2   0.2   0.2
    ## 5 0.04040404   0 0.3333333 0.3333333 0.62068966   0.1   0.2   0.1   0.2
    ## 6 0.04040404   0 0.1666667 0.6666667 0.27586207   0.2   0.2   0.2   0.2
    ##   PAY_5 PAY_6 BILL_AMT1  BILL_AMT2  BILL_AMT3 BILL_AMT4  BILL_AMT5
    ## 1   0.0   0.0 0.1499817 0.06916432 0.08672289 0.1601378 0.08064809
    ## 2   0.2   0.4 0.1488924 0.06785751 0.08781713 0.1632199 0.08407395
    ## 3   0.2   0.2 0.1723923 0.07953247 0.09378907 0.1736374 0.09547003
    ## 4   0.2   0.2 0.1880999 0.11199497 0.11340745 0.1868092 0.10936287
    ## 5   0.2   0.2 0.1541442 0.07160143 0.10601954 0.1798630 0.09963262
    ## 6   0.2   0.2 0.2035057 0.12038060 0.11797384 0.1784066 0.10010164
    ##   BILL_AMT6    PAY_AMT1     PAY_AMT2     PAY_AMT3    PAY_AMT4    PAY_AMT5
    ## 1 0.2609787 0.000000000 0.0004090820 0.0000000000 0.000000000 0.000000000
    ## 2 0.2634847 0.000000000 0.0005937329 0.0011160216 0.001610306 0.000000000
    ## 3 0.2729278 0.001737733 0.0008905994 0.0011160216 0.001610306 0.002344506
    ## 4 0.2836851 0.002289503 0.0011987467 0.0013392259 0.001771337 0.002506277
    ## 5 0.2756805 0.002289503 0.0217787169 0.0111602161 0.014492754 0.001615365
    ## 6 0.2763668 0.002861879 0.0010776252 0.0007332262 0.001610306 0.002344506
    ##      PAY_AMT6
    ## 1 0.000000000
    ## 2 0.003783107
    ## 3 0.009457767
    ## 4 0.001891553
    ## 5 0.001284365
    ## 6 0.001513243

### Put outcome variable back on and rename

``` r
dfn <- cbind(dfn, df[, 25])
names(dfn)[24] <- "DEFAULT"
```

### Check data

``` r
colnames(dfn)
```

    ##  [1] "LIMIT_BAL" "SEX"       "EDUCATION" "MARRIAGE"  "AGE"      
    ##  [6] "PAY_0"     "PAY_2"     "PAY_3"     "PAY_4"     "PAY_5"    
    ## [11] "PAY_6"     "BILL_AMT1" "BILL_AMT2" "BILL_AMT3" "BILL_AMT4"
    ## [16] "BILL_AMT5" "BILL_AMT6" "PAY_AMT1"  "PAY_AMT2"  "PAY_AMT3" 
    ## [21] "PAY_AMT4"  "PAY_AMT5"  "PAY_AMT6"  "DEFAULT"

``` r
head(dfn)
```

    ##    LIMIT_BAL SEX EDUCATION  MARRIAGE        AGE PAY_0 PAY_2 PAY_3 PAY_4
    ## 1 0.01010101   1 0.3333333 0.3333333 0.05172414   0.4   0.4   0.1   0.1
    ## 2 0.11111111   1 0.3333333 0.6666667 0.08620690   0.1   0.4   0.2   0.2
    ## 3 0.08080808   1 0.3333333 0.6666667 0.22413793   0.2   0.2   0.2   0.2
    ## 4 0.04040404   1 0.3333333 0.3333333 0.27586207   0.2   0.2   0.2   0.2
    ## 5 0.04040404   0 0.3333333 0.3333333 0.62068966   0.1   0.2   0.1   0.2
    ## 6 0.04040404   0 0.1666667 0.6666667 0.27586207   0.2   0.2   0.2   0.2
    ##   PAY_5 PAY_6 BILL_AMT1  BILL_AMT2  BILL_AMT3 BILL_AMT4  BILL_AMT5
    ## 1   0.0   0.0 0.1499817 0.06916432 0.08672289 0.1601378 0.08064809
    ## 2   0.2   0.4 0.1488924 0.06785751 0.08781713 0.1632199 0.08407395
    ## 3   0.2   0.2 0.1723923 0.07953247 0.09378907 0.1736374 0.09547003
    ## 4   0.2   0.2 0.1880999 0.11199497 0.11340745 0.1868092 0.10936287
    ## 5   0.2   0.2 0.1541442 0.07160143 0.10601954 0.1798630 0.09963262
    ## 6   0.2   0.2 0.2035057 0.12038060 0.11797384 0.1784066 0.10010164
    ##   BILL_AMT6    PAY_AMT1     PAY_AMT2     PAY_AMT3    PAY_AMT4    PAY_AMT5
    ## 1 0.2609787 0.000000000 0.0004090820 0.0000000000 0.000000000 0.000000000
    ## 2 0.2634847 0.000000000 0.0005937329 0.0011160216 0.001610306 0.000000000
    ## 3 0.2729278 0.001737733 0.0008905994 0.0011160216 0.001610306 0.002344506
    ## 4 0.2836851 0.002289503 0.0011987467 0.0013392259 0.001771337 0.002506277
    ## 5 0.2756805 0.002289503 0.0217787169 0.0111602161 0.014492754 0.001615365
    ## 6 0.2763668 0.002861879 0.0010776252 0.0007332262 0.001610306 0.002344506
    ##      PAY_AMT6 DEFAULT
    ## 1 0.000000000       1
    ## 2 0.003783107       1
    ## 3 0.009457767       0
    ## 4 0.001891553       0
    ## 5 0.001284365       0
    ## 6 0.001513243       0

Split Data
----------

#### Split data into training set (2/3) and testing set (1/3)

``` r
set.seed(2786)  # Random seed
dfn.split <- sample(2, nrow(dfn), 
                     replace = TRUE,
                     prob = c(2/3, 1/3))
```

Create training and testing datasets without outcome labels. Use just the first 23 variables.

``` r
dfn.train <- dfn[dfn.split == 1, 1:23]
dfn.test  <- dfn[dfn.split == 2, 1:23]
```

Create outcome labels
=====================

``` r
dfn.train.labels <- dfn[dfn.split == 1, 24]
dfn.test.labels  <- dfn[dfn.split == 2, 24]
```

### Build and test Classifier

Build classifier for test data.

k = number of neighbors to compare; odd n avoids ties.

Try with several values of k and check accuracy on

following table.

``` r
dfn.pred <- knn(train = dfn.train,
                test = dfn.test, 
                cl = dfn.train.labels,  # true class
                k = 9)                  # n neighbors
```

### Compare predicted outcome to observed outcome

``` r
table(dfn.pred, dfn.test.labels)
```

    ##         dfn.test.labels
    ## dfn.pred    0    1
    ##        0 7249 1462
    ##        1  485  737

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
