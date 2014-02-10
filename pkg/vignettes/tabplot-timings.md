<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Timings with tabplot}
-->



# Timings of Big Data visualization with `tabplot`

We test the speed of `tabplot` package with datasets over $1,00,000,000$ records.
For this purpose we multiply the diamonds dataset from the `ggplot2` package $2,000$ times.
This dataset contains 53940 records and 10 variables.

## Create testdata


```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
data(diamonds)
## add some NA's
is.na(diamonds$price) <- diamonds$cut == "Ideal"
is.na(diamonds$cut) <- (runif(nrow(diamonds)) > 0.8)
```



```r
n <- nrow(diamonds)
N <- 200L * n

## convert to ff format (not enough memory otherwise)
require(ffbase)
diamondsff <- as.ffdf(diamonds)
nrow(diamondsff) <- N

# fill with identical data
for (i in chunk(from = 1, to = N, by = n)) {
    diamondsff[i, ] <- diamonds
}
```


## Prepare data


The preparation step is the most time consuming. Per column, the rank order is determined.

```r
system.time(p <- tablePrepare(diamondsff))
```

```
##    user  system elapsed 
##   18.81    2.95   22.17
```


## Create tableplots
To focus on the processing time of the tableplot function, the `plot` argument is set to `FALSE`.  


```r
system.time(tab <- tableplot(p, plot = FALSE))
```

```
##    user  system elapsed 
##    2.14    1.06    3.20
```


The following tableplots are samples with respectively 100, 1,000 and 10,000 objects per bin.


```r
system.time(tab <- tableplot(p, sample = TRUE, sampleBinSize = 100, plot = FALSE))
```

```
##    user  system elapsed 
##    2.37    1.00    3.37
```



```r
system.time(tab <- tableplot(p, sample = TRUE, sampleBinSize = 1000, plot = FALSE))
```

```
##    user  system elapsed 
##    3.03    0.88    3.94
```



```r
system.time(tab <- tableplot(p, sample = TRUE, sampleBinSize = 10000, plot = FALSE))
```

```
##    user  system elapsed 
##    3.09    1.07    4.17
```


