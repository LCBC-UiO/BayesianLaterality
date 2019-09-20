
<!-- README.md is generated from README.Rmd. Please edit that file -->

# asymm

<!-- badges: start -->

<!-- badges: end -->

The goal of asymm is to predict the dominant hemisphere based on speech
lateralization measurements and information about handedness.

## Installation

You can install asymm from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("osorensen/asymm")
```

## Example

Here is an example showing basic usage.

``` r
library(asymm)

# Create example data. 
# Subject 1 is left-handed and has been measured twice.
# Subject 2 is right-handed and has been measured once.
data <- data.frame(ID = c(1, 1, 2),
                   handedness = c("L", "L", "R"),
                   listening = c(-10, 20, 10))

# The function predict_asymm computes the probability of left and right brain dominance.
predict_asymmetry(data)
#> # A tibble: 2 x 3
#>      ID LeftDominance RightDominance
#>   <dbl>         <dbl>          <dbl>
#> 1     1         0.923         0.0769
#> 2     2         0.960         0.0398
```
