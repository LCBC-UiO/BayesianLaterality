
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesianLaterality

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/LCBC-UiO/BayesianLaterality.svg?branch=master)](https://travis-ci.org/LCBC-UiO/BayesianLaterality)
<!-- badges: end -->

BayesianLaterality contains a function for predicting latent hemispheric
dominance based on observed laterality using a Bayes classifier
developed by [Sørensen and Westerhausen
(2020)](https://doi.org/10.31234/osf.io/yvmxc).

## Installation

You can install BayesianLaterality from [GitHub](https://github.com/)
with:

``` r
# install.packages("remotes")
remotes::install_github("LCBC-UiO/BayesianLaterality")
```

## Application Example

``` r
library(BayesianLaterality)
```

The main (and only) function of the package is `predict_dominance()`. To
see the arguments that can be set by the user and a more extended
example, type `?predict_dominance` in the R terminal. Here is a simple
example. The dataset `example_data1` contains three laterality
measurements on three right-handed individuals.

``` r
example_data1
#>   listening handedness
#> 1        20      right
#> 2        23      right
#> 3        14      right
```

We then obtain predicted hemispheric dominance as follows. The ID column
reflects the row in the original dataset.

``` r
predict_dominance(example_data1)
#> No ID column in data, assuming one subject per row.
#> # A tibble: 9 x 4
#>   ID    handedness dominance probability
#>   <chr> <chr>      <chr>           <dbl>
#> 1 1     right      left          0.994  
#> 2 1     right      right         0.00583
#> 3 1     right      none          0      
#> 4 2     right      left          0.996  
#> 5 2     right      right         0.00402
#> 6 2     right      none          0      
#> 7 3     right      left          0.988  
#> 8 3     right      right         0.0122 
#> 9 3     right      none          0
```
