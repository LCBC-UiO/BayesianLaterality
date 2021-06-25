
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesianLaterality

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/BayesianLaterality)](https://cran.r-project.org/package=BayesianLaterality)
[![R-CMD-check](https://github.com/LCBC-UiO/BayesianLaterality/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/BayesianLaterality/actions)
<!-- badges: end -->

BayesianLaterality contains a function for predicting latent hemispheric
dominance based on observed laterality using a Bayes classifier
developed by [Sørensen and Westerhausen
(2020)](https://doi.org/10.1080/1357650X.2020.1769124). See also the
[accompanying Shiny
app](https://osorensen.shinyapps.io/BayesianLateralityApp/).

## Installation

You can install BayesianLaterality from
[CRAN](https://cran.r-project.org/web/packages/BayesianLaterality/)
with:

``` r
install.packages("BayesianLaterality")
```

Install the latest development version from
[GitHub](https://github.com/LCBC-UiO/BayesianLaterality) with:

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
