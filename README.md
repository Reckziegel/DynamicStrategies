
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DynamicStrategies

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/Reckziegel/DynamicStrategies/branch/main/graph/badge.svg)](https://codecov.io/gh/Reckziegel/DynamicStrategies?branch=main)
[![R-CMD-check](https://github.com/Reckziegel/DynamicStrategies/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/DynamicStrategies/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`DynamicStrategies` is currently on the development stage.

## Installation

Install the development version from github with:

``` r
# install.packages("devtools")
install.packages("DynamicStrategies")
```

## Example

``` r
library(DynamicStrategies)
library(ggplot2)

# Build a Convex Strategy
convex_strat <- simulate_strategy(strategy = "max_utility")
convex_strat
#> # A tibble: 6 x 2
#>   name              value         
#>   <chr>             <list>        
#> 1 Time              <dbl [127]>   
#> 2 Portfolio_Series  <dbl [127]>   
#> 3 Market_Series     <dbl [127]>   
#> 4 Percentage_Series <dbl [127]>   
#> 5 Underlying_Index  <dbl [10,000]>
#> 6 Portfolio_Value   <dbl [10,000]>

# See the main statistics
extract_stats(convex_strat)
#> # A tibble: 6 x 2
#>   stat           value
#>   <fct>          <dbl>
#> 1 PnL        10625.   
#> 2 Volatility  1498.   
#> 3 Skewness       0.419
#> 4 Kurtosis       3.32 
#> 5 VaR         2414.   
#> 6 CVaR        2773.

# See the P&L simulation
autoplot(convex_strat)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
# Stress-Test a parameter
plot_sensivities(strategy = convex_strat, variable = "allocation", from = 0, to = 1, size = 20)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

## References

-   Attilio Meucci (2021). Review of Dynamic Allocation Strategies
    (<https://www.mathworks.com/matlabcentral/fileexchange/28384-review-of-dynamic-allocation-strategies>),
    MATLAB Central File Exchange. Retrieved September 5, 2021.

-   Meucci, Attilio, Review of Dynamic Allocation Strategies: Utility
    Maximization, Option Replication, Insurance, Drawdown Control,
    Convex/Concave Management (July 7, 2010). Available at SSRN:
    <https://www.ssrn.com/abstract=1635982>
