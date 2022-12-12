
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varde

<!-- badges: start -->
<!-- badges: end -->

The goal of varde is to provide functions for decomposing the variance
in multilevel models, e.g., for g studies in generalizability theory or
intraclass correlation analyses in interrater reliability.

## Installation

You can install the development version of varde from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/varde")
```

## Example

In the `ppa` example dataset, 72 human “raters” judged the perceived
physical attractiveness of 36 human “targets” in 6 different conditions
(i.e., stimulus “types”).

### Simple Generalizability Study

``` r
library(varde)

# Extract only type 1 observations (to simplify the example)
ppa_type1 <- ppa[ppa$Type == 1, ]

# Fit a mixed effects model with target and rater effects
fit_1 <- brms::brm(
  formula = Score ~ 1 + (1 | Target) + (1 | Rater),
  data = ppa_type1,
  chains = 4,
  cores = 4,
  init = "random",
  warmup = 5000,
  iter = 10000,
  seed = 2022
)
```

``` r
# Extract variance component estimates
res_1 <- varde(fit_1)
res_1
#> # A tibble: 3 × 6
#>   component variance lower upper percent method
#>   <chr>        <dbl> <dbl> <dbl>   <dbl> <chr> 
#> 1 Rater        1.05  0.811  1.63   0.330 brms  
#> 2 Target       0.681 0.471  1.26   0.214 brms  
#> 3 Residual     1.45  1.38   1.54   0.457 brms
```

``` r
# Create density plot of variance posteriors
plot(res_1, type = "posterior")
```

![](man/figures/README-p1a-1.png)<!-- -->

``` r
# Create river plot of variance percentages
plot(res_1, type = "river")
```

![](man/figures/README-p1b-1.png)<!-- -->

### Simple Two-Way ICC for Inter-Rater Reliability

``` r
# Calculate variance components and ICCs
res_3 <- calc_icc(
  .data = ppa_type1, 
  subject = "Target",
  rater = "Rater",
  score = "Score"
)
res_3
#> # A tibble: 9 × 6
#>   term                est lower upper raters error   
#>   <chr>             <dbl> <dbl> <dbl>  <dbl> <chr>   
#> 1 Subject Variance  0.672 0.471 1.27      NA <NA>    
#> 2 Rater Variance    1.06  0.800 1.59      NA <NA>    
#> 3 Residual Variance 1.46  1.38  1.54      NA <NA>    
#> 4 ICC(A,1)          0.210 0.150 0.335      1 Absolute
#> 5 ICC(A,k)          0.957 0.927 0.973     72 Absolute
#> 6 ICC(A,khat)       0.957 0.927 0.973     72 Absolute
#> 7 ICC(C,1)          0.320 0.244 0.468      1 Relative
#> 8 ICC(C,k)          0.975 0.959 0.984     72 Relative
#> 9 ICC(Q,khat)       0.975 0.959 0.984     72 Relative
```

``` r
# Create density plot of all posteriors
plot(res_3, components = "all")
```

![](man/figures/README-p2-1.png)<!-- -->
