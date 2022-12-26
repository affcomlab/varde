
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

``` r
library(varde)

# Extract only type 1 observations (to simplify the example)
ppa_type1 <- ppa[ppa$Type == 1, ]
```

### Simple Generalizability Study

``` r
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
#> # Variance Estimates
#> # A tibble: 3 × 6
#>   component term     estimate lower upper percent
#>   <chr>     <chr>       <dbl> <dbl> <dbl>   <dbl>
#> 1 Rater     Variance    1.05  0.811  1.63   0.330
#> 2 Target    Variance    0.681 0.471  1.26   0.214
#> 3 Residual  Variance    1.45  1.38   1.54   0.457
#> 
#> # Intercept Estimates
#> # A tibble: 108 × 6
#>    component id    term      estimate   lower  upper
#>    <chr>     <chr> <chr>        <dbl>   <dbl>  <dbl>
#>  1 Rater     1     Intercept    0.881  0.454   1.35 
#>  2 Rater     2     Intercept    1.70   1.23    2.14 
#>  3 Rater     3     Intercept    0.571  0.101   1.02 
#>  4 Rater     4     Intercept   -1.19  -1.63   -0.725
#>  5 Rater     5     Intercept    0.488  0.0521  0.963
#>  6 Rater     6     Intercept    1.32   0.856   1.76 
#>  7 Rater     7     Intercept    0.287 -0.221   0.684
#>  8 Rater     8     Intercept   -0.141 -0.616   0.288
#>  9 Rater     9     Intercept   -0.697 -1.18   -0.280
#> 10 Rater     10    Intercept   -1.75  -2.23   -1.32 
#> # … with 98 more rows
```

``` r
# Create river plot of variance percentages
plot(res_1, type = "river")
```

![](man/figures/README-p1a-1.png)<!-- -->

``` r
# Create density plot of variance posteriors
plot(res_1, type = "variances")
```

![](man/figures/README-p1b-1.png)<!-- -->

``` r
# Create jitter plot of random intercepts
plot(res_1, type = "intercepts")
```

![](man/figures/README-p1c-1.png)<!-- -->

### Simple Two-Way ICC for Inter-Rater Reliability

``` r
# Calculate variance components and ICCs
res_2 <- calc_icc(
  .data = ppa_type1, 
  subject = "Target",
  rater = "Rater",
  score = "Score"
)
res_2
#> # A tibble: 9 × 6
#>   term                est lower upper raters error   
#>   <chr>             <dbl> <dbl> <dbl>  <dbl> <chr>   
#> 1 Subject Variance  0.658 0.464 1.26      NA <NA>    
#> 2 Rater Variance    1.06  0.803 1.59      NA <NA>    
#> 3 Residual Variance 1.46  1.38  1.54      NA <NA>    
#> 4 ICC(A,1)          0.217 0.150 0.333      1 Absolute
#> 5 ICC(A,k)          0.955 0.927 0.973     72 Absolute
#> 6 ICC(A,khat)       0.955 0.927 0.973     72 Absolute
#> 7 ICC(C,1)          0.333 0.241 0.465      1 Relative
#> 8 ICC(C,k)          0.974 0.958 0.984     72 Relative
#> 9 ICC(Q,khat)       0.974 0.958 0.984     72 Relative
```

``` r
# Create density plot of all posteriors
plot(res_2, components = "all")
```

![](man/figures/README-p2a-1.png)<!-- -->

``` r
# Create density plot of ICC posteriors
plot(res_2, components = "icc")
```

![](man/figures/README-p2b-1.png)<!-- -->
