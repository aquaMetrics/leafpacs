
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LEAFPACS <img src='https://github.com/aquaMetrics/leafpacs/raw/main/inst/extdat/images/leafpacs_logo.png' align="right" height="300" />

<!-- badges: start -->
<!-- badges: end -->

THIS PACKAGE IS A WORK IN PROGRESS - DON’T USE IN PRODUCTION.

The goal of `leafpacs` R package is to calculate river LEAFPACS
classification.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aquaMetrics/leafpacs")
```

## Example

Run classification:

``` r
library(leafpacs)
data <- leafpacs(taxa_data)
data[, c("sample_id","eqr", "class","high", "good", "moderate", "poor", "bad")]
#> # A tibble: 1 × 8
#>   sample_id   eqr class  high  good moderate  poor   bad
#>   <chr>     <dbl> <chr> <dbl> <dbl>    <dbl> <dbl> <dbl>
#> 1 372276    0.344 poor      0   0.4     26.6    70   3.1
```

## Data

Enter taxa and percentage cover categories and/or enter pre-calculated
`rmni`, `rfa_pc` etc values.

Data structure:

1.  Book-keeping variables for instance `locations_id`, `date_taken`
    etc.
2.  Observation variables: `question`, `response`, and `taxon.`
3.  Predictor variables such as `slope`, `dist_from_source` etc.

Example data:

``` r
library(leafpacs)
taxa_data
#> # A tibble: 10 × 11
#>    location_id           sample_id date_taken question response taxon alkalinity
#>    <chr>                 <chr>     <chr>      <chr>    <chr>    <chr>      <dbl>
#>  1 http://environment.d… 372276    1998-08-25 Percent… 1        Clad…       219.
#>  2 http://environment.d… 372276    1998-08-25 Percent… 2        Epil…       219.
#>  3 http://environment.d… 372276    1998-08-25 Percent… 4        Phra…       219.
#>  4 http://environment.d… 372276    1998-08-25 Percent… 2        Rori…       219.
#>  5 http://environment.d… 372276    1998-08-25 Percent… 4        Spar…       219.
#>  6 http://environment.d… 372276    1998-08-25 n_rfg    1        <NA>        219.
#>  7 http://environment.d… 372276    1998-08-25 rfa_pc   0.05     <NA>        219.
#>  8 http://environment.d… 372276    1998-08-25 rmhi     8.35     <NA>        219.
#>  9 http://environment.d… 372276    1998-08-25 rmni     8.15     <NA>        219.
#> 10 http://environment.d… 372276    1998-08-25 rn_a_ta… 1        <NA>        219.
#> # … with 4 more variables: source_altitude <dbl>, dist_from_source <dbl>,
#> #   slope <dbl>, quality_element <chr>
```
