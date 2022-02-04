
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LEAFPACS <img src='https://github.com/aquaMetrics/leafpacs/raw/main/inst/extdat/images/leafpacs_logo.png' align="right" height="300" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/aquaMetrics/leafpacs/branch/main/graph/badge.svg)](https://app.codecov.io/gh/aquaMetrics/leafpacs?branch=main)
[![R-CMD-check](https://github.com/aquaMetrics/leafpacs/workflows/R-CMD-check/badge.svg)](https://github.com/aquaMetrics/leafpacs/actions)
<!-- badges: end -->

THIS PACKAGE IS A WORK IN PROGRESS - DON’T USE IN PRODUCTION.

The goal of `leafpacs` R package is to calculate river LEAFPACS
classification.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aquaMetrics/leafpacs", dependencies = TRUE)
```

## Example

Run classification:

``` r
library(leafpacs)
data <- leafpacs(taxa_data)
data[, c("sample_id", "eqr", "class", "high", "good", "moderate", "poor", "bad")]
#> # A tibble: 1 × 8
#>   sample_id   eqr class  high  good moderate  poor   bad
#>   <chr>     <dbl> <chr> <dbl> <dbl>    <dbl> <dbl> <dbl>
#> 1 372276    0.344 poor      0   0.4     26.6    70   3.1
```

## Data

Enter taxa and percentage cover categories and/or enter pre-calculated
`rmni`, `rfa_pc` etc values.

Dataframe structure:

1.  Book-keeping variables for instance `locations_id`, `date_taken`
    etc.
2.  Observation variables: `question`, `response`, and `taxon.`
3.  Predictor variables such as `slope`, `dist_from_source` etc.

The columns can be in any order. You can add as many columns as you like
as you as you provide the required\* columns.

Example data:

Demo data in package, \*required columns required:

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

Download web data including optional extra columns:

``` r
library(hera)
data <- get_data(location_id = 92751)
data
#># A tibble: 6 × 21
#>   location_id            location_descri… sample_id date_taken season quality_element question response taxon latitude #> longitude
#>   <chr>                  <chr>            <chr>     <chr>      <chr>  <chr>           <chr>    <chr>    <chr>    <dbl>     #> <dbl>
#> 1 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… percent… 8        Phra…     52.1   #> -0.0226
#> 2 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… percent… 7        Spar…     52.1   #> -0.0226
#> 3 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… n_rfg    0        NA        52.1   #> -0.0226
#> 4 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… rmhi     8.62     NA        52.1   #> -0.0226
#> 5 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… rmni     8        NA        52.1   #> -0.0226
#> 6 http://environment.da… CAM - 92751      724530    2015-09-08 3      River Macrophy… rn_a_ta… 0        NA        52.1   #> -0.0226
# … with 10 more variables: grid_reference <chr>, alkalinity <chr>, full_result_id <chr>, result.result_id <chr>,
#   result_id <chr>, northing <int>, easting <int>, dist_from_source <chr>, source_altitude <chr>, slope <chr>
```
