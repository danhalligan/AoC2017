
<!-- README.md is generated from README.Rmd. Please edit that file -->
# AoC2017

<!-- badges: start -->
[![R-CMD-check](https://github.com/danhalligan/AoC2017/workflows/R-CMD-check/badge.svg)](https://github.com/danhalligan/AoC2017/actions) <!-- badges: end -->

AoC2017 is an R package solver for Advent of Code 2017.

## Installation

You can install the released version of AoC2017 from [GitHub](https://github.com/) with:

``` r
devtools::install_github("danhalligan/AoC2017")
```

## Example

To solve a specific day, you can use `solve_day`, passing in the input for the current day.

``` r
library(AoC2017)
solve_day(day = 1, file = "day1-input.txt")
solve_day(file = "day1-input.txt")
```

If day is not provided it is guessed from the filename.

``` r
solve_day(file = "day1-input.txt")
```

Alternatively, some questions only require a short integer to be provided as input:

``` r
library(AoC2017)
solve_day(day = 3, input = 1024)
#> Part 1: 31 
#> Part 2: 1968
```
