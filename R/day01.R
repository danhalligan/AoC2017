read_day01 <- function(input)  {
  readLines(input)[[1]] |>
    strsplit('') |>
    unlist() |>
    as.numeric()
}

shift <- function(v, s) {
  c(tail(v, s), head(v, -s))
}

day1p1 <- function(input) {
  x <- read_day01(input)
  sum(x[x == shift(x, 1)])
}

day1p2 <- function(input) {
  x <- read_day01(input)
  sum(x[x == shift(x, length(x)/2)])
}
