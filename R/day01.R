shift <- function(v, s) {
  c(tail(v, s), head(v, -s))
}

#' @export
read_input.day1 <- function(x, file = x$file) {
  readLines(file)[[1]] |>
    strsplit('') |>
    unlist() |>
    as.numeric()
}

#' @export
part1.day1 <- function(x) {
  v <- input(x)
  sum(v[v == shift(v, 1)])
}

#' @export
part2.day1 <- function(x) {
  v <- input(x)
  sum(v[v == shift(v, length(v)/2)])
}
