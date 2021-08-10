#' @export
read_input.day1 <- function(x, file = x$file) {
  readLines(file)[[1]] |>
    strsplit('') |>
    unlist() |>
    as.numeric()
}

#' @export
part1.day1 <- function(x, ...) {
  v <- input(x)
  sum(v[v == rotate(v, 1)])
}

#' @export
part2.day1 <- function(x, ...) {
  v <- input(x)
  sum(v[v == rotate(v, length(v) / 2)])
}
