div <- function(r) {
  v <- combn(sort(as.numeric(r)), 2)
  i <- which(v[2,] %% v[1,] == 0)
  v[2, i] / v[1, i]
}

#' @export
read_input.day2 <- function(x) {
  read.table(x$file)
}

#' @export
part1.day2 <- function(x) {
  sum(apply(input(x), 1, max) - apply(input(x), 1, min))
}

#' @export
part2.day2 <- function(x) {
  sum(apply(input(x), 1, div))
}
