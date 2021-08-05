#' @importFrom readr read_lines
#' @importFrom purrr pluck
#' @export
read_input.day11 <- function(x, file = x$file) {
  scan(file, "", sep = ",", quiet = TRUE)
}

#' @importFrom dplyr case_when
hexdist <- function(x) {
  h <- c("n" = 0, "ne" = 1, "se" = 1, "s" = 0, "sw" = -1, "nw" = -1)
  v <- c("n" = 1, "ne" = 0, "se" = -1, "s" = -1, "sw" = 0, "nw" = 1)
  data.frame(h = h[x], v = v[x])
}

absdist <- function(h, v) {
  as.numeric((abs(h) + abs(v) + abs(h+v)) / 2)
}

#' @importFrom purrr reduce
#' @export
part1.day11 <- function(x, size = 256, ...) {
  d <- input(x) |> hexdist() |> colSums()
  absdist(d[1], d[2])
}

#' @export
part2.day11 <- function(x, size = 256, ...) {
  d <- input(x) |> hexdist() |> cumsum()
  max(absdist(d[['h']], d[['v']]))
}
