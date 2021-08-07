# This problem is not that well suited to R (a little slow)
# but is well suited to Rcpp!

# The R functions below have been ported to C++ at src/code.cpp

#' @export
read_input.day15 <- function(x, file = x$file) {
  read.table(file)[[5]]
}

day15p1R <- function(sa, sb, n) {
  count <- 0
  for (i in seq_len(n)) {
      sa <- (sa * 16807) %% 2147483647
      sb <- (sb * 48271) %% 2147483647
      if (sa %% 65536 == sb %% 65536) count <- count + 1
  }
  count
}

day15p2R <- function(sa, sb, n) {
  count <- 0
  for (i in seq_len(n)) {
      sa <- (sa * 16807) %% 2147483647
      sb <- (sb * 48271) %% 2147483647
      while (sa %% 4) sa <- (sa * 16807) %% 2147483647
      while (sb %% 8) sb <- (sb * 48271) %% 2147483647
      if (sa %% 65536 == sb %% 65536) count <- count + 1
  }
  count
}

#' @export
part1.day15 <- function(x, ...) {
  day15p1(input(x)[1], input(x)[2], 4e7)
}

#' @export
part2.day15 <- function(x, ...) {
  day15p2(input(x)[1], input(x)[2], 5e6)
}
