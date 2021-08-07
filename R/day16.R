# This problem is not that well suited to R (a little slow)
# but is well suited to Rcpp!

# The R functions below have been ported to C++ at src/code.cpp

#' @export
read_input.day16 <- function(x, file = x$file) {
  scan(file, sep = ",")
}

#' @export
part1.day16 <- function(x, ...) {
}

#' @export
part2.day16 <- function(x, ...) {
}
