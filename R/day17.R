insert <- function(x, pos, val) {
  c(head(x, pos - 1), val, tail(x, -pos + 1))
}

# vector rather than linked list.
spinlock <- function(offset, n = 2017) {
  x <- 0
  pos <- 1
  for (val in seq_len(n)) {
    pos <- wrapn(pos + offset, length(x)) + 1
    x <- insert(x, pos, val)
  }
  x
}

#' @export
part1.day17 <- function(x, ...) {
  res <- spinlock(input(x))
  res[which(res == 2017) + 1]
}

#' @export
part2.day17 <- function(x, ...) {
  day17p2(input(x), 5e7)
}
