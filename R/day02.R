div <- function(r) {
  v <- combn(sort(as.numeric(r)), 2)
  i <- which(v[2,] %% v[1,] == 0)
  v[2, i] / v[1, i]
}

day2p1 <- function(input) {
  x <- read.table(input)
  sum(apply(x, 1, max) - apply(x, 1, min))
}

day2p2 <- function(input) {
  x <- read.table(input)
  sum(apply(x, 1, div))
}
