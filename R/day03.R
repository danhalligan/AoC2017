coord <- function(i) {
  i <- i - 1
  j <- round(sqrt(i))
  k <- abs(j^2 - i) - j
  (c(k, -k) + j^2 - i - (j %% 2)) * 0.5 * (-1)^j
}

# get neightbouring coordinates of a position
neighbours <- function(p) {
  g <- expand.grid((p[1]-1):(p[1]+1), (p[2]-1):(p[2]+1))
  apply(g, 1, paste, collapse = ",")[-5]
}

# sum of neighbouring coordinates
nsum <- function(x, p) {
  x[neighbours(coord(p))] |>
    purrr::discard(is.null) |>
    unlist() |>
    sum()
}

# convert coordinate to a string for sparse matrix
pos <- function(p) {
  paste(coord(p), collapse = ",")
}

day3p1 <- function() {
  sum(abs(coord(325489)))
}

day3p2 <- function() {
  x <- list()
  x[pos(1)] <- 1
  i <- 2
  while (x[pos(i-1)] < 325489) {
    x[pos(i)] <- nsum(x, i)
    i <- i +1
  }
  x[pos(i-1)]
}
