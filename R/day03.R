# Return x, y coordinates from a counterclockwise spiral starting at 1
spiral_coord <- function(i) {
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
  x[neighbours(spiral_coord(p))] |>
    purrr::discard(is.null) |>
    unlist() |>
    sum()
}

# convert coordinate to a string for sparse matrix
pos <- function(p) {
  paste(spiral_coord(p), collapse = ",")
}

#' @export
part1.day3 <- function(x, ...) {
  sum(abs(spiral_coord(input(x))))
}

#' @export
part2.day3 <- function(x, ...) {
  l <- list()
  l[pos(1)] <- 1
  i <- 2
  while (l[pos(i-1)] < input(x)) {
    l[pos(i)] <- nsum(l, i)
    i <- i +1
  }
  l[[pos(i-1)]]
}
