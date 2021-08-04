execute <- function(offsets, inc = function(x) x + 1) {
  i <- 1
  count <- 0
  while (i >= 1 && i <= length(offsets)) {
    n <- i + offsets[i]
    offsets[i] <- inc(offsets[i])
    i <- n
    count <- count + 1
  }
  count
}

#' @export
part1.day5 <- function(x) {
  execute(input(x))
}

#' @export
part2.day5 <- function(x) {
  execute(input(x), function(x) x + if (x >= 3) -1 else 1)
}
