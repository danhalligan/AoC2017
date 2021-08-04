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

# offsets <- c(0, 3, 0, 1, -3)
day5p1 <- function(file) {
  offsets <- scan(file, what = integer())
  execute(offsets)
}

day5p2 <- function(file) {
  offsets <- scan(file, what = integer())
  execute(offsets, function(x) x + if (x >= 3) -1 else 1)
}
