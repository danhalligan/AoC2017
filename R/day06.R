spread_blocks <- function(blocks) {
  m <- which.max(blocks)
  b <- blocks[m]
  blocks[m] <- 0
  while (b > 0) {
    m <- m %% length(blocks) + 1
    blocks[m] <- blocks[m] + 1
    b <- b - 1
  }
  blocks
}

hash <- function(x) paste(x, collapse = ",")

redistribute <- function(blocks) {
  seen <- list()
  cycles <- 0
  str <- hash(blocks)
  while (is.null(seen[[str]])) {
    seen[[str]] <- cycles
    blocks <- spread_blocks(blocks)
    str <- hash(blocks)
    cycles <- cycles + 1
  }
  list(cycles = cycles, seen = seen, final = str)
}

#' @export
part1.day6 <- function(x) {
  redistribute(input(x))$cycles
}

#' @export
part2.day6 <- function(x) {
  res <- redistribute(input(x))
  res$cycles - res$seen[[res$final]]
}
