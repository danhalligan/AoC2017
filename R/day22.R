# Padding the matrix means we don't have to worry about out of bounds errors
# as long as we pad enough! In practice, 1000 cells is fine.
#' @export
read_input.day22 <- function(x, file = x$file, pad = 1000) {
  x <- scan(file, character(), quiet = TRUE) |>
    str_split("") |>
    do.call(what = rbind)

  grid <- matrix(0, nrow = pad*2+nrow(x), ncol = pad*2+ncol(x))
  for (i in seq_len(ncol(x))) {
    for (j in seq_len(nrow(x))) {
      grid[i+pad, j+pad] <- as.numeric(x[i, j] == "#")
    }
  }
  grid
}

# Codes are 0: up, 1: right, 2: down, 3: left
move <- function(p, dir) {
  list(
    c(p[1] - 1, p[2]), c(p[1], p[2] + 1),
    c(p[1] + 1, p[2]), c(p[1], p[2] - 1)
  )[[dir + 1]]
}

print_grid <- function(grid, pos) {
  for (i in seq_len(nrow(grid))) {
    for (j in seq_len(ncol(grid))) {
      char <- if (grid[i, j]) "#" else "."
      m <- if (pos[1] == i && pos[2] == j) "]" else " "
      m <- if (pos[1] == i && pos[2] == j+1) "[" else m
      cat(char, m, sep = "")
    }
    cat("\n")
  }
  cat("\n")
}

evolve_grid <- function(grid, n, turn, update, infected = 1, print = FALSE) {
  grid[grid == 1] <- infected # change infected status
  mid <- (sqrt(length(grid)) + 1) / 2
  p <- c(mid, mid)
  dir <- 0
  infections <- 0
  for (i in seq_len(n)) {
    dir <- turn(dir, grid[p[1], p[2]])
    grid[p[1], p[2]] <- update(grid[p[1], p[2]])
    if (grid[p[1], p[2]] == infected) infections <- infections + 1
    p <- move(p, dir)
  }
  if (print) print_grid(grid, p)
  infections
}

# Codes are 0: clean, 1: infected
#' @export
part1.day22 <- function(x, n = 10000, ...) {
  input(x) |>
    evolve_grid(
      turn = function(dir, v) (dir + c(-1, 1)[v+1]) %% 4,
      update = function(v) (v + 1) %% 2,
      n = n,
      infected = 1,
      ...
    )
}

# Codes are 0: clean, 1: weakened, 2: infected, 3: flagged
#' @export
part2.day22 <- function(x, n = 10000000, ...) {
  input(x) |>
    evolve_grid(
      turn = function(dir, v) (dir + c(-1, 0, 1, 2)[v + 1]) %% 4,
      update = function(v) (v + 1) %% 4,
      n = n,
      infected = 2,
      ...
    )
}
