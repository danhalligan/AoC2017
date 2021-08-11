#' @export
read_input.day22 <- function(x, file = x$file) {
  x <- scan("inputs/day22.txt", character(), quiet = TRUE) |>
    str_split("") |>
    do.call(what = rbind)
  grid <- list()
  for (i in seq_len(ncol(x))) {
    for (j in seq_len(nrow(x))) {
      v <- paste(i, j, sep = ",")
      grid[[v]] <- x[i,j]
    }
  }
  grid
}

move <- function(p, dir) {
  switch(as.character(dir),
    "0" = c(p[1] - 1, p[2]),
    "1" = c(p[1], p[2] + 1),
    "2" = c(p[1] + 1, p[2]),
    "3" = c(p[1], p[2] - 1)
  )
}

print_grid <- function(grid, pos) {
  nom <- do.call(rbind, str_split(names(grid), ","))
  nom <- apply(nom, 2, as.numeric)
  vi <- min(nom[ ,1]):max(nom[ ,1])
  vj <- min(nom[ ,2]):max(nom[ ,2])
  for (i in vi) {
    for (j in vj) {
      v <- paste(i, j, sep = ",")
      char <- grid[[v]] %||% "."
      m <- if (pos[1] == i && pos[2] == j) "]" else " "
      m <- if (pos[1] == i && pos[2] == j+1) "[" else m
      cat(char, m, sep = "")
    }
    cat("\n")
  }
  cat("\n")
}

#' @importFrom purrr %||%
#' @export
part1.day22 <- function(x, ...) {
  grid <- input(x)
  mid <- (sqrt(length(grid)) + 1) / 2
  start <- c(mid, mid)
  dir <- 0
  pos <- start
  i <- 0

  as_v <- function(p) paste(p[1], p[2], sep = ",")
  g <- function(p) grid[[as_v(p)]] %||% "."
  setg <- function(p, value) grid[[as_v(p)]] <<- value

  infections <- 0
  while (i < 70) {
    dir <- (if (g(pos) == "#") dir + 1 else dir - 1) %% 4
    if (g(pos) == ".") infections <- infections + 1
    new <- if (g(pos) == "#") "." else "#"
    setg(pos, new)
    pos <- move(pos, dir)
    i <- i + 1
  }
  print_grid(grid, pos)
  print(infections)
}

# #' @export
part2.day22 <- function(x, ...) {
  grid <- input(x)
  mid <- (sqrt(length(grid)) + 1) / 2
  start <- c(mid, mid)
  dir <- 0
  pos <- start
  i <- 0

  as_v <- function(p) paste(p[1], p[2], sep = ",")
  g <- function(p) grid[[as_v(p)]] %||% "."
  setg <- function(p, value) grid[[as_v(p)]] <<- value
  update <- function(v) {
    switch(v, "." = "W", "W" = "#", "#" = "F", "F" = ".")
  }
  turn <- function(dir, v) {
    (dir + switch(v, "." = -1, "W" = 0, "#" = 1, "F" = 2)) %% 4
  }
  infections <- 0
  while (i < 1000) {
    dir <- turn(dir, g(pos))
    if (g(pos) == "W") infections <- infections + 1
    setg(pos, update(g(pos)))
    pos <- move(pos, dir)
    i <- i + 1
  # print_grid(grid, pos)
  }
  print(infections)

}
