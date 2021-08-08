# This problem is not that well suited to R (a little slow)
# but is well suited to Rcpp!

# The R functions below have been ported to C++ at src/code.cpp

#' @export
read_input.day16 <- function(x, file = x$file) {
  scan(file, character(), sep = ",")
}

move <- function(x) {
  s <- str_sub(x, 1, 1)
  switch(s, "s" = spin, "x" = exchange, "p" = partner)(x)
}

new_move <- function(..., class = c()) {
  structure(.Data = list(...), class = c(class, "move"))
}

apply_move <- function(x, p) UseMethod("apply_move")

spin <- function(x) {
  new_move(n = as.integer(str_sub(x, 2)), class = "spin")
}

#' @export
apply_move.spin <- function(x, p) {
  i <- length(p) - x$n
  c(p[(i+1):length(p)], p[1:i])
}

exchange <- function(x) {
  x <- x |> str_sub(2) |> str_split("/") |> unlist() |> as.numeric()
  new_move(a = x[1] + 1, b = x[2] + 1, class = "exchange")
}

#' @export
apply_move.exchange <- function(x, p) {
  replace(p, c(x$a, x$b), c(p[x$b], p[x$a]))
}

partner <- function(x) {
  x <- x |> str_sub(2) |> str_split("/") |> unlist()
  new_move(a = x[1], b = x[2], class = "partner")
}

#' @export
apply_move.partner <- function(x, p) {
  i <- which(p == x$a)
  j <- which(p == x$b)
  replace(p, c(i, j), c(p[j], p[i]))
}

dance <- function(x, v) {
  dance_moves <- map(input(x), ~ move(.x))
  for (m in dance_moves) v <- apply_move(m, v)
  v
}

#' @export
part1.day16 <- function(x, size = 16, ...) {
  dance(x, letters[seq_len(size)]) |>
    paste(collapse = "")
}

#' @export
part2.day16 <- function(x, size = 16, ...) {
  s <- letters[seq_len(size)]
  cycle <- 1
  res <- list()
  res[[cycle]] <- dance(x, s)
  while (!all(res[[cycle]] == s)) {
    cycle <- cycle + 1
    res[[cycle]] <- dance(x, res[[cycle - 1]])
  }
  paste(res[[(1e9 %% cycle)]], collapse = "")
}
