# Rotate a matrix or vector forward by "by".
rotate <- function(x, by) UseMethod("rotate")

#' @export
rotate.matrix <- function(x, by = 1) {
  if (by == 0) x else rotate(t(apply(x, 2, rev)), by = by - 1)
}

#' @export
rotate.numeric <- function(v, by = 1) {
  c(tail(v, by), head(v, -by))
}

#' @export
rotate.character <- function(v, by = 1) {
  c(tail(v, by), head(v, -by))
}

# flip columns in a matrix
flip <- function(x) t(apply(x, 1, rev))

# Which elements are unique in a vector?
is_unique <- function(x) {
  !(duplicated(x) | duplicated(x, fromLast = TRUE))
}

# Wrap a 1-based at a given length n
wrapn <- function(x, n) (x - 1) %% n + 1

# Insert a value into a vector at a given position
insert <- function(x, pos, val) {
  c(head(x, pos - 1), val, tail(x, -pos + 1))
}

# Shift a vector (remove first element and return the value)
shift <- function(x) {
  assign(as.character(substitute(x)), tail(x, -1), parent.frame())
  head(x, 1)
}
