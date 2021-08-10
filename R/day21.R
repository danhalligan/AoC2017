
matrix_options <- function(x) {
  y <- flip(x)
  list(
    x, rotate(x), rotate(x, 2), rotate(x, 3),
    y, rotate(y), rotate(y, 2), rotate(y, 3)
  )
}

#' @importFrom purrr map_lgl
upscale <- function(x, lookup) {
  lookup$to[[which(map_lgl(lookup$from, ~identical(.x, x)))]]
}

# memoising helps here, but this solution is still pretty slow in R
#' @importFrom memoise memoise
upscaler <- memoise(upscale)

upscale_set <- function(x, lookup) {
  sub <- if (nrow(x) %% 2 == 0) 2 else 3
  size <- nrow(x)
  nblocks <- size / sub

  new <- matrix(FALSE, nrow = size + nblocks, ncol = size + nblocks)
  grids <- map(seq_len(nblocks) - 1, ~ 1:sub + sub * .x)
  ngrids <- map(seq_len(nblocks) - 1, ~ 1:(sub+1) + (sub+1) * .x)

  for (i in seq_len(nblocks)) {
    for (j in seq_len(nblocks)) {
      new[ngrids[[i]], ngrids[[j]]] <- upscaler(x[grids[[i]], grids[[j]]], lookup)
    }
  }
  new
}

as_picture <- function(x) {
  x |> str_split("\\/") |>
    map(function(x) {
      res <- str_split(x, "") |> do.call(what = rbind)
      res == "#"
    })
}

#' @importFrom tidyr unnest
#' @importFrom dplyr distinct
#' @export
read_input.day21 <- function(x, file = x$file) {
  x <- read_lines(file) |>
    str_split(" => ") |>
    map(~as_picture(.x))
  x <- tibble(
      from = map(x, ~ .x[[1]]),
      to = map(x, ~ .x[[2]])
    )
  x$tsize <- unlist(map(x$to, ~nrow(.x)))
  x$fsize <- unlist(map(x$from, ~nrow(.x)))
  x$from <-  map(x$from, ~matrix_options(.x))
  unnest(x, from) |> distinct()
}

picture <- function() {
  matrix(c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    ncol = 3, byrow = TRUE)
}

#' @export
part1.day21 <- function(x, reps = 5, ...) {
  lookup <- input(x)
  x <- picture()
  for (i in seq_len(reps)) x <- upscale_set(x, lookup)
  sum(x)
}

#' @export
part2.day21 <- function(x, reps = 18, ...) {
  lookup <- input(x)
  x <- picture()
  for (i in seq_len(reps)) x <- upscale_set(x, lookup)
  sum(x)
}
