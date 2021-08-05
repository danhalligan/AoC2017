#' @importFrom readr read_lines
#' @export
read_input.day10 <- function(x, file = x$file) {
  read_lines(file)
}

wrapn <- function(x, n) (x - 1) %% n + 1

sparse_hash <- function(x, size = 256, reps = 1) {
  seq <- seq(0, length.out = size)
  pos <- 1
  skip <- 0
  for (rep in seq_len(reps)) {
    for (inp in x) {
      ind <- wrapn(seq(pos, length.out = inp), size)
      seq[ind] <- rev(seq[ind])
      pos <- wrapn(pos + inp + skip, size)
      skip <- skip + 1
    }
  }
  seq
}

#' @importFrom magrittr extract
#' @export
part1.day10 <- function(x, size = 256, ...) {
  input(x) |>
    str_split(",") |>
    unlist() |>
    as.numeric() |>
    sparse_hash(size) |>
    extract(1:2) |>
    prod()
}

#' @importFrom purrr reduce map_int
#' @export
part2.day10 <- function(x, size = 256, ...) {
  c(utf8ToInt(input(x)), 17, 31, 73, 47, 23) |>
    sparse_hash(size, 64) |>
    split(rep(1:16, each = 16)) |>
    map_int(~ reduce(.x, bitwXor)) |>
    as.hexmode() |>
    paste(collapse = "")
}
