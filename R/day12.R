#' @importFrom readr read_delim
#' @importFrom tidyr separate_rows
#' @export
read_input.day12 <- function(x, file = x$file) {
  read_delim(file,
      delim = " <-> ",
      col_names = c("from", "to"),
      col_types = c("c", "c"),
    ) |>
    separate_rows("to", sep = ", ")
}

# list of vectors of connected elements
connected <- function(x) {
  conn <- list()
  i <- 1
  while (nrow(x)) {
    conn[[i]] <- x[[1, 1]]
    while (nrow(x)) {
      m <- x[[1]] %in% conn[[i]] | x[[2]] %in% conn[[i]]
      if (!any(m)) break
      conn[[i]] <- c(conn[[i]], flatten_chr(x[m, ])) |> unique()
      x <- x[!m, ]
    }
    i <- i + 1
  }
  conn
}

#' @importFrom purrr flatten_chr keep
#' @export
part1.day12 <- function(x, ...) {
  input(x) |>
    connected() |>
    keep(~ "0" %in% .x) |>
    flatten_chr() |>
    length()
}

#' @export
part2.day12 <- function(x, ...) {
  input(x) |>
    connected() |>
    length()
}
