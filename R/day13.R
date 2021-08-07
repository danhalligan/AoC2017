#' @importFrom dplyr mutate
#' @importFrom readr read_delim
#' @importFrom tidyr separate_rows
#' @export
read_input.day13 <- function(x, file = x$file) {
  read_delim(file,
      delim = ": ",
      col_names = c("layer", "range"),
      col_types = c("i", "i")
    ) |>
    mutate(
      size = range*2 - 2,
      severity = layer * range
    )
}

position <- function(x, delay = 0) {
  (x$layer + x$size + delay) %% x$size
}

#' @importFrom purrr flatten_chr keep
#' @export
part1.day13 <- function(x, ...) {
  pos <- position(input(x))
  sum(input(x)$severity[pos == 0])
}

#' @export
part2.day13 <- function(x, ...) {
  delay <- 0
  while (any(position(input(x), delay) == 0)) delay <- delay + 1
  delay
}
