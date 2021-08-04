#' @export
read_input.day4 <- function(x) readLines(x$file)

#' @export
part1.day4 <- function(x) {
  strsplit(input(x), " ") |>
    purrr::map_lgl(~ !any(duplicated(.x))) |>
    sum()
}

sort_words <- function(x) {
  strsplit(x, "") |>
    purrr::map(~ paste(sort(.x), collapse = ""))
}

#' @export
part2.day4 <- function(x) {
  strsplit(input(x), " ") |>
    purrr::map_lgl(~ !any(duplicated(sort_words(.x)))) |>
    sum()
}
