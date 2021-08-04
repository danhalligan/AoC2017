day4p1 <- function(input) {
  x <- readLines(input)
  strsplit(x, " ") |>
    purrr::map_lgl(~ !any(duplicated(.x))) |>
    sum()
}

sort_words <- function(x) {
  strsplit(x, "") |>
    map(~ paste(sort(.x), collapse = ""))
}

day4p2 <- function(input) {
  x <- readLines(input)
  strsplit(x, " ") |>
    purrr::map_lgl(~ !any(duplicated(sort_words(.x)))) |>
    sum()
}
