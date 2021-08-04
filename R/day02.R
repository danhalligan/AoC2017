div <- function(r) {
  v <- combn(sort(as.numeric(r)), 2)
  i <- which(v[2,] %% v[1,] == 0)
  v[2, i] / v[1, i]
}

#' @export
#' @importFrom purrr map
#' @importFrom stringr str_split
read_input.day2 <- function(x, file = x$file){
  readr::read_lines(file) |>
    str_split(" ") |>
    map(~ as.numeric(.x))
}

#' @importFrom purrr map_dbl
#' @export
part1.day2 <- function(x) {
  input(x) |> map_dbl(~ diff(range(.x))) |> sum()
}

#' @export
part2.day2 <- function(x) {
  input(x) |> map_dbl(~ div(.x)) |> sum()
}
