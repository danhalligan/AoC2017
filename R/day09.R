#' @export
read_input.day9 <- function(x, file = x$file) {
  readr::read_lines(file)
}

rm_cancelled <- function(x) {
  gsub("!.", "", x)
}

rm_garbage <- function(x) {
  gsub("<(?>[^>]|(?R))*>", "<>", x, perl = TRUE)
}

#' @importFrom dplyr case_when
#' @export
part1.day9 <- function(x, ...) {
  x <- input(x) |> rm_cancelled() |> rm_garbage()
  x <- str_split(x, "")[[1]]
  s <- cumsum(case_when(x == "{" ~ 1, x == "}" ~ -1, TRUE ~ 0))
  sum(s[x == "{"])
}

#' @importFrom stringr str_length
#' @export
part2.day9 <- function(x, ...) {
  x <- input(x) |> rm_cancelled()
  str_length(x) - str_length(rm_garbage(x))
}
