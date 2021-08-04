#' Solve a day's problem for both parts
#'
#' @param input An input file.
#' @param day The day to solve.
#'
#' @export
solve_day <- function(file, day = guess_day(file)) {
  stopifnot(is.numeric(day) && day >= 1 && day <= 25)
  x <- day(day, file = file) |> set_input()
  cat("Part1:", part1(x), "\n")
  cat("Part2:", part2(x), "\n")
}

guess_day <- function(file) {
  as.numeric(stringr::str_extract(file, "\\d+"))
}

read_input <- function(x, file) UseMethod("read_input")

# read a single number
#' @export
read_input.default <- function(x, file = x$file) {
  scan(file, what = numeric(), quiet = TRUE)
}

set_input <- function(x, ...) {
  x$input <- read_input(x)
  x
}

input <- function(x) x$input

part1 <- function(x) UseMethod("part1")

part2 <- function(x) UseMethod("part2")

day <- function(day = c(), file = NULL, input = NULL) {
  structure(
    .Data = list(input = input, file = file),
    class = c(paste0("day", day), "day")
  )
}
