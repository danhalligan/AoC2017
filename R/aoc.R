#' Solve all problems using input files from a directory
#'
#' @param dir A directory of input files (including day number in name).
#'
#' @importFrom fs dir_exists dir_ls
#' @importFrom crayon green blue
#' @export
solve_all <- function(dir) {
  stopifnot(dir_exists(dir))
  for (file in dir_ls("inputs/")) {
    cli::cli_h2(blue(file))
    solve_day(file)
    cat("\n")
  }
}

#' Solve a day's problem for both parts
#'
#' @param file An input file.
#' @param input A direct input (numeric or string).
#' @param day The day to solve.
#'
#' @importFrom crayon green
#' @export
solve_day <- function(file = NULL, day = NULL, input = NULL) {
  x <- day(day, file = file, input = input)
  cat("Part 1:", green(part1(x)), "\n")
  cat("Part 2:", green(part2(x)), "\n")
}

#' @importFrom stringr str_extract
guess_day <- function(file) {
  as.numeric(str_extract(file, "\\d+"))
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

part1 <- function(x, ...) UseMethod("part1")

part2 <- function(x, ...) UseMethod("part2")

#' @export
part1.default <- function(x, ...) NULL

#' @export
part2.default <- function(x, ...) NULL

day <- function(day = NULL, file = NULL, input = NULL) {
  if (!is.null(file)) stopifnot(file.exists(file))
  if (!is.null(file) && is.null(day)) day <- guess_day(file)
  stopifnot(is.numeric(day) && day >= 1 && day <= 25)
  x <- structure(
    .Data = list(input = input, file = file),
    class = c(paste0("day", day), "day")
  )
  if (!is.null(file) && is.null(input)) x <- set_input(x)
  x
}
