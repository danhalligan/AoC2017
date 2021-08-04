#' Solve a day's problem for both parts
#'
#' @param input An input file.
#' @param day The day to solve.
#'
#' @export
solve_day <- function(file, day = guess_day(file)) {
  stopifnot(is.numeric(day) && day >= 1 && day <= 25)
  cat("Part1:", guess_fn(day, 1)(file), "\n")
  cat("Part2:", guess_fn(day, 2)(file), "\n")
}

guess_day <- function(file) {
  as.numeric(stringr::str_extract(file, "\\d+"))
}

guess_fn <- function(day, part) {
  match.fun(paste0("day", day, "p", part))
}
