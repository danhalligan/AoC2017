#' @export
read_input.day8 <- function(x, file = x$file) {
  readr::read_lines(file)
}

as_val <- function(reg, v) {
  if (stringr::str_detect(v, "\\d+")) as.numeric(v) else reg[[v]]
}

#' @importFrom purrr walk
#' @importFrom stringr str_extract str_replace
compute_register <- function(x) {
  reg <- new.env(parent = baseenv())
  str_extract(input(x), "\\w+") |>
    walk(~ assign(.x, 0, envir = reg))

  run <- function(x) eval(parse(text = x), envir = reg)
  regmax <- function() max(as.numeric(as.list(reg)))

  anymax <- 0
  lines <- str_split(input(x), " if ") |>
    map(~ str_replace(.x, "(\\w+) inc", "\\1 <- \\1 +")) |>
    map(~ str_replace(.x, "(\\w+) dec", "\\1 <- \\1 -")) |>
    walk(~ {
      if (run(.x[2])) run(.x[1])
      anymax <<- max(regmax(), anymax)
    })
  list(final = regmax(), any = anymax)
}

#' @export
part1.day8 <- function(x) {
  compute_register(x)$final
}

#' @export
part2.day8 <- function(x) {
  compute_register(x)$any
}
