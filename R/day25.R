#' @importFrom stringr str_match
#' @importFrom yaml yaml.load
#' @export
read_input.day25 <- function(x, file = x$file) {
  lines <- readr::read_lines(file)
  state <- str_match(lines[1], "state (\\w)")[[2]]
  diag <- str_match(lines[2], "(\\d+) steps")[[2]]
  x <- lines[-c(1:3)] |> yaml.load()
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      x[[i]][[j]] <- extract_match(x[[i]][[j]])
    }
  }
  list(diag = as.double(diag), state = match(state, LETTERS), data = x)
}

extract_match <- function(x) {
  m <- stringr::str_match(x, "(\\w+)\\.$")[, 2]
  list(as.integer(m[1]), ifelse(m[2] == "right", 1, -1), match(m[3], LETTERS))
}

#' @export
part1.day25 <- function(x, ...) {
  tape <- integer(1e6)
  cursor <- 5e5
  state <- input(x)$state
  data <- input(x)$data
  diag <- input(x)$diag
  for (i in seq_len(diag)) {
    v <- data[[state]][[tape[cursor] + 1]]
    tape[cursor] <- v[[1]]
    cursor <- cursor + v[[2]]
    state <- v[[3]]
  }
  sum(tape)
}
