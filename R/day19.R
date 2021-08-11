#' @importFrom readr read_lines
#' @importFrom stringr str_split
#' @export
read_input.day19 <- function(x, file = x$file) {
  x <- read_lines(file) |> str_split("")
  m <- max(map_int(x, length))
  for (i in seq_along(x)) x[[i]][is.na(x[[i]][1:m])] <- " "
  do.call(rbind, x)
}

follow_path <- function(path, end = "T") {
  m <- function(p) tryCatch(path[p[1], p[2]], error = function(e) " ")
  r <- function(p) c(p[1], p[2] + 1)
  l <- function(p) c(p[1], p[2] - 1)
  u <- function(p) c(p[1] - 1, p[2])
  d <- function(p) c(p[1] + 1, p[2])

  move_dir <- function(dir, p) switch(dir, "U" = u, "R" = r, "L" = l, "D" = d)(p)
  is_letter <- function(p) str_detect(p, "[A-Z]")

  p <- c(1, which(path[1, ] == "|"))
  dir <- "D"
  sequence <- c()
  count <- 1
  while (m(p) != end) {
    if (m(p) == "+") {
      if (dir == "D" || dir == "U") {
        dir <- if (m(r(p)) == "-" || is_letter(m(r(p)))) "R" else "L"
      } else {
        dir <- if (m(u(p)) == "|" || is_letter(m(u(p)))) "U" else "D"
      }
    }
    p <- move_dir(dir, p)
    count <- count + 1
    if (is_letter(m(p))) sequence <- c(sequence, m(p))
  }
  list(sequence = paste(sequence, collapse = ""), count = count)
}

#' @export
part1.day19 <- function(x, end = "T", ...) {
  follow_path(input(x), end = end)$sequence
}

#' @export
part2.day19 <- function(x, end = "T", ...) {
  follow_path(input(x), end = end)$count
}
