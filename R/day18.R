#' @importFrom readr read_delim
#' @export
read_input.day18 <- function(x, file = x$file) {
  read.delim(file, sep = " ", header = FALSE,
    col.names = c("c", "a", "b"))
}

#' @importFrom stringr str_detect
as_val <- function(x, reg) {
  if (str_detect(x, "\\d+")) as.numeric(x) else reg[[x]]
}

solo <- function(p, start = 0) {
  reg <- set_names(rep(0, 26), letters)
  reg[["p"]] <- start
  i <- 1
  sounds <- numeric()
  rec_reg <- NULL
  function(rec = 0) {
    while (i <= nrow(p)) {
      code <- p[["c"]][i]
      a <- p[["a"]][i]
      b <- p[["b"]][i]
      if (code == "set") {
        reg[[a]] <<- as_val(b, reg)
      } else if (code == "add") {
        reg[[a]] <<- reg[[a]] + as_val(b, reg)
      } else if (code == "mul") {
        reg[[a]] <<- reg[[a]] * as_val(b, reg)
      } else if (code == "mod") {
        reg[[a]] <<- reg[[a]] %% as_val(b, reg)
      } else if (code == "snd") {
        sounds <<- c(sounds, reg[[a]])
      } else if (code == "rcv") {
        if (is.null(rec_reg)) {
          rec_reg <<- a
          save <- sounds
          sounds <<- numeric()
          return(save)
        } else {
          reg[[rec_reg]] <<- rec
          rec_reg <<- NULL
        }
      } else if (code == "jgz") {
        if (str_detect(a, "\\d+")) a <- as.numeric(a)
        if (reg[[a]] > 0) i <<- i + as_val(b, reg) - 1
      }
      i <<- i + 1
    }
  }
}

#' @export
part1.day18 <- function(x, ...) {
  solo(input(x))() |> tail(n = 1)
}

#' @export
part2.day18 <- function(x, ...) {
  a <- solo(input(x), 0)
  b <- solo(input(x), 1)
  va <- a()
  vb <- b()
  count <- length(vb)
  while (length(va) || length(vb)) {
    while (length(va)) vb <- c(vb, b(shift(va)))
    count <- count + length(vb)
    while (length(vb)) va <- c(va, a(shift(vb)))
  }
  count
}
