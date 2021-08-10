#' @importFrom tibble as_tibble
#' @importFrom stringr str_extract_all
#' @export
read_input.day20 <- function(x, file = x$file) {
  x <- readr::read_lines(file) |>
    str_extract_all("-*\\d+") |>
    do.call(what = rbind) |>
    apply(2, as.numeric)

  colnames(x) <- c("p1", "p2", "p3", "v1", "v2", "v3", "a1", "a2", "a3")
  as_tibble(x) |>
    transmute(
      id = seq_len(nrow(x)) - 1,
      p = cbind(p1, p2, p3),
      v = cbind(v1, v2, v3),
      a = cbind(a1, a2, a3)
    )
}

#' @export
part1.day20 <- function(x, iter = 1000, ...) {
  dat <- input(x)
  for (i in seq_len(iter)) {
    dat <- dat |> mutate(v = v + a, p = p + v)
  }
  dat$id[which.min(rowSums(abs(dat$p)))]
}

#' @export
part2.day20 <- function(x, iter = 1000, ...) {
  dat <- input(x)
  for (i in seq_len(iter)) {
    dat <- dat |>
      mutate(v = v + a, p = p + v) |>
      filter(is_unique(p))
  }
  nrow(dat)
}
