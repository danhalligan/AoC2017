#' @importFrom readr read_lines
#' @importFrom stringr str_split
#' @export
read_input.day24 <- function(x, file = x$file) {
  x <- read_lines(file) |> str_split("\\/") |> do.call(what = rbind)
  x <- apply(x, 2, as.numeric)
  colnames(x) <- c("a", "b")
  as_tibble(x)
}

#' @importFrom purrr flatten
find_bridges <- function(dat, end = 0, bridge = c()) {
  if (nrow(dat) == 0) return(list(bridge))
  poss <- end == dat$a | end == dat$b
  if (!any(poss)) return(list(bridge))
  map(which(poss), function(p) {
      ndat <- dat[-p, ]
      link <- as.numeric(dat[p, ])
      nend <- link[-match(end, link)]
      nbridge <- c(bridge, link)
      find_bridges(ndat, nend, nbridge)
    }) |>
    flatten()
}

#' @export
part1.day24 <- function(x, ...) {
  input(x) |> find_bridges() |> map_dbl(sum) |> max()
}

#' @export
part2.day24 <- function(x, ...) {
  b <- input(x) |> find_bridges()
  len <- map_int(b, length) |> max()
  b |> keep(~ length(.x) == len) |> map_dbl(sum) |> max()
}
