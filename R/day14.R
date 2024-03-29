#' @export
read_input.day14 <- function(x, file = x$file) {
  scan(file, what = character(), quiet = TRUE)
}

#' @importFrom R.utils intToBin
as_binary <- function(x) {
  str_split(x, "")[[1]] |>
    map(~ intToBin(strtoi(.x, base = 16))) |>
    flatten_chr() |>
    sprintf(fmt = "%04s") |>
    paste(collapse = "")
}

disk_grid <- function(x) {
  paste0(x, "-", seq(0, 127)) |>
    map(~ knot_hash(.x)) |>
    map(~ as_binary(.x)) |>
    flatten_chr() |>
    str_split("") |>
    map(~ as.numeric(.x)) |>
    do.call(what = rbind)
}

#' @export
part1.day14 <- function(x, ...) {
  input(x) |> disk_grid() |> sum()
}

# Here we build a tibble of all possible coordinates (fx,fy) with their
# neighbours (tx,ty). We can filter this by those that are connected and use
# our previous function `connected()` to count distinct regions.
# We have to remember to create "connections" for each coordinate with itself
# ("S") as isolated locations count as regions too.
#' @importFrom dplyr mutate transmute filter
#' @importFrom tidyr uncount
#' @importFrom tibble tibble
#' @export
part2.day14 <- function(x, ...) {
  grid <- input(x) |> disk_grid()
  d <- nrow(grid)
  tibble(
      fx = rep(seq(1, d), d * 3),
      fy = rep(seq(1, d), each = d * 3),
      dir = rep(c("S", "D", "R"), d * d),
    ) |>
    mutate(
      tx = case_when(dir == "R" ~ fx + 1L, TRUE ~ fx),
      ty = case_when(dir == "D" ~ fy + 1L, TRUE ~ fy)
    ) |>
    filter(tx <= d, ty <= d) |>
    filter(grid[cbind(fx, fy)] & grid[cbind(tx, ty)]) |>
    transmute(
      c1 = paste(fx, fy, sep = ","),
      c2 = paste(tx, ty, sep = ",")
    ) |>
    connected() |>
    length()
}
