#' @importFrom stringr str_remove_all
#' @export
read_input.day7 <- function(x, file = x$file) {
  readr::read_lines(file) |>
    str_split(" ") |>
    map(~ str_remove_all(.x, "[^\\w]"))
}

# build a reverse map to lookup root
rmap <- function(x) {
  map <- list()
  for (row in x) {
    for (v in row[-c(1:3)]) map[[v]] <- row[1]
  }
  map
}

fmap <- function(x) {
  fmap <- list()
  for (row in x) {
    val <- as.integer(row[2])
    fmap[[row[1]]] <- list(value = val, children = row[-c(1:3)])
  }
  fmap
}

#' @importFrom purrr set_names
node_sum <- function(fmap, x) {
  fmap[[x]]$children |>
    map_dbl(~ fmap[[.x]]$value + sum(node_sum(fmap, .x))) |>
    set_names(fmap[[x]]$children)
}

unbalanced <- function(fmap, x) {
  sums <- node_sum(fmap, x)
  any(sums[1] != sums)
}

singleton <- function(x) {
  x[!(duplicated(x) | duplicated(x, fromLast = TRUE))]
}

# Root is found by picking a random start and iteratively finding parent

#' @export
part1.day7 <- function(x) {
  rmap <- rmap(input(x))
  v <- rmap[[1]]
  while (!is.null(rmap[[v]])) v <- rmap[[v]]
  v
}

# Here we start at the root and ask if it is unbalanced (children do not
# all sum to the same value). If so we repeat for the the node that is the odd
# one out. If the children of the odd one out are balaned, then we need
# to adjust the weight of this node.

#' @export
part2.day7 <- function(x) {
  node <- part1(x)
  fmap <- fmap(input(x))
  while (unbalanced(fmap, node)) {
    node <- names(singleton(node_sum(fmap, node)))[[1]]
  }
  node <- rmap(input(x))[node][[1]]
  vals <- node_sum(fmap, node)
  bad <- singleton(vals)
  target <- vals[which(duplicated(vals))[1]]
  as.numeric(target - sum(node_sum(fmap, names(bad))))
}



