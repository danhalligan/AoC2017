#' @export
read_input.day23 <- function(x, file = x$file) {
  read.delim(file, sep = " ", header = FALSE,
    col.names = c("c", "a", "b"))
}

# Implments the processor as described, but we need to optimise for part 2...
coprocessor <- function(p, start = 0) {
  reg <- set_names(rep(0, 8), letters[1:8])
  i <- 1
  mul_count <- 0
  while (i <= nrow(p)) {
    code <- p[["c"]][i]
    a <- p[["a"]][i]
    b <- p[["b"]][i]
    if (code == "set") {
      reg[[a]] <- as_val(b, reg)
    } else if (code == "sub") {
      reg[[a]] <- reg[[a]] - as_val(b, reg)
    } else if (code == "mul") {
      mul_count <- mul_count + 1
      reg[[a]] <- reg[[a]] * as_val(b, reg)
    } else if (code == "jnz") {
      if (as_val(a, reg) != 0) i <- i + as_val(b, reg) - 1
    }
    i <- i + 1
  }
  mul_count
}

#' @export
part1.day23 <- function(x, ...) {
  coprocessor(input(x))
}

# Translating the assembly style code into loops (and only considering the case
# when a is 1 at the start we get the following. Note that using "repeat"
# loops more directly translates the reverse "jump" statements.

# part2.day23 <- function(x, ...) {
#   b <- 109300
#   c <- 126300
#   h <- 0
#   repeat {
#     f <- 1
#     d <- 2
#     repeat {
#       e <- 2
#       repeat {
#         g <- d
#         g <- g * e - b
#         if (g == 0) f <- 0
#         e <- e + 1
#         g <- e - b
#         if (g == 0) break
#       }
#       d <- d + 1
#       g <- d - b
#       if (g == 0) break
#     }
#     if (f == 0) h <- h + 1
#     g <- b - c
#     if (g == 0) break
#     b <- b + 17
#   }
#   h
# }

# Above, replacing the repeat loops with for loops.
# In each, "g" is used to break the loop once the loop reaches "b"

# part2.day23 <- function(x, ...) {
#   b <- 109300
#   c <- 126300
#   h <- 0
#   repeat {
#     f <- 1
#     d <- 2
#     for (d in 2:b) {
#       for (e in 2:b) {
#         if (d * e == b) f <- 0
#       }
#     }
#     if (f == 0) h <- h + 1
#     if (b == c) break
#     b <- b + 17
#   }
#   h
# }

# From above we can see we're searching for pairs of numbers that multiply to b
# Instead, we can ask if numbers (between 2 and sqrt(b)) are divisible with
# no remainder (using modulo) (if so, then they are a divisor of b)
# part2.day23 <- function(x, ...) {
#   b <- 109300
#   c <- 126300
#   h <- 0
#   repeat {
#     for (d in 2:sqrt(b)) {
#       if (b %% d == 0)  {
#         h <- h + 1
#         break
#       }
#     }
#     if (b == c) break
#     b <- b + 17
#   }
#   h
# }

# Further simplifying the above and naming what we're doing.
prime <- function(x) {
  for (d in 2:sqrt(x)) if (x %% d == 0) return(FALSE)
  TRUE
}

#' @export
part2.day23 <- function(x, ...) {
  map_lgl(seq(109300, 126300, 17), ~ !prime(.x)) |> sum()
}
