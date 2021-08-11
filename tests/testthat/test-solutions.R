options(readr.show_progress = FALSE)

default_file <- function(day) {
  file.path("inputs", paste0("day", sprintf("%02d", day), ".txt"))
}

expect_part <- function(day, part, expected,
    input = NULL,
    file = if (is.null(input)) default_file(day) else NULL,
    ...
  ) {
  x <- day(day, file = file, input = input)
  x <- if (part == 1) part1(x, ...) else part2(x, ...)
  expect_equal(x, expected)
}

test_that("day 1 works", {
  expect_part(1, 1, 3, input = c(1, 1, 2, 2))
  expect_part(1, 1, 4, input = c(1, 1, 1, 1))
  expect_part(1, 1, 0, input = c(1, 2, 3, 4))
  expect_part(1, 1, 9,  input = c(9, 1, 2, 1, 2, 1, 2, 9))

  expect_part(1, 2, 6, input = c(1, 2, 1, 2))
  expect_part(1, 2, 0, input = c(1, 2, 2, 1))
  expect_part(1, 2, 4, input = c(1, 2, 3, 4, 2, 5))
  expect_part(1, 2, 12, input = c(1, 2, 3, 1, 2, 3))
  expect_part(1, 2, 4, input = c(1, 2, 1, 3, 1, 4, 1, 5))
})

test_that("day 2 works", {
  expect_part(2, 1, 18, file = "inputs/day02a.txt")
  expect_part(2, 2, 9, file = "inputs/day02b.txt")
})

test_that("day 3 part 1 works", {
  expect_part(3, 1, 0, 1)
  expect_part(3, 1, 3, 12)
  expect_part(3, 1, 2, 23)
  expect_part(3, 1, 31, 1024)
})

test_that("day 5 works", {
  expect_part(5, 1, 5, c(0, 3, 0, 1, -3))
  expect_part(5, 2, 10, c(0, 3, 0, 1, -3))
})

test_that("day 6 works", {
  expect_part(6, 1, 5, c(0, 2, 7, 0))
  expect_part(6, 2, 4, c(0, 2, 7, 0))
})

test_that("day 7 works", {
  expect_part(7, 1, "tknk")
  expect_part(7, 2, 60)
})

test_that("day 8 works", {
  expect_part(8, 1, 1)
  expect_part(8, 2, 10)
})

test_that("day 10 works", {
  expect_part(10, 1, 12, size = 5)

  expect_part(10, 2, "a2582a3a0e66e6e86e3812dcb672a272", "")
  expect_part(10, 2, "33efeb34ea91902bb2f59c9920caa6cd", "AoC 2017")
  expect_part(10, 2, "3efbe78a8d82f29979031a4aa0b16a9d", "1,2,3")
  expect_part(10, 2, "63960835bcdc130f0b66d7ff4f6a5a8e", "1,2,4")
})

test_that("day 11 part 1 works", {
  expect_part(11, 1, 3, c("ne", "ne", "ne"))
  expect_part(11, 1, 0, c("ne", "ne", "sw", "sw"))
  expect_part(11, 1, 2, c("ne", "ne", "s", "s"))
  expect_part(11, 1, 3, c("se", "sw", "se", "sw", "sw"))
})

test_that("day 12 works", {
  expect_part(12, 1, 6)
  expect_part(12, 2, 2)
})

test_that("day 13 works", {
  expect_part(13, 1, 24)
  expect_part(13, 2, 10)
})

test_that("day 14 works", {
  as_binary("a0c2017") |> expect_equal("1010000011000010000000010111")
  expect_part(14, 1, 8108, input = "flqrgnkx")
  expect_part(14, 2, 1242, input = "flqrgnkx")
})

test_that("day 15 works", {
  expect_part(15, 1, 588, input = c(65, 8921))
  expect_part(15, 2, 309, input = c(65, 8921))
})

test_that("day 16 works", {
  expect_part(16, 1, "baedc", size = 5, input = c("s1", "x3/4", "pe/b"))
})

test_that("day 17 works", {
  expect_part(17, 1, 638, input = 3)
})

test_that("day 18 works", {
  expect_part(18, 1, 4)
})

test_that("day 19 works", {
  expect_part(19, 1, "ABCDEF", end = "F")
  expect_part(19, 2, 38, end = "F")
})

test_that("day 21 works", {
  expect_part(21, 1, 12, reps = 2)
})

test_that("day 22 works", {
  expect_part(22, 1, 41, n = 70)
  expect_part(22, 1, 5587, n = 10000)
  expect_part(22, 2, 26, n = 100)
  # expect_part(22, 2, 2511944, n = 10000000) # takes a while!
})

test_that("day 24 works", {
  expect_part(24, 1, 31)
  expect_part(24, 2, 19)
})

test_that("day 25 works", {
  expect_part(25, 1, 3)
})
