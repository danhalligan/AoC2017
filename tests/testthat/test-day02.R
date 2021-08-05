test_that("day 2 part 1 works", {
  x <- day(2, file = "inputs/day02a.txt")
  expect_equal(part1(x), 18)
})

test_that("day 2 part 2 works", {
  x <- day(2, file = "inputs/day02b.txt")
  expect_equal(part2(x), 9)
})
