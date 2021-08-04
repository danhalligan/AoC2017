test_that("day 5 works", {
  x <- day(5, input = c(0, 3, 0, 1, -3))
  expect_equal(part1(x), 5)
  expect_equal(part2(x), 10)
})
