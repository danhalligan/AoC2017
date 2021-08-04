test_that("day 2 part 1 works", {
  x <- day(2, file = "data/day02a.txt") |> set_input()
  expect_equal(part1(x), 18)
})

test_that("day 2 part 2 works", {
  x <- day(2, file = "data/day02b.txt") |> set_input()
  expect_equal(part2(x), 9)
})
