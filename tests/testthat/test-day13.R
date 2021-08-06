test_that("day 13 part 1 works", {
  day(13, file = "inputs/day13.txt") |> part1() |> expect_equal(24)
})

test_that("day 13 part 2 works", {
  day(13, file = "inputs/day13.txt") |> part2() |> expect_equal(10)
})
