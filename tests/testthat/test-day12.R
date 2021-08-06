test_that("day 12 part 1 works", {
  day(12, file = "inputs/day12.txt") |> part1() |> expect_equal(6)
})

test_that("day 12 part 2 works", {
  day(12, file = "inputs/day12.txt") |> part2() |> expect_equal(2)
})
