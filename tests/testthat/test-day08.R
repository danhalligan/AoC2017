test_that("day 8 part 1 works", {
  day(8, file = "inputs/day08.txt") |>
    part1() |>
    expect_equal(1)
})

test_that("day 8 part 2 works", {
  day(8, file = "inputs/day08.txt") |>
    part2() |>
    expect_equal(10)
})
