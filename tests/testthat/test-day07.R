test_that("day 7 part 1 works", {
  day(7, file = "inputs/day07.txt") |>
    part1() |>
    expect_equal("tknk")
})

test_that("day 8 part 2 works", {
  day(7, file = "inputs/day07.txt") |>
    part2() |>
    expect_equal(60)
})
