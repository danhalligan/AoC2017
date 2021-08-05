test_that("day 6 part 1 works", {
  day(6, input = c(0, 2, 7, 0)) |>
    part1() |>
    expect_equal(5)
})

test_that("day 6 part 2 works", {
  day(6, input = c(0, 2, 7, 0)) |>
    part2() |>
    expect_equal(4)
})
