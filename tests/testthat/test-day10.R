test_that("day 10 part 1 works", {
  day(10, file = "inputs/day10.txt") |>
    part1(size = 5) |>
    expect_equal(12)
})

test_that("day 10 part 2 works", {
  day(10, input = "") |> part2() |>
    expect_equal("a2582a3a0e66e6e86e3812dcb672a272")

  day(10, input = "AoC 2017") |> part2() |>
    expect_equal("33efeb34ea91902bb2f59c9920caa6cd")

  day(10, input = "1,2,3") |> part2() |>
    expect_equal("3efbe78a8d82f29979031a4aa0b16a9d")

  day(10, input = "1,2,4") |> part2() |>
    expect_equal("63960835bcdc130f0b66d7ff4f6a5a8e")
})
