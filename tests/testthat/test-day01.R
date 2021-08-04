test_that("day 1 part 1 works", {
  day(1, input = c(1, 1, 2, 2)) |> part1() |> expect_equal(3)
  day(1, input = c(1, 1, 1, 1)) |> part1() |> expect_equal(4)
  day(1, input = c(1, 2, 3, 4)) |> part1() |> expect_equal(0)
  day(1, input = c(9, 1, 2, 1, 2, 1, 2, 9)) |> part1() |> expect_equal(9)
})

test_that("day 1 part 2 works", {
  day(1, input = c(1, 2, 1, 2)) |> part2() |> expect_equal(6)
  day(1, input = c(1, 2, 2, 1)) |> part2() |> expect_equal(0)
  day(1, input = c(1, 2, 3, 4, 2, 5)) |> part2() |> expect_equal(4)
  day(1, input = c(1, 2, 3, 1, 2, 3)) |> part2() |> expect_equal(12)
  day(1, input = c(1, 2, 1, 3, 1, 4, 1, 5)) |> part2() |> expect_equal(4)
})
