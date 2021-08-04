test_that("day 3 part 1 works", {
  x <- day(3, input = 1) |> part1() |> expect_equal(0)
  x <- day(3, input = 12) |> part1() |> expect_equal(3)
  x <- day(3, input = 23) |> part1() |> expect_equal(2)
  x <- day(3, input = 1024) |> part1() |> expect_equal(31)
})
