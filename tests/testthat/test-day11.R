test_that("day 11 part 1 works", {
  day(11, input = c("ne", "ne", "ne")) |> part1() |> expect_equal(3)
  day(11, input = c("ne", "ne", "sw", "sw")) |> part1() |> expect_equal(0)
  day(11, input = c("ne", "ne", "s", "s")) |> part1() |> expect_equal(2)
  day(11, input = c("se", "sw", "se", "sw", "sw")) |> part1() |> expect_equal(3)
})
