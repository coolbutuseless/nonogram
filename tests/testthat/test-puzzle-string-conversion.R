context("puzzle-string-conversion")

test_puzzle_string <- "2:2,1:1,1:3:1,1:1,1:2:1,1:1,2:2-2,1:2,1,3:7:1,3:2,1"
test_puzzle <- list(
  row_clues = list(2L, 2:1, c(1L, 1L), 3L, c(1L, 1L), c(1L, 1L), 2L, c(1L, 1L), 1:2, 2L),
  col_clues = list(2:1, c(2L, 1L, 3L), 7L, c(1L, 3L), 2:1)
)



test_that("multiplication works", {

  res <- convert_puzzle_string_to_puzzle(test_puzzle_string)
  expect_identical(res, test_puzzle)

  res <- convert_puzzle_to_puzzle_string(test_puzzle)
  expect_identical(res, test_puzzle_string)

})
