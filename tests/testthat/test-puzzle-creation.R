context("test-puzzle-creation")

test_that("puzzle-creation works", {

  res <- create_clue_from_pattern(c(0, 1, 0, 0, 0, 1, 1, 1))
  expect_identical(res, c(1L, 3L))


  res <- create_clue_from_pattern(c(0, 0, 0, 0, 0))
  expect_identical(res, 0L)


  mat <- matrix(c(0, 1, 1, 0), nrow = 2)
  puz <- create_puzzle_from_matrix(mat)

  expect_identical(puz$row_clues, c(1L, 1L))
  expect_identical(puz$col_clues, c(1L, 1L))

})
