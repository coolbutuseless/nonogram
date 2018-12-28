context("test-plot")

test_that("plotting runs without error", {

  a <- create_clue_plot(c(1, 2, 3), 10)
  expect_s3_class(a, 'ggplot')

  a <- create_puzzle_plot(puzzle_string_library[[1]])
  expect_s3_class(a, 'ggplot')


  expect_error(create_puzzle_plot(puzzle_string_library[[1]], solution_matrix = 1), "identical.*not TRUE")
})
