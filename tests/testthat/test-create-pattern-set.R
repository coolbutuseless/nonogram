context("test-create-pattern-set")

test_that("create-pattern-set corner cases", {

  res <- create_pattern_set(clue = 10, total_length = 10)
  expect_identical(res, matrix(1L, ncol = 10))

  res <- create_pattern_set(clue = 0, total_length = 10)
  expect_identical(res, matrix(0L, ncol = 10))

  expect_error(create_pattern_set(clue = c(1, 0, 1), total_length = 10), 'Bad clue')
})
