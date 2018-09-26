context("find-integers-with-sum")



test_that("find_integers_with_sum works", {
  res <- do.call(rbind, find_integers_with_sum(5, 3))

  expect_true(ncol(res) == 3)
  expect_true(nrow(res) == 6)
  expect_true(all(rowSums(res) == 5))

})



test_that("find_integers_with_sum works big", {
  res <- do.call(rbind, find_integers_with_sum(17, 7))

  expect_true(ncol(res) == 7)
  expect_true(nrow(res) == 8008)
  expect_true(all(rowSums(res) == 17))

})