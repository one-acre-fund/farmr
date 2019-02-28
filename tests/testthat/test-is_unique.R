context("is_unique")

# input -----------------------------------------

test_that("is_unique returns true for unique vector", {

  number_sequence <- 1:10
  different_classes <- c("1", 4, "3", 2)

  expect_true(is_unique(number_sequence))
  expect_true(is_unique(different_classes))
})

test_that("is_unique returns false if non-unique vector", {

  non_unique_different_classes <- c("1", 4, "3", 3)

  expect_false(is_unique(non_unique_different_classes))
})
