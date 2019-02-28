context("is_duplicated")

# input -----------------------------------------

test_that("is_duplicated correctly flags all duplicates regardless of class", {

  duplicated_numbers <- c(1:5, 1:3)
  duplicated_characters <- c("a", "b", "c", "a")

  expect_equal(is_duplicated(duplicated_numbers), c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(is_duplicated(duplicated_characters), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("is_duplicated deals with all_instances = FALSE as expected", {

  duplicated_numbers <- c(1:5, 1:3)
  duplicated_characters <- c("a", "b", "c", "a")

  expect_equal(is_duplicated(duplicated_numbers, all_instances = FALSE), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(is_duplicated(duplicated_characters, all_instances = FALSE), c(FALSE, FALSE, FALSE, TRUE))

})

test_that("is_duplicated flags all instances of values duplicated more than twice", {

  multiple_duplicates <- c(1:3, 1, 1)

  expect_equal(is_duplicated(multiple_duplicates), c(TRUE, FALSE, FALSE, TRUE, TRUE))

})

test_that("is_duplicated only flags the last instance of a value duplicated more than twice when all_instances is FALSE",{

  multiple_duplicates <- c(1:3, 1, 1, 1)

  expect_equal(is_duplicated(multiple_duplicates, all_instances = FALSE), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})
