context("check_iqr")



# output ----------------------------------------


test_that("output returns correct values for data that is not grouped", {

  df1 <- data.frame(col = c(-2, 1,1,1,1,2,2,2,2,3,3,3,3, NA, 21))

  # returning correct flag (> 3 SD above the mean)
  expect_equal(check_iqr(df1, col),
               c(0,0,0,0,0,0,0,0,0,0,0,0,0,NA,1))


})


test_that("output returns correct values for data that is grouped", {

  # loading in SR2017 Kenya example dataset...
  testData <- readr::read_csv("sr17KenyaTestData.csv")

  # returning correct SD from mean
  expect_equal(check_iqr(testData, ton.hectare,
                        block_type_number, trial_number),
               testData$iqr.check)

})

