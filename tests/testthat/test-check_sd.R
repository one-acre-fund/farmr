context("check_sd")



# output ----------------------------------------


test_that("output returns correct values for data that is not grouped", {

  df1 <- data.frame(sd_col = c(1,1,1,1,2,2,2,2,3,3,3,3, NA, 21))

  # returning correct flag (> 3 SD above the mean)
  expect_equal(check_sd(df1, sd_col, flag = TRUE),
               c(0,0,0,0,0,0,0,0,0,0,0,0,NA,1))

  df1_sd <- sd(df1$sd_col, na.rm = TRUE)
  df1_mean <- mean(df1$sd_col, na.rm = TRUE)

  df1_check <- sapply(df1$sd_col, function(x)
    abs((x-df1_mean)/df1_sd))

  # returning correct SD from mean
  expect_equal(check_sd(df1, sd_col), df1_check)

})



test_that("output returns correct values for data that is grouped", {

  # loading in SR2017 Kenya example dataset...
  testData <- readr::read_csv("testdata.csv")

  # returning correct SD from mean
  expect_equal(check_sd(testData, ton.hectare,
                        block_type_number, trial_number),
               testData$sd.check)

  expect_equal(check_sd(testData, ton.hectare,
                        block_type_number, trial_number,
                        flag = TRUE, sdNum = 2),
               testData$sd.check.boolSD2)

})






