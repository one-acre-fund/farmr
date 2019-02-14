context("commcare_gps_split")

# input -----------------------------------------

test_that("input can be split into four sections", {
  df1 <- data.frame(gps_col = "19.18 2 18")

  df2 <- data.frame(gps_col = c("0.1723 34.5 10 5",
           "1888 1987 0.32 10"))

  expect_error(commcare_gps_split(df1$gps_col))
  #expect_equal(commcare_gps_split(df2$gps_col))
})

# output ----------------------------------------

test_that("output is df of four columns", {

  df2 <- data.frame(gps_col = c("0.1723 34.5 10 5",
                                "1888 1987 0.32 10"))

  expect_equal(class(commcare_gps_split(df2$gps_col)), "data.frame")
  expect_equal(ncol(commcare_gps_split(df2$gps_col)), 4)

})

