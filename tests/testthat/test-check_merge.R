context("check_merge")


# output ----------------------------------------


test_that("output returns correct values", {

  # loading in SR2017 Kenya example dataset...
  testData <- readr::read_csv("testdata.csv")

  expectedOutput_All <- testData %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(
      "Merged Value" = mean(seed.price)
    ) %>%
    dplyr::mutate(Match = ifelse(!is.na(`Merged Value`), "Yes", "No"))

  expectedOutput_NAs <- expectedOutput_All %>%
    dplyr::filter(is.na(`Merged Value`))

  expect_equal(check_merge(testData, name,
                           seed.price),
               expectedOutput_All)

  expect_equal(check_merge(testData, name,
                           seed.price, onlyMissingValues = TRUE),
               expectedOutput_NAs)



})
