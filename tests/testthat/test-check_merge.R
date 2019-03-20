context("check_merge")


# output ----------------------------------------


test_that("output returns correct values", {

  # loading in SR2017 Kenya example dataset...
  testData <- readr::read_csv("sr17KenyaTestData.csv")

  expectedOutput_All <- testData %>%
    dplyr::group_by(seed_variety) %>%
    dplyr::summarize(
      "Merged Value" = mean(seed.price)
    ) %>%
    dplyr::mutate(Match = ifelse(!is.na(`Merged Value`), "Yes", "No"))

  expectedOutput_NAs <- expectedOutput_All %>%
    dplyr::filter(is.na(`Merged Value`))


  expect_equal(check_merge(testData, seed_variety,
                           seed.price),
               expectedOutput_All)

  expect_equal(check_merge(testData, seed_variety,
                           seed.price, onlyMissingValues = TRUE),
               expectedOutput_NAs)



})
