context("analyze_ag_trials")


test_that("output returns correct means/confidence intervals", {

  testData <- readr::read_csv("testdata.csv")

  manualtestData <-testData %>%
    dplyr::group_by(block_type_number, name) %>%
    dplyr::summarize(
      groupMean = mean(ton.hectare, na.rm = TRUE),
      groupVariance = var(ton.hectare, na.rm = TRUE),
      groupCount = n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(groupMean)) %>%
    dplyr::mutate(groupMean = round(groupMean, 2)) %>%
    dplyr::select(Trial = block_type_number, TrialName = name,
                  TrialResult = groupMean)

  analysisData <- analyze_ag_trials(testData, block_type_number, ton.hectare,
                                    name, profitVariable = NULL, block_number)

  analysisData1 <- analysisData %>%
    dplyr::select(Trial, TrialName = `Trial 1`, TrialResult = `Trial 1 Outcome`)
  analysisData2 <- analysisData %>%
    dplyr::select(Trial, TrialName = `Trial 2`, TrialResult = `Trial 2 Outcome`)
  analysisData3 <- rbind(analysisData1, analysisData2)
  analysisData3 <- analysisData3 %>%
    dplyr::distinct() %>%
    dplyr::mutate(TrialResult = as.numeric(as.character(
      gsub(".[(].*", "\\1",TrialResult))))

  expect_equal(manualtestData, analysisData3)



})




test_that("output returns correct p-values from Tukey's test", {

  testData <- readr::read_csv("testdata.csv")

  testData <- dplyr::filter(testData, block_type_number == 1)

  testAOV <- aov(ton.hectare ~ name + as.factor(block_number),
                 data = testData)
  testTukey <- as.data.frame(TukeyHSD(testAOV, "name")$name)

  testTukey$`P-Value` = ifelse(testTukey$`p adj` < .01,
                                      "< .01",
                                      round(testTukey$`p adj`, 2))

  testTukey <- dplyr::select(testTukey, `P-Value`)

  rownames(testTukey) <- c()

  analysisData <- analyze_ag_trials(testData, block_type_number, ton.hectare,
                                    name, profitVariable = NULL, block_number)

  analysisData <- analysisData %>%
    dplyr::select(`P-Value`)

  rownames(analysisData) <- c()

  expect_equal(testTukey, analysisData)


})
