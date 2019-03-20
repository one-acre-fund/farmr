#' Analyze Phase 1 and Phase 2 RCBD Agricultural Trials
#'
#' This function takes a clean dataset, the dependent variable name, and the
#' names of the stratification variables and returns the analyzed output (using
#' Tukey's HSD) in a dataframe.
#'
#' @param dataset The full dataset
#' @param depVariable The name of the dependent variable from the dataset
#' (e.g. yield, profit)
#' @param trialVariable The name of the variable that designates which trial
#' you are referring to. This will be used to split the dataset.
#' @param testVariable The name of the variable that you are testing for
#' significant differences
#' @param profitVariable The name of the variable that designates farmer profit.
#' If this is not included in your dataset, please set this variable to Null.
#' @param ... The names of the other columns that the trial was stratified by /
#' source of variation to control for (e.g. block, AEZ)
#'
#' @return Returns a dataframe with the pairwise comparison of all trials.
#' @export
#' @examples
#' With profit column:
#' rcbdOutput <- analyze_ag_trials(finalData, block_type_number, ton.hectare,
#' name, profitVariable = profit, block_number)
#'
#' Without profit column:
#' rcbdOutput <- analyze_ag_trials(finalData, block_type_number, ton.hectare,
#' name, profitVariable = NULL, block_number)


# ref: https://community.rstudio.com/t/quasiquotation-inside-a-formula/14929

# wrapper function that returns the final output
analyze_ag_trials <- function(dataset, trialVariable,
                              depVariable, testVariable,
                              profitVariable = NULL, ... ) {

  # quoting variables
  depVar = rlang::enquo(depVariable)
  trialVar = rlang::enquo(trialVariable)
  testVar = rlang::enquo(testVariable)
  groupVars = rlang::enquos(...)
  profitVar = rlang::enquo(profitVariable)

  # ensuring that the test variable is in factor form
  dataset[[rlang::quo_text(testVar)]] <-
    as.factor(dataset[[rlang::quo_text(testVar)]])

  # ensuring that the grouping variables are in factor form in the
  # regression formula
  groupVars <- purrr::map(groupVars, function(var) {
    rlang::expr(as.factor(!!var))
  })

  # filtering any rows with Na in the dependent variable
  dataset <- dplyr::filter(dataset, !is.na(!!depVar))

  # creating regression formula
  if (length(groupVars) > 0) {

    # collapsing group variables into a single string
    groupVars = paste(purrr::map(groupVars, rlang::quo_text),
                      collapse=" + ")

    # creating the formula for the first dependent variable
    completeFormula = paste(rlang::quo_text(depVar), " ~ ",
                            rlang::quo_text(testVar),
                            " + ", groupVars)

  } else {

    stop("Error - no grouping variables found! Did you set profitVariable to NULL?")


  }

  # forcing recognition of trial variable as a column name
  trialVarColumn <- eval(as.name(rlang::quo_text(trialVar)), dataset)

  # Send to helper function to analyze the trial
  finalOutput <- do.call(rbind, lapply(split(dataset, trialVarColumn),

                       function(x) {

                         # running RCBD function
                         tukeyOutput <- create_tukey_output(
                           completeFormula,
                           depVar,
                           testVar,
                           x
                         )

                         # running function to compute means
                         meansOutput <- create_pairwise_means(depVar,
                                                              testVar,
                                                              x)

                         output <- cbind(tukeyOutput, meansOutput)

                         # adding in variable for the trial
                         trial <- unique(x[[rlang::quo_text(trialVar)]])
                         output$Trial <- trial

                         # cleaning up and selecting required columns
                         outputClean <-
                           dplyr::select(output, `Trial`,
                                         `Trial 1`, `Trial 2`,
                                         `Trial 1 Outcome`, `Trial 2 Outcome`,
                                         `P-Value`, `Percent Change`)

                         # weird way to get around the error of
                         # checking if profitVariable is Null
                         if(rlang::quo_text(profitVar) %in% colnames(dataset)) {

                           profitMeansOutput <-
                             create_pairwise_means(profitVar,
                                                    testVar,
                                                    x)

                           profitMeansOutput <- dplyr::select(
                             profitMeansOutput,
                              `Trial 1 Profit Outcome` = `Trial 1 Outcome`,
                              `Trial 2 Profit Outcome` = `Trial 2 Outcome`,
                              `Profit Percent Change` = `Percent Change`,
                           )

                           outputClean <- cbind(outputClean, profitMeansOutput)

                         }


                         return(outputClean)

                         }

  ))


  return(finalOutput)

}

