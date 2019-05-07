#' Analyze Phase 1 and Phase 2 RCBD Agricultural Trials
#'
#' This function is designed to analyze all data from phase 1 and phase 2 trials.
#'
#' This function takes a clean dataset, the dependent variable name, experiment
#' variable name, treatment variable name, block variable name, and any other
#' variable names other than the dependent variable to calculate means and
#' confidence intervals for, e.g. germination  rates / profitability.
#'
#' The function returns the analyzed output, using Tukey's HSD to calculate
#' significance for the dependent variable, in a dataframe.
#'
#' Please note that each experiment must have its own unique code.
#'
#' If you want results separated by AEZ, you must ensure that all trials
#' in different AEZs have different experimental codes.
#'
#' @param dataset The full dataset
#' @param depVariable The name of the dependent variable from the dataset
#' (e.g. yield)
#' @param experimentVariable The name of the variable that designates which
#' experiment you are referring to. This will be used to split the dataset.
#' Please ensure that each experiment has its own code.
#' @param treatmentVariable The name of the treatment variable to test for
#' significant differences, usually seed variety.
#' @param blockVariable The name of the variable that designates which block
#' the plot belonged to.
#' @param ... The names of the other columns that you would like to calculate
#' the means and upper/lower bounds for, e.g. profit, germination rate, etc.
#'
#' @return Returns a dataframe with the pairwise comparison of trials within
#' each experiment.
#' @export
#' @examples
#'
#'
#'

# ref: https://community.rstudio.com/t/quasiquotation-inside-a-formula/14929

# wrapper function that returns the final output
analyze_ag_trials <- function(dataset, depVariable,
                              experimentVariable, treatmentVariable,
                              blockVariable, ... ) {

  # quoting variables
  depVar = rlang::enquo(depVariable)
  experimentVar = rlang::enquo(experimentVariable)
  treatmentVar = rlang::enquo(treatmentVariable)
  blockVar = rlang::enquo(blockVariable)
  additionalTestVars = rlang::enquos(...)

  # checking to ensure that variable names are in the dataset

  for (i in c(rlang::quo_text(depVar),
              rlang::quo_text(experimentVar),
              rlang::quo_text(treatmentVar),
              rlang::quo_text(blockVar))) {

         if(!i %in% names(dataset)) {

           stop(paste("Error!", i, "not found in dataset!"))

         }

  }


  # Ensuring that the experiment/treatment/block variables are in factor form
  # ensuring that the test variable is in factor form
  dataset[[rlang::quo_text(treatmentVar)]] <-
    as.factor(dataset[[rlang::quo_text(treatmentVar)]])

  dataset[[rlang::quo_text(experimentVar)]] <-
    as.factor(dataset[[rlang::quo_text(experimentVar)]])

  dataset[[rlang::quo_text(blockVar)]] <-
    as.factor(dataset[[rlang::quo_text(blockVar)]])

  # filtering out rows with NA in the dependent variable
  dataset <- dplyr::filter(dataset, !is.na(!!depVar))


  # creating regression formula
  completeFormula = paste(rlang::quo_text(depVar), " ~ ",
                          rlang::quo_text(treatmentVar),
                          " + ", rlang::quo_text(blockVar))


  # forcing recognition of trial variable as a column name
  experimentVarColumn <- eval(as.name(rlang::quo_text(experimentVar)), dataset)


  # Send to helper function to analyze the trial
  finalOutput <- do.call(rbind, lapply(split(dataset, experimentVarColumn),

                       function(x) {

                         # running RCBD function
                         tukeyOutput <- create_tukey_output(
                           completeFormula,
                           depVar,
                           treatmentVar,
                           x
                         )

                         # running function to compute means
                         meansOutput <- create_pairwise_means(depVar,
                                                              treatmentVar,
                                                              x)

                         output <- cbind(tukeyOutput, meansOutput)

                         # adding in variable for the trial
                         experiment <- unique(x[[rlang::quo_text(experimentVar)]])
                         output$Experiment <- experiment

                         # cleaning up and selecting required columns
                         outputClean <-
                           dplyr::select(output, `Experiment`,
                                         `Trial 1`, `Trial 2`,
                                         `Trial 1 Outcome`, `Trial 2 Outcome`,
                                         `P-Value`, `Percent Change`)


                         ### Looping through and binding groupvariables
                         if (length(additionalTestVars) > 0) {

                           # collapsing group variables into a single string
                           updatedOutput <- do.call(cbind, purrr::map(additionalTestVars,

                                      function(var) {

                                        addtlVar <- rlang::enquo(var)

                                        additionalOutput <-
                                        create_pairwise_means(addtlVar,
                                                              treatmentVar,
                                                              x)


                                        additionalOutput <- dplyr::select(
                                          additionalOutput,
                                          `Trial 1 Outcome`,
                                          `Trial 2 Outcome`,
                                          `Percent Change`
                                        )

                                        # not a beautiful way to do this, but it works
                                        name1 <-  paste("Trial 1", rlang::quo_text(var), "Outcome")
                                        name2 <- paste("Trial 2", rlang::quo_text(var), "Outcome")
                                        name3 <- paste(rlang::quo_text(var), "Percent Change")

                                        names(additionalOutput)[1] <- name1
                                        names(additionalOutput)[2] <- name2
                                        names(additionalOutput)[3] <- name3

                                        return(additionalOutput)
                                      }

                            ))

                          outputClean <- cbind(outputClean, updatedOutput)

                         }


                         return(outputClean)

                 }

  ))



  return(finalOutput)

}

