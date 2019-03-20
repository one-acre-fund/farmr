#' Create Tukey HSD Output for RCBD Trials
#'
#' This function takes a clean formula, the already quosered dependent/test
#' variables, and full dataset to generate a cleaned-up dataframe of Tukey
#' HSD output.
#'
#' @param completeFormula A complete regression formula to use for the
#' ANOVA test.
#' @param depVariable The name of the dependent variable from the dataset
#' (e.g. yield, profit)
#' @param testVariable The name of the variable that you are testing for
#' significant differences
#' @param dataset The full dataset
#'
#' @return Returns a dataframe with the pairwise comparison of all trials and
#' cleaned-p-values.

create_tukey_output <- function(completeFormula,
                                depVar, testVar, dataset) {

  linearReg <- lm(completeFormula, data = dataset)
  aovTest <- aov(linearReg)

  tukeyTest <- TukeyHSD(aovTest, as.factor(rlang::quo_text(testVar)),
                        conf.level=0.95)

  # forcing recognition of testVar as a variable in the dataset
  testVarColumn <- eval(as.name(rlang::quo_text(testVar)), dataset)

  # coercing output to dataframe format
  dfOutput <- as.data.frame(tukeyTest[[rlang::quo_text(testVar)]])

  # changing row names to an actual DF column
  dfOutput$names <- rownames(dfOutput)

  # removing row names to get rid of strange behaviour with cbind()
  rownames(dfOutput) <- c()

  dfOutput$`P-Value` <- ifelse(dfOutput$`p adj` < .01,
                               "< .01",
                               round(dfOutput$`p adj`, 2))


  return(dfOutput)

}
