#' Create table with pairwise means and confidence intervals
#'
#' This function takes a phase 1/phase 2 dataset, dependent variable, and
#' test variable and outputs a table showing the pairwise comparison of means
#' and confidence intervals for all possible combinations. It is designed
#' to match up with the Tukey's HSD output.
#'
#' @param depVariable The name of the dependent variable from the dataset
#' (e.g. yield, profit)
#' @param testVariable The name of the variable that you are testing for
#' significant differences
#' @param dataset The full dataset
#'
#' @return Returns a dataframe with the pairwise means and confidence intervals
#' for all trials.


create_pairwise_means <- function(depVar, testVar, dataset) {

  # these are quoted for development.
  #depVar = rlang::enquo(depVar)
  #testVar = rlang::enquo(testVar)

  means = aggregate(dataset[[rlang::quo_text(depVar)]],
                    by = list(dataset[[rlang::quo_text(testVar)]]),
                    FUN=mean, na.rm=T)

  ci = aggregate(dataset[[rlang::quo_text(depVar)]],
                 by = list(dataset[[rlang::quo_text(testVar)]]),
                 FUN=norm.interval)
  ## create a matrix for means
  h <- as.matrix(means[,2])
  names <- as.matrix(means[,1])
  ci_m <- as.matrix(ci[,2])

  tab.a <- do.call(rbind, lapply(1:(nrow(h)-1), function(z) {

    cbind(cbind(rep(h[z,], nrow(h)-z)), h[,1][(1+z):nrow(h)])

  }))

  ## create a matrix for the names
  tab.names <- do.call(rbind, lapply(1:(nrow(names)-1), function(z) {

    cbind(cbind(rep(names[z,], nrow(names)-z)), names[,1][(1+z):nrow(names)])

  }))

  tab.ci <- do.call(rbind, lapply(1:(nrow(ci_m)-1), function(z) {

    cbind(cbind(rep(ci_m[z,], nrow(ci_m)-z)), ci_m[,1][(1+z):nrow(ci_m)])

  }))


  # bind means matrix, and names matrix, together
  tab <- cbind(tab.names, tab.a, tab.ci)

  # remove all non-finite values
  finalTab <- tab[ tab[,3] != "NaN" & tab[,4] != "NaN", ]


  # fixing behaviour of one-row test
  if (class(finalTab) == "character") {
    finalTab <- t(finalTab)
  }

  finalDf <- as.data.frame(finalTab, stringsAsFactors = FALSE)

  # note that names are in reverse order
  names(finalDf) <- c("Trial 2", "Trial 1", "M1", "M2", "C1", "C2")

  # these were converted to characters due to NAs, recasting as integers
  finalDf$M1 <- as.numeric(as.character(finalDf$M1))
  finalDf$M2 <- as.numeric(as.character(finalDf$M2))

  # reformatting the trial outcomes to only one column
  finalDf$`Trial 2 Outcome` <- paste(round(finalDf$M1, 2), finalDf$C1)
  finalDf$`Trial 1 Outcome` <- paste(round(finalDf$M2, 2), finalDf$C2)

  # adding in percent yield change
  finalDf$`Percent Change` <- paste(round((((
    finalDf$M2 - finalDf$M1)/finalDf$M1)*100),1), "%", sep="")

  finalDf <-
    dplyr::select(finalDf, `Trial 1`, `Trial 2`, `Trial 1 Outcome`,
                  `Trial 2 Outcome`, `Percent Change`)

  return(finalDf)

}



# confidence interval function (assuming normal distribution)
# from http://pages.stat.wisc.edu/~yandell/st571/R/append7.pdf (slightly modified)
norm.interval = function(data) {
  variance = var(data, na.rm = TRUE)
  conf.level = 0.95
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance/length(data))
  result <- paste("(",round(xbar - z * sdx,2),", ",
                  round(xbar + z * sdx,2),")", sep="")
  return(result)
}

