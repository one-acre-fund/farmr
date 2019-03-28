#' Create table with pairwise means and confidence intervals
#'
#' This function takes a phase 1/phase 2 dataset, dependent variable, and
#' test variable and outputs a table showing the pairwise comparison of means
#' and confidence intervals for all possible combinations. It is designed
#' to match up with the Tukey's HSD output.
#'
#' @param depVar The name of the dependent variable from the dataset
#' (e.g. yield, profit)
#' @param testVar The name of the variable that you are testing for
#' significant differences
#' @param dataset The full dataset
#'
#' @return Returns a dataframe with the pairwise means and confidence intervals
#' for all trials.


pairwise_stats <- function(mat){

  output_table <- do.call(rbind, lapply(1:(nrow(mat)-1), function(z) {

    cbind(cbind(rep(mat[z,], nrow(mat)-z)), mat[,1][(1+z):nrow(mat)])

  }))

  return(output_table)

}

dataset <- readr::read_csv("tests/testthat/testdata.csv") %>% as.data.frame()
depVar = "ton.hectare"
testVar = "trial_number"


create_pairwise_means <- function(dep_var_list, testVar, dataset) {

  #these are quoted for development.
  # depVar = rlang::enquo(depVar) # depVar
  # testVar = rlang::enquo(testVar) # testVar

  agg_vars <- lapply(dep_var_list, function(dep_var){

    agg_tab <- as.matrix(aggregate(dataset[,dep_var], by = list(testVar), function(x){
      c(mean = mean(x, na.rm = T),
        ci = norm.interval(x))
    }))

    agg_tab <- as.data.frame(agg_tab)
    names(agg_tab) <- c("group", paste0(dep_var, ".mean"), paste0(dep_var, ".ci"))
    return(agg_tab)
  })

  # js, this is me trying to give us the flexibility to accept multiple variables for aggregation.
  # The next steps will be to assemble the list and label it in a flexible manner.


  # depVar = dataset[ ,depVar] # <<< can eventually delete this....
  # testVar = dataset [,testVar]
  #
  # means = aggregate(depVar,
  #                   by = list(testVar),
  #                   FUN=mean, na.rm=T)
  #
  # ci = aggregate(depVar,
  #                by = list(testVar),
  #                FUN=norm.interval)

  # take 2 << this needs names!
  tab <- do.call(cbind, lapply(agg_vars, function(x){
    data.frame(
      pairwise_stats(as.matrix(x[, 2])),
      pairwise_stats(as.matrix(x[, 3]))
    )
  }))


  # take 1
  tab <- cbind(
    pairwise_stats(as.matrix(means[,1])), # names
    pairwise_stats(as.matrix(means[,2])), # means
    pairwise_stats(as.matrix(ci[,2])) # ci
  )


  # remove all non-finite values
  finalTab <- tab[ tab[,3] != "NaN" & tab[,4] != "NaN", ] # this needs to be updated.


  # fixing behaviour of one-row test
  if (class(finalTab) == "character") {
    finalTab <- t(finalTab)
  }

  finalDf <- as.data.frame(finalTab, stringsAsFactors = FALSE)

  # note that names are in reverse order
  col <- ncol(finalDf)/3
  names(finalDf) <- c("Trial 2", "Trial 1", paste0("M", 1:col), paste0("C", 1:col))

  # these were converted to characters due to NAs, recasting as integers
  finalDf$M1 <- as.numeric(as.character(finalDf$M1))
  finalDf$M2 <- as.numeric(as.character(finalDf$M2))

  # reformatting the trial outcomes to only one column
  finalDf$`Trial 2 Outcome` <- paste(round(finalDf$M1, 2), finalDf$C1)
  finalDf$`Trial 1 Outcome` <- paste(round(finalDf$M2, 2), finalDf$C2)

  # adding in percent yield change
  finalDf$`Percent Change` <- paste(round((((
    finalDf$M2 - finalDf$M1)/finalDf$M1)*100),1), "%", sep="")

  finalDf <- dplyr::select(finalDf, `Trial 1`, `Trial 2`, `Trial 1 Outcome`,
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

