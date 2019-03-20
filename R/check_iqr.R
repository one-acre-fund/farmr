#' Flag potential outliers using the IQR.
#'
#' This function takes a column of values and flags any values that are above a
#' set standard deviation from the mean. It is designed to help analysts flag
#' erroneous values.
#'
#' @param dataset The full dataset
#' @param column The name of the variable to be checked for outliers.
#' @param .... The names of the group (or groups) of data.
#' For example, trial, block, and/or AEZ. Note: groups must be in order with the
#' parent group (e.g. maize monocrop trial) before the child (e.g. the specific
#' variety).
#'
#' @return A vector of values with either a boolean value if the column is more
#' than 1.5 IQRs above the third quartile and below the first quartile.
#' @export
#'
#' @examples
#' Without groups:
#' dataframe$Outlier <- sd_check(dataframe, yield)
#'
#' With groups:
#' dataframe$Outlier <- iqr_check(dataframe, yield, block, trial, aez)
#'



check_iqr <- function(dataset, column, ...) {

  # initializing new column
  dataset$IQRMark = NA

  # evaluating dataset column namess
  column <- rlang::enquo(column)
  group_vars <- rlang::enquos(...)

  # checking to see if the data is clustered
  if (length(group_vars) == 0) {

    dataset <- dataset %>%
      dplyr::mutate(Q1 = quantile(!!column, probs = c(.25), na.rm = TRUE)) %>%
      dplyr::mutate(Q3 = quantile(!!column, probs = c(.75), na.rm = TRUE)) %>%
      dplyr::mutate(IQRMark = ifelse(
        !!column < (Q1 - (1.5 * (Q3 - Q1))) |
          !!column > (Q3 + (1.5 * (Q3 - Q1))),
        1, 0
      ))

  } else {

    dataset <- dataset %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::mutate(Q1 = quantile(!!column, probs = c(.25), na.rm = TRUE)) %>%
      dplyr::mutate(Q3 = quantile(!!column, probs = c(.75), na.rm = TRUE)) %>%
      dplyr::mutate(IQRMark = ifelse(
        !!column < (Q1 - (1.5 * (Q3 - Q1)))|
          !!column > (Q3 + (1.5 * (Q3 - Q1))),
        1, 0
      ))

  }


  return(dataset$IQRMark)

}




