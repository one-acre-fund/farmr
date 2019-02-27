#' Flag potential outliers using standard deviation.
#'
#' This function takes a column of values and flags any values that are above a
#' set standard deviation from the mean. It is designed to help analysts flag
#' erroneous values.
#'
#' @param dataset The full dataset
#' @param column The variable (in vector form) to be checked for outliers.
#' @param .... The names of the group (or groups) of data.
#' For example, trial, block, and/or AEZ. Note: groups must be in order with the
#' parent group (e.g. maize monocrop trial) before the child (e.g. the specific variety).
#' @param flag Flag if the value is sdNum aboe the mean (Default = False)
#' @param sdNum The number of standard deviations to check (Default = 3)
#' @export
#'
#' @return A vector of values with either a boolean value if the column is more
#' than `sdNum` SDs above the mean, or the actual number of standard deviations
#' above the mean.
#'
#' @examples
#' Without groups:
#' dataframe$Outlier <- sd_check(dataframe, yield, flag = TRUE)
#' With groups:
#' dataframe$Outlier <- sd_check(dataframe, yield, block, trial, aez, flag = TRUE)
#'
#'

sd_check <- function(dataset, column, ..., flag = FALSE, sdNum = 3) {

  # initializing new column
  dataset$sd.check = 0

  # evaluating dataset column names
  column <- rlang::enquo(column)
  group_vars <- rlang::enquos(...)

  # checking to see if the data is clustered
  if (length(group_vars) == 0) {

    # checking to see if it should return a boolean flag or the actual SD
    # from the mean
      if (flag == TRUE) {

        dataset <- dataset %>%
          dplyr::mutate(sd.check = ifelse(
            abs(
                (!!column - mean(!!column, na.rm = TRUE)) /
                  sd(!!column, na.rm = TRUE)
              ) >= sdNum, 1, 0))

      } else {

        dataset <- dataset %>%
          dplyr::mutate(sd.check = abs(
            (!!column - mean(!!column, na.rm = TRUE)) /
              sd(!!column, na.rm = TRUE))
          )

      }


  # handling clustered data

  } else {

    if (flag == TRUE) {

      dataset <-dataset %>%
        dplyr::group_by(!!!group_vars) %>%
        dplyr::mutate(sd.check = ifelse(abs(
                        (!!column - mean(!!column, na.rm = TRUE)) /
                        sd(!!column, na.rm = TRUE)) >= sdNum, 1, 0))

    } else {

      dataset <-dataset %>%
        dplyr::group_by(!!!group_vars) %>%
        dplyr::mutate(sd.check = abs(
          (!!column - mean(!!column, na.rm = TRUE)) /
            sd(!!column, na.rm = TRUE)))

    }

  }

  return(dataset$sd.check)

}



