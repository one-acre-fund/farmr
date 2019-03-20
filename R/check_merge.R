#' Flag potential merge errors / mismatches.
#'
#' This function takes a dataset and checks for values by group. This
#' function was primarily designed to check merges against crop/seed and price
#' for p1/p2 trial analysis, but has other use cases as well.
#'
#' @param dataset The full dataset
#' @param xVariable The main dataset's column that the other dataset was merged
#' on (e.g. seed).
#' @param yVariable The column from the other dataset that you want to check
#' the merge on.
#' @param onlyMissingValues Boolean value (default = FALSE) to return only values
#' that are missing from the main dataset.
#'
#' @return Returns a dataframe with the missing values.
#' @export
#' @examples
#'
#'


check_merge <- function(dataset, xVariable, yVariable, onlyMissingValues = FALSE) {

  # evaluating dataset column names
  group <- rlang::enquo(xVariable)
  sum_variable <- rlang::enquo(yVariable)


  return_data <- dataset %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarize(
      "Merged Value" = mean(!!sum_variable)
    )  %>%
    dplyr::mutate(Match = ifelse(!is.na(`Merged Value`), "Yes", "No"))

  if (onlyMissingValues == TRUE) {

    return_data <- return_data %>%
      dplyr::filter(is.na(`Merged Value`))

  }


  return(return_data)

}
