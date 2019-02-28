#' Create a boolean flag to indicate the duplicated values in a variable.
#'
#' This function takes a vector or df column and flags whether a value is duplicated
#' in the vector. This simplifies finding all duplicated values in the data.
#'
#' @param input_var The df column or vector to check
#' @param all_instances defaults to TRUE. This flag determines if all duplicates
#' should be flagged or only the last instance of the duplicate.
#' @return A boolean vector of whether the value is duplicated in the vector or not
#' @export
#' @examples
#' is_duplicated(df$id)
#' is_duplicated(c(1:10, 1))


is_duplicated <- function(input_var, all_instances = TRUE){
  if(all_instances){
    return(duplicated(input_var) | duplicated(input_var, fromLast = TRUE))
  } else {
    return(duplicated(input_var))
  }
}
