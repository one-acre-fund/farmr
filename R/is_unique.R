#' Identify if a variable has only unique values
#'
#' This function takes a column from a df or vector and tells you if the variable
#' has only unique values
#'
#' @param input_var The df column or vector to check
#' @return A boolean indicator of if it's uniquely identified or not.
#' @export
#' @examples
#' is_unique(df$id)
#' is_unique(c(1,2,6,4,2))


is_unique <- function(input_var){
  return(length(input_var) == length(unique(input_var)))
}
