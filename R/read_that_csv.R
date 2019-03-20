
#' Faster CSV Readings
#'
#' @param data Path to CSV file.
#' @return dataframe object.
#'
#' @examples
#' data <- read_that_csv("data.csv")
#' @export



read_that_csv <- function(data) {

  x <- read.csv(data, nrows = 1)

  d <- readr::read_csv(data, na = c("", " ", "NA", "#N/A", "---"))
  names(d) <- names(x)

  return(d)
}
