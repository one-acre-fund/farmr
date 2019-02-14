#' Split GPS data as saved by commcare for use in data.frame
#' @param gps_col the column in the df where the GPS character is saved.
#' @examples
#' @export
#' @example
#' df <- cbind(df, commcare_gps_split(df$gps_col))


commcare_gps_split <- function(gps_col) {

  split_col <- as.data.frame(stringr::str_split_fixed(gps_col, " ", n = 4))

  split_col <- as.data.frame(sapply(split_col[,1:4], function(x) as.numeric(as.character(x))))
  names(split_col) <- c("lat", "lon", "alt", "precision")

  assertthat::assert_that(sum(is.na(split_col)) == 0)

  return(split_col)
}
