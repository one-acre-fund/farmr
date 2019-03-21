#' This gives you a summary of what the joined data will look like following the sepcified join
#'
#' The problem it is aiming to solve is making it easy to see that
#' the join is working the way you expect it to.
#'
#' The benefit of this approach is that it accepts all join statments. In an ideal world
#' we're able to easily see which ids are not matching based on what is provided to the
#' by feature in the command. That will be a future version of this function.
#'
#' @param .datq The data in the dplyr pipe
#' @param join_statement the full dplyr join statement that creates the joined data
#' @export
#' @examples
#' join_report(band_members, left_join(band_members, band_instruments, by = "name"))
#' band_members %>% join_report(., left_join(., band_instruments, by = "name"))


join_report <- function(.data, join_statement){

  original_data <- .data
  joined_data = join_statement

  cat("\n Original data has", nrow(.data), "rows")
  cat("\n Joined data has", nrow(joined_data), "rows")

}
