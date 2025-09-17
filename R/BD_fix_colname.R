#' Clean column names by removing special characters and replacing spaces
#'
#' This function standardizes column names by:
#' - Replacing spaces with underscores
#' - Removing parentheses `()`
#' - Removing single quotes `'`
#' - Removing hyphens `-`
#'
#' @param the_data A data frame whose column names need fixing
#'
#' @return A data frame with cleaned column names
#' @examples
#' df <- data.frame("Col (1)" = 1:3, "Name's-Age" = c(20, 25, 30))
#' AH_fix_colname(df)
#'
#' @export
BD_fix_colname <- function(the_data) {
  new_names <- colnames(the_data) |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_replace_all("[()]", "") |>
    stringr::str_replace_all("'", "") |>
    stringr::str_replace_all("-", "")

  colnames(the_data) <- new_names
  the_data
}
