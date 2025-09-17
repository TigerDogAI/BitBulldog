#' Count unique (non-missing) categories
#'
#' Returns the number of unique, non-NA categories in a variable.
#'
#' @param dataset A data frame containing \code{var_name}.
#' @param var_name String. The column to inspect.
#' @param include_missing Logical. If \code{TRUE}, includes NA as a category in the count.
#'
#' @return Integer count of unique categories.
#'
#' @examples
#' df <- data.frame(g = c("A","B","A", NA, "C"))
#' BD_count_unique_categories(df, "g")            # 3
#' BD_count_unique_categories(df, "g", TRUE)      # 4 (includes NA)
#'
#' @export
BD_count_unique_categories <- function(dataset, var_name, include_missing = FALSE) {
  if (!is.data.frame(dataset)) stop("`dataset` must be a data.frame")
  if (!var_name %in% names(dataset)) stop("Variable not found in the dataset: ", var_name)

  x <- dataset[[var_name]]
  if (isTRUE(include_missing)) {
    return(length(unique(x)))
  } else {
    return(length(unique(x[!is.na(x)])))
  }
}
