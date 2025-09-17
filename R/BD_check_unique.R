#' Check if a column is unique
#'
#' Reports whether a column has unique values (optionally ignoring NAs),
#' along with counts and the set of duplicated values.
#'
#' @param data A data.frame or tibble.
#' @param var  Column to check; unquoted name or a string.
#' @param na.rm If TRUE, ignore NA values when assessing uniqueness.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{variable}: column name as a string
#'   \item \code{n_unique}: number of distinct values (per \code{na.rm})
#'   \item \code{n_rows}: total rows in \code{data}
#'   \item \code{n_non_na}: number of non-NA entries in \code{var}
#'   \item \code{is_unique}: TRUE if all (non-NA if \code{na.rm=TRUE}) values are unique
#'   \item \code{duplicates}: vector of values that appear more than once
#' }
#'
#' @examples
#' BD_check_unique(iris, Species)
#' BD_check_unique(iris, "Species", na.rm = TRUE)
#'
#' @export
BD_check_unique <- function(data, var, na.rm = FALSE) {
  stopifnot(is.data.frame(data))

  var <- rlang::ensym(var)
  col_name <- rlang::as_string(var)
  if (!col_name %in% names(data)) {
    stop(sprintf("Column `%s` not found in `data`.", col_name), call. = FALSE)
  }

  col <- dplyr::pull(data, !!var)
  n_rows   <- nrow(data)
  n_non_na <- sum(!is.na(col))
  n_unique <- dplyr::n_distinct(col, na.rm = na.rm)

  # Determine uniqueness criterion
  is_unique <- if (na.rm) {
    n_unique == n_non_na
  } else {
    n_unique == n_rows
  }

  # Identify duplicate values (respecting na.rm)
  vals <- if (na.rm) col[!is.na(col)] else col
  dup_vals <- unique(vals[duplicated(vals)])

  list(
    variable  = col_name,
    n_unique  = n_unique,
    n_rows    = n_rows,
    n_non_na  = n_non_na,
    is_unique = is_unique,
    duplicates = dup_vals
  )
}
