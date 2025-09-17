#' Get duplicate rows by one or more keys
#'
#' Find duplicates based on selected columns. If no columns are selected,
#' duplicates are computed across all columns.
#'
#' @param data A data.frame or tibble.
#' @param ... Columns used as the duplicate keys (tidyselect supported).
#' @param .min_n Minimum frequency to keep (default 2).
#' @param .keep_all If TRUE, return all original rows with a count column.
#'   If FALSE, return one row per key with the count only.
#' @param na.rm If TRUE, drop rows where any key column is NA before counting.
#' @param .count_col Name of the count column to add (default ".n").
#' @param .sort If TRUE, sort descending by count (then keys).
#'
#' @return A tibble with duplicates and a count column.
#' @examples
#' # One key
#' BD_get_duplicates(mtcars, cyl)
#' # Multiple keys
#' BD_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
#' # All columns (exact duplicate rows)
#' BD_get_duplicates(mtcars)
#' @export
BD_get_duplicates <- function(data, ...,
                              .min_n = 2,
                              .keep_all = TRUE,
                              na.rm = FALSE,
                              .count_col = ".n",
                              .sort = TRUE) {
  stopifnot(is.data.frame(data))
  # Resolve selection (supports tidyselect helpers)
  keys_idx <- tidyselect::eval_select(rlang::expr(c(...)), data = data)
  keys <- if (length(keys_idx) == 0) names(data) else names(keys_idx)

  dat <- data
  if (na.rm && length(keys) > 0) {
    dat <- dplyr::filter(dat, dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.x)))
  }

  # Count by key(s)
  counts <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(!!.count_col := dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data[[.count_col]] >= .min_n)

  if (.keep_all) {
    out <- dplyr::left_join(dat, counts, by = keys)
  } else {
    out <- counts
  }

  if (.sort) {
    out <- dplyr::arrange(
      out,
      dplyr::desc(.data[[.count_col]]),
      dplyr::across(dplyr::all_of(keys))
    )
  }

  out
}
