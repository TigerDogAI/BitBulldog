#' One-row n(%) table for a categorical variable
#'
#' Builds a single, display-ready \code{tibble} row of counts and percentages
#' for each level of a categorical variable, plus a \code{Total} column.
#'
#' @param dataset A data frame containing \code{var_name}.
#' @param var_name String. The column to summarize.
#' @param display_name String. Label placed in the first column header.
#'   Defaults to \code{var_name}.
#' @param digits Integer. Decimal places for percentages (default 1).
#' @param include_missing Logical. If \code{TRUE} (default), NA values are counted
#'   under \code{missing_label}.
#' @param missing_label String used to label missing values (default "Missing").
#' @param sort_levels One of \code{"desc"} (default, by count desc),
#'   \code{"asc"}, \code{"alpha"} (alphabetical), or \code{"levels"} (keep factor order).
#' @param top_n Integer or \code{NULL}. If set, keeps the \code{top_n} most frequent
#'   levels and collapses the rest into an \code{"Other"} column (after sorting).
#' @param other_label String label used when collapsing to "Other" (default "Other").
#'
#' @return A one-row \code{tibble}: first column is \code{display_name},
#'         then one column per level showing \code{"n (p%)"}, ending with \code{Total}.
#'
#' @examples
#' df <- data.frame(g = c("A","B","A", NA, "C","B","B"))
#' BD_1_cat(df, "g", display_name = "Group")
#' BD_1_cat(df, "g", digits = 0, sort_levels = "alpha", top_n = 2)
#'
#' @importFrom dplyr mutate count arrange desc select relocate bind_rows
#' @importFrom dplyr everything
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom rlang .data sym
#' @export
BD_1_cat <- function(dataset,
                     var_name,
                     display_name = var_name,
                     digits = 1,
                     include_missing = TRUE,
                     missing_label = "Missing",
                     sort_levels = c("desc", "asc", "alpha", "levels"),
                     top_n = NULL,
                     other_label = "Other") {

  sort_levels <- match.arg(sort_levels)

  if (!is.data.frame(dataset)) stop("`dataset` must be a data.frame")
  if (!var_name %in% names(dataset)) stop("Variable not found in dataset: ", var_name)

  # Work on a copy of the target column
  v <- dataset[[var_name]]

  # Add missing label if requested
  if (include_missing) {
    v <- ifelse(is.na(v), missing_label, v)
  } else {
    v <- v[!is.na(v)]
  }

  # If after dropping/relabelling there's nothing left, return an empty row with Total 0
  if (length(v) == 0L) {
    out <- tibble::tibble(!!display_name := "Count n(%)", Total = "0 (0.0%)")
    return(out)
  }

  # Preserve factor level order if asked
  if (is.factor(dataset[[var_name]]) && sort_levels == "levels") {
    # If missing_label inserted, ensure it's appended to levels
    lv <- levels(dataset[[var_name]])
    if (include_missing && !(missing_label %in% lv)) lv <- c(lv, missing_label)
    v <- factor(v, levels = lv)
  } else {
    # Otherwise treat as character for flexible sorting later
    v <- as.character(v)
  }

  # Compute counts
  counts <- dplyr::tibble(val = v) |>
    dplyr::count(.data$val, name = "n")

  # Sorting
  counts <- switch(
    sort_levels,
    desc   = dplyr::arrange(counts, dplyr::desc(.data$n), .data$val),
    asc    = dplyr::arrange(counts, .data$n, .data$val),
    alpha  = dplyr::arrange(counts, .data$val),
    levels = {
      if (is.factor(v)) {
        # order by factor level index
        lvl_index <- match(counts$val, levels(v))
        counts[order(lvl_index), , drop = FALSE]
      } else {
        dplyr::arrange(counts, dplyr::desc(.data$n), .data$val)
      }
    }
  )

  # Optionally collapse tail into "Other"
  if (is.numeric(top_n) && top_n > 0L && nrow(counts) > top_n) {
    head_part <- counts[seq_len(top_n), , drop = FALSE]
    tail_part <- counts[-seq_len(top_n), , drop = FALSE]
    other_n   <- sum(tail_part$n)
    counts <- dplyr::bind_rows(
      head_part,
      tibble::tibble(val = other_label, n = other_n)
    )
  }

  total_n <- sum(counts$n)

  pct_fmt <- function(n) sprintf(paste0("%.", digits, "f%%"), 100 * n / total_n)

  # Build display strings "n (p%)"
  counts$value <- paste0(counts$n, " (", pct_fmt(counts$n), ")")

  # Spread to wide format (one column per level)
  wide <- tidyr::pivot_wider(
    counts,
    names_from = "val",
    values_from = "value"
  )

  # Add label and Total
  wide <- dplyr::mutate(
    wide,
    label = "Count n(%)",
    Total = paste0(total_n, " (", pct_fmt(total_n), ")")
  )
  # Put label first and rename to display_name
  wide <- dplyr::relocate(wide, .data$label)
  names(wide)[1] <- display_name

  wide
}
