#' One-line summary for a continuous variable
#'
#' Produces a single display-ready \code{tibble} row summarizing a numeric column:
#' mean (sd), median, min–max, selected percentiles, and missingness counts.
#'
#' @param dataset A data frame containing \code{var_name}.
#' @param var_name String. The column name in \code{dataset} to summarize (must be numeric).
#' @param display_name String. Label to show in the \code{Variable} column.
#'   Defaults to \code{var_name}.
#' @param digits Integer. Number of decimal places for formatted outputs (default \code{2}).
#' @param finite_only Logical. If \code{TRUE} (default), exclude \code{Inf/-Inf} from summaries.
#' @param probs Numeric vector of quantile probabilities in the order you want reported.
#'   Defaults to \code{c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)}.
#'
#' @returns A one-row \code{tibble} with columns:
#' \itemize{
#'   \item \code{Variable}, \code{Mean_SD}, \code{Median}, \code{Min_Max},
#'   \item \code{P1}, \code{P5}, \code{P10}, \code{Q1}, \code{Q2}, \code{Q3}, \code{P90}, \code{P95}, \code{P99},
#'   \item \code{Missing}, \code{N}, \code{NonMissing}, \code{MissingPct}.
#' }
#'
#' @details
#' If all values are missing (or filtered out by \code{finite_only = TRUE}),
#' numeric summaries are returned as \code{NA_character_} while counts are provided.
#'
#' @examples
#' x <- data.frame(a = c(rnorm(100), NA, Inf))
#' BD_1_cont(x, "a", display_name = "My Var")
#'
#' @importFrom tibble tibble
#' @importFrom stats quantile sd median
#' @export
BD_1_cont <- function(dataset,
                      var_name,
                      display_name = var_name,
                      digits = 2,
                      finite_only = TRUE,
                      probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)) {

  # --- Input checks ---
  stopifnot(is.data.frame(dataset))
  if (!var_name %in% names(dataset)) stop("Variable not found in dataset: ", var_name)
  x <- dataset[[var_name]]
  if (!is.numeric(x)) stop("Variable must be numeric: ", var_name)

  # --- Missing / finite handling ---
  na_idx <- is.na(x)
  missing_n <- sum(na_idx)
  if (isTRUE(finite_only)) {
    ok <- !is.na(x) & is.finite(x)
  } else {
    ok <- !is.na(x)
  }
  x_ok <- x[ok]
  non_missing_n <- length(x_ok)

  # Early return if nothing to summarize
  if (non_missing_n == 0L) {
    return(tibble::tibble(
      Variable = display_name,
      Mean_SD  = NA_character_,
      Median   = NA_character_,
      Min_Max  = NA_character_,
      P1  = NA_character_, P5  = NA_character_, P10 = NA_character_,
      Q1  = NA_character_, Q2  = NA_character_, Q3  = NA_character_,
      P90 = NA_character_, P95 = NA_character_, P99 = NA_character_,
      Missing    = missing_n,
      N          = length(x),
      NonMissing = non_missing_n,
      MissingPct = round(100 * missing_n / length(x), 1)
    ))
  }

  # --- Core summaries ---
  m   <- base::mean(x_ok)
  sdv <- stats::sd(x_ok)
  med <- stats::median(x_ok)
  mn  <- base::min(x_ok)
  mx  <- base::max(x_ok)

  # Quantiles in requested order
  qs <- stats::quantile(x_ok, probs = probs, names = FALSE, type = 7)

  # Map quantiles to labels (expects the default probs order shown above)
  # If you change probs, labels may not match; keep default for standard table output.
  q_labels <- setNames(qs, c("P1","P5","P10","Q1","Q2","Q3","P90","P95","P99")[seq_along(qs)])

  # Formatter
  fmt <- function(v) sprintf(paste0("%.", digits, "f"), v)

  tibble::tibble(
    Variable = display_name,
    Mean_SD  = sprintf("%s (%s)", fmt(m), fmt(sdv)),
    Median   = fmt(med),
    Min_Max  = sprintf("%s - %s", fmt(mn), fmt(mx)),
    P1  = if ("P1"  %in% names(q_labels)) fmt(q_labels["P1"])  else NA_character_,
    P5  = if ("P5"  %in% names(q_labels)) fmt(q_labels["P5"])  else NA_character_,
    P10 = if ("P10" %in% names(q_labels)) fmt(q_labels["P10"]) else NA_character_,
    Q1  = if ("Q1"  %in% names(q_labels)) fmt(q_labels["Q1"])  else NA_character_,
    Q2  = if ("Q2"  %in% names(q_labels)) fmt(q_labels["Q2"])  else NA_character_,
    Q3  = if ("Q3"  %in% names(q_labels)) fmt(q_labels["Q3"])  else NA_character_,
    P90 = if ("P90" %in% names(q_labels)) fmt(q_labels["P90"]) else NA_character_,
    P95 = if ("P95" %in% names(q_labels)) fmt(q_labels["P95"]) else NA_character_,
    P99 = if ("P99" %in% names(q_labels)) fmt(q_labels["P99"]) else NA_character_,
    Missing    = missing_n,
    N          = length(x),
    NonMissing = non_missing_n,
    MissingPct = round(100 * missing_n / length(x), 1)
  )
}
