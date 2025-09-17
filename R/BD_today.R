#' Today's date in two formats
#'
#' Returns today's date as "YYYY_MM_DD" and "YYYYMMDD".
#'
#' @param date A date (or something coercible to `Date`). Defaults to `Sys.Date()`.
#'
#' @return A named character vector of length 2:
#' \describe{
#'   \item{YMD_underscores}{e.g., "2025_09_16"}
#'   \item{YMD_compact}{e.g., "20250916"}
#' }
#'
#' @examples
#' BD_today()
#' BD_today("2024-01-05")
#'
#' @export
BD_today <- function(date = Sys.Date()) {
  d <- suppressWarnings(as.Date(date))
  if (is.na(d)) stop("`date` must be a Date or coercible to Date.")

  out <- c(
    YMD_underscores = format(d, "%Y_%m_%d"),
    YMD_compact     = format(d, "%Y%m%d")
  )
  out
}
