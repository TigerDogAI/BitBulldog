#' Remove quotes from character/factor columns
#'
#' Strips common quote characters from character and factor columns,
#' leaving other column types unchanged.
#'
#' @param data A data.frame or tibble.
#' @param chars Quote characters to remove, applied literally (not regex).
#'   Default removes straight, curly, and backtick quotes.
#' @return The same data type with quotes removed from relevant columns.
#' @examples
#' df <- data.frame(a = "O'Neil", b = factor('“Hello”'), n = 1)
#' BD_quote_rm(df)
#' @export
BD_quote_rm <- function(data,
                        chars = c("'", "’", "\"", "“", "”", "`")) {
  strip_chars <- function(x) {
    for (ch in chars) x <- gsub(ch, "", x, fixed = TRUE)
    x
  }
  dplyr::mutate(
    data,
    dplyr::across(
      where(is.character),
      ~ strip_chars(.x)
    ),
    dplyr::across(
      where(is.factor),
      ~ {
        x <- strip_chars(as.character(.x))
        factor(x, levels = unique(x))
      }
    )
  )
}
