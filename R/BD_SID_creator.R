#' Create a stable, de-identified SID from name, DOB, and gender
#'
#' @description
#' `BD_SID_creator()` builds an SID by concatenating:
#' - First 2 letters of the **first name** (A–Z only, uppercased)
#' - First 2 letters of the **last name** (A–Z only, uppercased)
#' - **DOB** as `"YYYYMMDD"`
#' - First letter of **gender** (uppercased)
#'
#' Any row with a missing component yields `"Invalid SID"`.
#'
#' @details
#' - Fully vectorized; works directly inside `dplyr::mutate()`.
#' - Accepts DOB as `Date`, `POSIXt`, character, or numeric (e.g. `20240101`).
#' - Character DOBs are parsed against these formats:
#'   `"%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%Y%m%d", "%d-%b-%Y", "%b %d, %Y"`.
#' - Name cleaning removes non-letters `[A-Za-z]`. If you expect accents,
#'   see **Accent handling** below.
#'
#' @section Accent handling:
#' By default, only ASCII letters are kept. To turn “José” into “JO” rather than
#' dropping the accented character, set `strip_accents = TRUE` (uses base `iconv`
#' to transliterate where possible).
#'
#' @param first_name,last_name Vectors coercible to character. Only letters are kept.
#'   If fewer than 2 letters remain, that component is treated as missing.
#' @param DOB_ready A vector of dates (`Date`, `POSIXt`, character, or numeric).
#' @param gender Vector coercible to character; first character used.
#' @param strip_accents Logical, attempt to transliterate accents before cleaning.
#'
#' @return A character vector of SIDs. Rows with any missing component are `"Invalid SID"`.
#'
#' @examples
#' # Simple vectors
#' BD_SID_creator("Ann", "O'Neil", "1990-07-03", "F")
#' BD_SID_creator(c("Li", "Élodie"), c("Xu", "Brûlé"),
#'                c("2000/01/02", "03/14/1999"),
#'                c("M", "f"), strip_accents = TRUE)
#'
#' # Inside dplyr
#' # dplyr::mutate(df, SID = BD_SID_creator(FIRST_NAME, LAST_NAME, DOB, SEX))
#'
#' @export
BD_SID_creator <- function(first_name, last_name, DOB_ready, gender, strip_accents = FALSE) {

  # --- helpers (vectorized) ---
  .maybe_strip_accents <- function(x) {
    if (!strip_accents) return(x)
    # Try transliteration; if it fails, fall back to original
    x2 <- tryCatch(iconv(x, from = "", to = "ASCII//TRANSLIT"), error = function(e) x)
    x2[is.na(x2)] <- x[is.na(x2)]
    x2
  }

  clean_name_vec <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- .maybe_strip_accents(x)
    # keep only letters A–Z
    x <- gsub("[^A-Za-z]", "", x)
    # need at least 2 letters to contribute
    out <- ifelse(nchar(x) >= 2, toupper(substr(x, 1, 2)), NA_character_)
    out
  }

  clean_gender_vec <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    out <- toupper(substr(x, 1, 1))
    out[nchar(out) == 0] <- NA_character_
    out
  }

  to_yyyymmdd_vec <- function(x) {
    # Accept Date, POSIXt, character, or numeric like 20240101
    if (inherits(x, "Date")) {
      d <- x
    } else if (inherits(x, "POSIXt")) {
      d <- as.Date(x)
    } else {
      # coerce numeric like 20240101 safely
      x_chr <- as.character(x)
      d <- suppressWarnings(as.Date(
        x_chr,
        tryFormats = c(
          "%Y-%m-%d",  # 2025-09-18
          "%m/%d/%Y",  # 09/18/2025
          "%d/%m/%Y",  # 18/09/2025
          "%Y/%m/%d",  # 2025/09/18
          "%Y%m%d",    # 20250918
          "%d-%b-%Y",  # 18-Sep-2025
          "%b %d, %Y"  # Sep 18, 2025
        )
      ))
    }
    out <- ifelse(is.na(d), NA_character_, format(d, "%Y%m%d"))
    out
  }

  FN  <- clean_name_vec(first_name)
  LN  <- clean_name_vec(last_name)
  G   <- clean_gender_vec(gender)
  DOB <- to_yyyymmdd_vec(DOB_ready)

  # invalid if any component missing
  invalid <- is.na(FN) | is.na(LN) | is.na(DOB) | is.na(G)

  SID <- ifelse(invalid, "Invalid SID", paste0(FN, LN, DOB, G))
  # ensure plain character, no NA (we used "Invalid SID" sentinel)
  as.character(SID)
}
