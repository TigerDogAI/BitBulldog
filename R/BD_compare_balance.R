#' Compare_balance
#'
#' Prints a hello message
#' @param name A character string
#' @return A greeting
#' @export

BD_compare_balance <- function(df1, df2, vars, weights1 = NULL, weights2 = NULL) {
  stopifnot(all(vars %in% union(names(df1), names(df2))))
  suppressPackageStartupMessages({
    library(dplyr); library(purrr); library(rlang)
  })

  # helpers ----
  w_mean <- function(x, w = NULL, na.rm = TRUE) {
    if (is.null(w)) return(mean(x, na.rm = na.rm))
    ok <- !is.na(x); x <- x[ok]; w <- w[ok]
    sum(w * x) / sum(w)
  }
  w_var <- function(x, w = NULL) {
    if (is.null(w)) return(var(x, na.rm = TRUE))
    ok <- !is.na(x); x <- x[ok]; w <- w[ok]
    if (length(x) <= 1 || sum(w) == 0) return(NA_real_)
    mu <- sum(w * x) / sum(w)
    sum(w * (x - mu)^2) / sum(w)
  }

  if (!is.null(weights1)) weights1 <- as.numeric(weights1)
  if (!is.null(weights2)) weights2 <- as.numeric(weights2)

  schema_src <- bind_rows(df1 |> mutate(.src = "A"),
                          df2 |> mutate(.src = "B"))

  out_list <- purrr::map(vars, function(v) {
    x1 <- df1[[v]]; x2 <- df2[[v]]
    if (is.character(x1)) x1 <- factor(x1)
    if (is.character(x2)) x2 <- factor(x2)
    w1 <- weights1; w2 <- weights2

    total1 <- length(x1); total2 <- length(x2)
    miss_n1 <- sum(is.na(x1)); miss_n2 <- sum(is.na(x2))
    miss_pct1 <- 100 * miss_n1 / total1
    miss_pct2 <- 100 * miss_n2 / total2

    if (is.numeric(x1) && is.numeric(x2)) {
      n1 <- sum(!is.na(x1)); n2 <- sum(!is.na(x2))
      m1 <- w_mean(x1, w1); m2 <- w_mean(x2, w2)
      sd1 <- sqrt(w_var(x1, w1)); sd2 <- sqrt(w_var(x2, w2))

      tibble::tibble(
        variable = v, type = "continuous", level = NA_character_,
        n1 = n1, mean1 = m1, sd1 = sd1,
        n2 = n2, mean2 = m2, sd2 = sd2,
        miss_n1 = miss_n1, miss_pct1 = miss_pct1,
        miss_n2 = miss_n2, miss_pct2 = miss_pct2,
        prop1 = NA_real_, prop2 = NA_real_
      )
    } else {
      lvls <- schema_src[[v]] |> as.factor() |> levels()
      f1 <- factor(x1, levels = lvls); f2 <- factor(x2, levels = lvls)
      n1 <- sum(!is.na(f1)); n2 <- sum(!is.na(f2))

      purrr::map_dfr(lvls, function(L) {
        p1 <- if (is.null(w1)) mean(f1 == L, na.rm = TRUE) else {
          ok <- !is.na(f1); if (!any(ok)) NA_real_ else sum(w1[ok] * as.numeric(f1[ok] == L)) / sum(w1[ok])
        }
        p2 <- if (is.null(w2)) mean(f2 == L, na.rm = TRUE) else {
          ok <- !is.na(f2); if (!any(ok)) NA_real_ else sum(w2[ok] * as.numeric(f2[ok] == L)) / sum(w2[ok])
        }

        tibble::tibble(
          variable = v, type = "categorical", level = L,
          prop1 = p1, prop2 = p2,
          n1 = n1, mean1 = NA_real_, sd1 = NA_real_,
          n2 = n2, mean2 = NA_real_, sd2 = NA_real_,
          miss_n1 = miss_n1, miss_pct1 = miss_pct1,
          miss_n2 = miss_n2, miss_pct2 = miss_pct2
        )
      })
    }
  })

  bind_rows(out_list) |>
    mutate(across(where(is.double), ~round(., 4))) |>
    select(variable, type, level,
           prop1, prop2,
           n1, mean1, sd1,
           n2, mean2, sd2,
           miss_n1, miss_pct1, miss_n2, miss_pct2)
}
