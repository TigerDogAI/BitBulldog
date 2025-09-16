library(tidyverse)

AH_today <- str_replace_all(today(),"-","_")

AH_fix_colname <- function(the_data){
  the_colnames <- colnames(the_data)
  the_colnames_2 <- str_replace_all(the_colnames," ","_")
  the_colnames_3 <- str_replace_all(the_colnames_2,"[()]","")
  the_colnames_4 <- the_colnames_3
  the_colnames_5 <- str_replace_all(the_colnames_4,"'","")
  the_colnames_6 <- str_replace_all(the_colnames_5,"-","")
  colnames(the_data) <- the_colnames_6
  return(the_data)
}


AH_quote_rm <- function(the_data){
  the_data_01 <- the_data %>% mutate_all(~ str_remove_all(.,"'"))
  return(the_data_01)
}

BD_check_unique <- function(data, var) {
  # ensure var can be passed without quotes
  var <- rlang::ensym(var)

  n_unique <- dplyr::n_distinct(dplyr::pull(data, !!var))
  n_rows   <- nrow(data)

  result <- list(
    variable = rlang::as_string(var),
    n_unique = n_unique,
    n_rows   = n_rows,
    is_unique = n_unique == n_rows
  )

  return(result)
}

BD_get_duplicates <- function(data, var) {
  var <- rlang::ensym(var)   # allows passing unquoted column name

  data %>%
    dplyr::group_by(!!var) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
}

BD_sec_to_hms <- function(x) {
  h <- x %/% 3600
  m <- (x %% 3600) %/% 60
  s <- x %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

BD_1_cat <- function(dataset, var_name, display_name){

  summary_table <- dataset %>%
    mutate(!!var_name := replace_na(.data[[var_name]], "Missing")) %>%
    count(.data[[var_name]]) %>%
    mutate(percent = sprintf("%.1f%%", n / sum(n) * 100)) %>%
    mutate(value = paste0(n, " (", percent, ")")) %>%
    dplyr::select(-n, -percent) %>%
    pivot_wider(names_from = .data[[var_name]], values_from = value) %>%
    mutate(variable = "Count n(%)") %>%
    relocate(variable)

  colnames(summary_table)[1] <- display_name

  return(summary_table)
}

BD_1_cat <- function(dataset, var_name, display_name) {

  # Replace NA and calculate counts
  counts <- dataset %>%
    mutate(!!var_name := replace_na(.data[[var_name]], "Missing")) %>%
    count(!!sym(var_name))

  # Total count
  total_n <- sum(counts$n)

  # Create formatted summary table
  summary_table <- counts %>%
    mutate(percent = sprintf("%.1f%%", n / total_n * 100),
           value = paste0(n, " (", percent, ")")) %>%
    select(-n, -percent) %>%
    pivot_wider(names_from = all_of(var_name), values_from = value) %>%
    mutate(label = "Count n(%)") %>%
    relocate(label)

  # Rename first column
  colnames(summary_table)[1] <- display_name

  # Add Total column at the end
  summary_table <- summary_table %>%
    mutate(Total = paste0(total_n, " (100.0%)"))

  return(summary_table)
}

BD_count_unique_categories <- function(data, variable_name) {
  if (!variable_name %in% names(data)) {
    stop("Variable not found in the dataset.")
  }
  return(length(unique(data[[variable_name]])))
}

BD_1_cont <- function(dataset, var_name, display_name) {
  # Input checks
  if (!var_name %in% names(dataset)) stop("Variable not found in dataset.")
  if (!is.numeric(dataset[[var_name]])) stop("Variable must be numeric.")

  # Extract and process data
  var_data <- dataset[[var_name]]
  missing_n <- sum(is.na(var_data))
  non_missing_data <- var_data[!is.na(var_data)]

  # Compute quantiles
  qs <- quantile(non_missing_data, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))

  # Create summary row
  summary_row <- tibble(
    Variable = display_name,
    Mean_SD = sprintf("%.2f (%.2f)", mean(non_missing_data), sd(non_missing_data)),
    Median = sprintf("%.2f", median(non_missing_data)),
    Min_Max = sprintf("%.2f - %.2f", min(non_missing_data), max(non_missing_data)),
    P1 = sprintf("%.2f", qs[1]),
    P5 = sprintf("%.2f", qs[2]),
    P10 = sprintf("%.2f", qs[3]),
    Q1 = sprintf("%.2f", qs[4]),
    Q2 = sprintf("%.2f", qs[5]),
    Q3 = sprintf("%.2f", qs[6]),
    P90 = sprintf("%.2f", qs[7]),
    P95 = sprintf("%.2f", qs[8]),
    P99 = sprintf("%.2f", qs[9]),
    Missing = missing_n
  )

  return(summary_row)
}
















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




# ------------------ Example ------------------
# Simulate "treated" vs "matched controls"
set.seed(42)
A <- data.frame(
  age = rnorm(300, 65, 10),
  bmi = rnorm(300, 28, 4),
  sex = factor(sample(c("F","M"), 300, TRUE, c(0.45, 0.55))),
  smoke = factor(sample(c("No","Yes"), 300, TRUE, c(0.7, 0.3)))
)
B <- data.frame(
  age = rnorm(300, 63, 11),
  bmi = rnorm(300, 29, 4.5),
  sex = factor(sample(c("F","M"), 300, TRUE, c(0.50, 0.50))),
  smoke = factor(sample(c("No","Yes"), 300, TRUE, c(0.75, 0.25)))
)

tbl <- BD_compare_balance(A, B, vars = c("age","bmi","sex","smoke"))
tbl
