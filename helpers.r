WORD_TABLE_FONT_SIZE <- 12L
WORD_TABLE_FOOTER_FONT_SIZE <- 10L
WORD_TABLE_DECIMAL_PLACES <- 3L
WORD_TABLE_ONE_DECIMAL_PLACE <- 1L
WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES <- 0L

new_word_table <- function(data) {
  table <- flextable::flextable(data)
  table <- flextable::theme_booktabs(table)
  flextable::set_table_properties(
    table,
    opts_word = list(repeat_headers = FALSE)
  )
}

save_word_table <- function(table, path) {
  table <- flextable::font(table, fontname = "Times New Roman", part = "all")
  table <- flextable::fontsize(table, size = WORD_TABLE_FONT_SIZE, part = "all")
  table <- flextable::fontsize(
    table,
    size = WORD_TABLE_FOOTER_FONT_SIZE,
    part = "footer"
  )

  table <- flextable::autofit(table)
  flextable::save_as_docx(table, path = path)
}

format_word_number <- function(table, j, digits = WORD_TABLE_DECIMAL_PLACES,
                               i = NULL, big.mark = NULL,
                               prefix = NULL, suffix = NULL) {
  arguments <- list(x = table, j = j, digits = digits)

  if (!is.null(i)) arguments$i <- i
  if (!is.null(big.mark)) arguments$big.mark <- big.mark
  if (!is.null(prefix)) arguments$prefix <- prefix
  if (!is.null(suffix)) arguments$suffix <- suffix

  do.call(flextable::colformat_double, arguments)
}

format_word_currency <- function(table, j, i = NULL) {
  format_word_number(
    table,
    j = j,
    digits = WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES,
    i = i,
    big.mark = ",",
    prefix = "$"
  )
}

format_word_percent <- function(table, j, i = NULL) {
  format_word_number(
    table,
    j = j,
    digits = WORD_TABLE_ONE_DECIMAL_PLACE,
    i = i,
    suffix = "%"
  )
}

format_word_stat <- function(label, value, digits = WORD_TABLE_DECIMAL_PLACES) {
  sprintf("%s: %.*f", label, digits, as.numeric(value))
}

add_regression_footer <- function(table, fit_stats, extra_lines = character()) {
  fit_stat_lines <- vapply(
    names(fit_stats),
    function(label) format_word_stat(label, fit_stats[[label]]),
    character(1)
  )
  footer_lines <- c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    extra_lines,
    fit_stat_lines
  )

  flextable::add_footer_lines(table, values = footer_lines)
}

add_regression_header <- function(table, n, title = NULL, vcov_type = NULL) {
  header_lines <- c(
    title,
    paste0("N = ", format(n, big.mark = ",")),
    if (!is.null(vcov_type)) paste("Standard errors:", vcov_type)
  )
  header_lines <- header_lines[!is.na(header_lines) & nzchar(header_lines)]

  table <- flextable::add_header_lines(table, values = header_lines)
  flextable::align(
    table,
    i = seq_along(header_lines),
    align = "right",
    part = "header"
  )
}

format_regression_table <- function(table) {
  estimate_columns <- intersect(
    c(
      "Estimate",
      "Std. Error",
      "t value",
      "z value",
      "Avg Marginal Effect",
      "LPM Estimate",
      "Logit Avg Marginal Effect"
    ),
    table$col_keys
  )
  p_value_columns <- grep("^Pr", table$col_keys, value = TRUE)

  if (length(estimate_columns) > 0) {
    table <- format_word_number(table, j = estimate_columns)
  }

  if (length(p_value_columns) > 0) {
    table <- format_word_number(table, j = p_value_columns)
    table <- flextable::set_header_labels(
      table,
      values = as.list(stats::setNames(rep("P-value", length(p_value_columns)), p_value_columns))
    )
  }

  if ("Term" %in% table$col_keys) {
    table <- flextable::align(table, j = "Term", align = "left", part = "all")
  }

  numeric_columns <- unique(c(estimate_columns, p_value_columns))
  if (length(numeric_columns) > 0) {
    table <- flextable::align(
      table,
      j = numeric_columns,
      align = "right",
      part = "all"
    )
  }

  table
}
