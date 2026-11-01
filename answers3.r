library(haven)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)

tenure_data <- read_dta("data/tenure_choice.dta")


# 3.1

tenure_data <- mutate(tenure_data,
  birth_year = 2010 - age,
  greatest_silent = (age >= 65),
  boomer = (age >= 46 & age <= 64),
  genx = (age >= 30 & age <= 45),
  millenial = (age <= 29),
  married = (marst == 1),
  separated = (marst == 2),
  unmarried = (marst == 3),
  native_citizen = (citizen == 0),
  abroad_citizen = (citizen == 1),
  naturalized_citizen = (citizen == 2),
  noncitizen = (citizen == 3),
  log_income = log(hhincome),
  income_quintile = ntile(hhincome, 5),
  income_quintile_1 = income_quintile == 1,
  income_quintile_2 = income_quintile == 2,
  income_quintile_3 = income_quintile == 3,
  income_quintile_4 = income_quintile == 4,
  income_quintile_5 = income_quintile == 5,
  homeowner = (own == 1),
  renter = (own == 0)
)

sum(tenure_data$trantime == 0 & tenure_data$empstat == 1, na.rm = TRUE)


# 3.2
demographic_means <- function(data) {
  data |>
    summarise(across(
      c(hhincome, age, married, native_citizen),
      ~ mean(.x, na.rm = TRUE)
    ))
}

demographic_data <- bind_rows(
  list(
    "Full Sample" = demographic_means(tenure_data),
    "Homeowners" = demographic_means(filter(tenure_data, homeowner == 1)),
    "Renters" = demographic_means(filter(tenure_data, renter == 1))
  ),
  .id = "Sample"
) |>
  tidyr::pivot_longer(
    cols = -Sample,
    names_to = "Variable",
    values_to = "Mean"
  ) |>
  tidyr::pivot_wider(names_from = Sample, values_from = Mean) |>
  mutate(Variable = recode(
    Variable,
    hhincome = "Household Income",
    age = "Age",
    married = "Proportion Married",
    native_citizen = "Proportion Native Citizen"
  ))

flextable(demographic_data) |>
  theme_booktabs() |>
  set_header_labels(Variable = "", `Full Sample` = "Full Sample") |>
  colformat_double(j = c("Full Sample", "Homeowners", "Renters"), digits = 2) |>
  colformat_double(
    j = c("Full Sample", "Homeowners", "Renters"),
    i = which(demographic_data$Variable == "Age"),
    digits = 0
  ) |>
  colformat_double(
    j = c("Full Sample", "Homeowners", "Renters"),
    i = which(demographic_data$Variable == "Household Income"),
    digits = 0,
    big.mark = ",",
    prefix = "$"
  ) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  align(j = "Variable", align = "left", part = "all") |>
  align(j = c("Full Sample", "Homeowners", "Renters"), align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/3.2a.docx")

  cohort_percentages <- tenure_data |>
    summarise(across(
      c(greatest_silent, boomer, genx, millenial),
      ~ sum(.x == 1, na.rm = TRUE) / n() * 100
    )) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "Cohort",
      values_to = "Percent"
    ) |>
    mutate(`Cohort` = recode(
      `Cohort`,
      greatest_silent = "Greatest/Silent Generations",
      boomer = "Baby Boomers",
      genx = "Generation X",
      millenial = "Millennials"
    ))

  flextable(cohort_percentages) |>
    theme_booktabs() |>
    set_header_labels(`Cohort` = "Cohort") |>
    colformat_double(j = "Percent", digits = 1, suffix = "%") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 12, part = "all") |>
    align(j = "Cohort", align = "left", part = "all") |>
    align(j = "Percent", align = "right", part = "all") |>
    autofit() |>
    save_as_docx(path = "answers/3.2b.docx")

income_percentiles <- tibble::tibble(
  Percentile = c("10th", "25th", "50th", "75th", "90th"),
  `Household Income` = as.numeric(quantile(
    tenure_data$hhincome,
    probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
    na.rm = TRUE
  ))
)

flextable(income_percentiles) |>
  theme_booktabs() |>
  colformat_double(j = "Household Income", digits = 0, big.mark = ",", prefix = "$" ) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  align(j = "Percentile", align = "left", part = "all") |>
  align(j = "Household Income", align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/3.2c.docx")

log_income_density <- ggplot(tenure_data, aes(x = log_income)) +
  geom_density() +
  labs(x = "Log Household Income", y = "Density") +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/3.2c.png", plot = log_income_density)


homeowner_cohort_percentages <- tenure_data |>
  tidyr::pivot_longer(
    cols = c(greatest_silent, boomer, genx, millenial),
    names_to = "Cohort",
    values_to = "In Cohort"
  ) |>
  filter(`In Cohort`) |>
  group_by(Cohort) |>
  summarise(
    Percent = mean(homeowner == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(Cohort = recode(
    Cohort,
    greatest_silent = "Greatest/Silent Generations",
    boomer = "Baby Boomers",
    genx = "Generation X",
    millenial = "Millennials"
  )) |>
  mutate(Cohort = factor(Cohort, levels = c(
    "Greatest/Silent Generations",
    "Baby Boomers",
    "Generation X",
    "Millennials"
  )))

homeowner_cohort_plot <- ggplot(
  homeowner_cohort_percentages,
  aes(x = Cohort, y = Percent, fill = Cohort)
) +
  geom_col(width = 0.5) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(
    title = "Homeownership Rate by Cohort",
    x = "Cohort",
    y = "Homeownership Rate (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/3.2di.png", plot = homeowner_cohort_plot)

homeownership_by_age <- tenure_data |>
  filter(!is.na(age)) |>
  mutate(age = as.numeric(age)) |>
  group_by(age) |>
  summarise(
    homeownership_rate = mean(homeowner == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  )

homeownership_by_age_plot <- ggplot(
  homeownership_by_age,
  aes(x = age, y = homeownership_rate)
) +
  geom_line() +
  scale_x_continuous(breaks = pretty(homeownership_by_age$age)) +
  scale_y_continuous(breaks = pretty(homeownership_by_age$homeownership_rate)) +
  labs(
    title = "Homeownership Rate by Age",
    x = "Age",
    y = "Homeownership Rate (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/3.2ii.png", plot = homeownership_by_age_plot)


# 3.3

cohort_model <- feols(homeowner ~ greatest_silent + genx + millenial, tenure_data)

cohort_model_summary <- summary(cohort_model)
cohort_coefficients <- cohort_model_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term") |>
  mutate(Term = sub("TRUE$", "", Term))
cohort_model_fit <- fitstat(cohort_model, c("rmse", "r2", "ar2"))

flextable(cohort_coefficients) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = c(
    "OLS Model: Homeownership by Cohort",
    paste("N =", format(cohort_model_summary$nobs, big.mark = ",")),
    paste("Standard errors:", attr(cohort_model_summary$coeftable, "vcov_type"))
  )) |>
  set_header_labels(`Pr(>|t|)` = "P-value") |>
  add_footer_lines(values = c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    sprintf("RMSE: %.4f", as.numeric(cohort_model_fit["rmse"])),
    sprintf("R-squared: %.4f", as.numeric(cohort_model_fit["r2"])),
    sprintf("Adjusted R-squared: %.4f", as.numeric(cohort_model_fit["ar2"]))
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "footer") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(j = c("Estimate", "Std. Error", "t value"), digits = 4) |>
  colformat_double(j = "Pr(>|t|)", digits = 3) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1:3, align = "right", part = "header") |>
  align(j = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/3.3a.docx")


cohort_model_with_controls <- feols(
  homeowner ~ greatest_silent + genx + millenial + 
  log_income + married + vet + native_citizen,
   tenure_data)

cohort_model_with_controls_summary <- summary(cohort_model_with_controls)
cohort_controls_coefficients <- cohort_model_with_controls_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term") |>
  mutate(Term = sub("TRUE$", "", Term))
cohort_controls_fit <- fitstat(cohort_model_with_controls, c("rmse", "r2", "ar2"))

flextable(cohort_controls_coefficients) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = c(
    "OLS Model: Homeownership by Cohort with Controls",
    paste("N =", format(cohort_model_with_controls_summary$nobs, big.mark = ",")),
    paste(
      "Standard errors:",
      attr(cohort_model_with_controls_summary$coeftable, "vcov_type")
    )
  )) |>
  set_header_labels(`Pr(>|t|)` = "P-value") |>
  add_footer_lines(values = c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    sprintf("RMSE: %.3f", as.numeric(cohort_controls_fit["rmse"])),
    sprintf("R-squared: %.3f", as.numeric(cohort_controls_fit["r2"])),
    sprintf("Adjusted R-squared: %.3f", as.numeric(cohort_controls_fit["ar2"]))
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "footer") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(j = c("Estimate", "Std. Error", "t value"), digits = 3) |>
  colformat_double(j = "Pr(>|t|)", digits = 3) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1:3, align = "right", part = "header") |>
  align(
    j = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.3b.docx")


# 3.4

income_reference <- quantile(
  tenure_data$hhincome,
  probs = c(0.25, 0.90),
  na.rm = TRUE
)
veteran_reference <- mean(tenure_data$vet, na.rm = TRUE)

cohort_prediction_data <- tibble::tibble(
  Cohort = c(
    "Greatest/Silent Generations",
    "Baby Boomers",
    "Generation X",
    "Millennials"
  ),
  greatest_silent = c(TRUE, FALSE, FALSE, FALSE),
  genx = c(FALSE, FALSE, TRUE, FALSE),
  millenial = c(FALSE, FALSE, FALSE, TRUE)
)

cohort_predictions <- cohort_prediction_data |>
  mutate(
    `25th Percentile Income` = as.numeric(predict(
      cohort_model_with_controls,
      newdata = tibble::tibble(
        greatest_silent = greatest_silent,
        genx = genx,
        millenial = millenial,
        log_income = log(income_reference[[1]]),
        married = TRUE,
        vet = veteran_reference,
        native_citizen = TRUE
      )
    )) * 100,
    `90th Percentile Income` = as.numeric(predict(
      cohort_model_with_controls,
      newdata = tibble::tibble(
        greatest_silent = greatest_silent,
        genx = genx,
        millenial = millenial,
        log_income = log(income_reference[[2]]),
        married = TRUE,
        vet = veteran_reference,
        native_citizen = TRUE
      )
    )) * 100
  ) |>
  select(Cohort, `25th Percentile Income`, `90th Percentile Income`)

flextable(cohort_predictions) |>
  theme_booktabs() |>
  add_header_lines(values = "Predicted Homeownership Rate by Cohort") |>
  set_header_labels(
    Cohort = "Cohort",
    `25th Percentile Income` = "25th Percentile Income",
    `90th Percentile Income` = "90th Percentile Income"
  ) |>
  colformat_double(
    j = c("25th Percentile Income", "90th Percentile Income"),
    digits = 1,
    suffix = "%"
  ) |>
  add_footer_lines(values = c(
    sprintf(
      "Married and native-born; veteran status held at sample mean (%.1f%%)",
      veteran_reference * 100
    ),
    sprintf(
      "Income cutoffs: 25th percentile = $%s; 90th percentile = $%s",
      format(income_reference[[1]], big.mark = ",", scientific = FALSE),
      format(income_reference[[2]], big.mark = ",", scientific = FALSE)
    )
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 12, part = "header") |>
  border_inner_h(
    border = officer::fp_border(color = "white", width = 0),
    part = "header"
  ) |>
  align(j = "Cohort", align = "left", part = "all") |>
  align(
    j = c("25th Percentile Income", "90th Percentile Income"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.4.docx")


# 3.5

cohort_model_logit <- feglm(
  homeowner ~ greatest_silent + genx + millenial + 
  log_income + married + vet + native_citizen,
  family = binomial(link = "logit"),
  data = tenure_data
)

cohort_model_logit_summary <- summary(cohort_model_logit)

cohort_logit_coefficients <- cohort_model_logit_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term") |>
  mutate(Term = sub("TRUE$", "", Term))
cohort_logit_fit <- fitstat(cohort_model_logit, c("ll", "pr2", "aic"))

flextable(cohort_logit_coefficients) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = c(
    "Logit Model: Homeownership by Cohort with Controls",
    paste("N =", format(cohort_model_logit_summary$nobs, big.mark = ",")),
    paste(
      "Standard errors:",
      attr(cohort_model_logit_summary$coeftable, "vcov_type")
    )
  )) |>
  set_header_labels(`Pr(>|z|)` = "P-value") |>
  add_footer_lines(values = c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    sprintf("Log-likelihood: %.3f", as.numeric(cohort_logit_fit["ll"])),
    sprintf("Pseudo R-squared: %.3f", as.numeric(cohort_logit_fit["pr2"])),
    sprintf("AIC: %.3f", as.numeric(cohort_logit_fit["aic"]))
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "footer") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(
    j = c("Estimate", "Std. Error", "z value"),
    digits = 3
  ) |>
  colformat_double(j = "Pr(>|z|)", digits = 3) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1:3, align = "right", part = "header") |>
  align(
    j = c("Estimate", "Std. Error", "z value", "Pr(>|z|)"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.5.docx")

cohort_controls_logit_slopes <- marginaleffects::avg_slopes(cohort_model_logit) |>
  as.data.frame() |>
  transmute(Term = sub("TRUE$", "", term), `Logit Avg Marginal Effect` = estimate)
cohort_controls_comparison <- cohort_controls_coefficients |>
  left_join(cohort_controls_logit_slopes, by = "Term") |>
  select(Term, `LPM Estimate` = Estimate, `Logit Avg Marginal Effect`)

flextable(cohort_controls_comparison) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = "LPM Estimates and Logit Average Marginal Effects") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(
    j = c("LPM Estimate", "Logit Avg Marginal Effect"),
    digits = 3
  ) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1, align = "right", part = "header") |>
  align(
    j = c("LPM Estimate", "Logit Avg Marginal Effect"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.5b.docx")

cohort_logit_predictions <- cohort_prediction_data |>
  mutate(
    `25th Percentile Income` = as.numeric(predict(
      cohort_model_logit,
      newdata = tibble::tibble(
        greatest_silent = greatest_silent,
        genx = genx,
        millenial = millenial,
        log_income = log(income_reference[[1]]),
        married = TRUE,
        vet = veteran_reference,
        native_citizen = TRUE
      ),
      type = "response"
    )) * 100,
    `90th Percentile Income` = as.numeric(predict(
      cohort_model_logit,
      newdata = tibble::tibble(
        greatest_silent = greatest_silent,
        genx = genx,
        millenial = millenial,
        log_income = log(income_reference[[2]]),
        married = TRUE,
        vet = veteran_reference,
        native_citizen = TRUE
      ),
      type = "response"
    )) * 100
  ) |>
  select(Cohort, `25th Percentile Income`, `90th Percentile Income`)

flextable(cohort_logit_predictions) |>
  theme_booktabs() |>
  add_header_lines(values = "Logit-Predicted Homeownership Rate by Cohort") |>
  set_header_labels(
    Cohort = "Cohort",
    `25th Percentile Income` = "25th Percentile Income",
    `90th Percentile Income` = "90th Percentile Income"
  ) |>
  colformat_double(
    j = c("25th Percentile Income", "90th Percentile Income"),
    digits = 1,
    suffix = "%"
  ) |>
  add_footer_lines(values = c(
    sprintf(
      "Married and native-born; veteran status held at sample mean (%.1f%%)",
      veteran_reference * 100
    ),
    sprintf(
      "Income cutoffs: 25th percentile = $%s; 90th percentile = $%s",
      format(income_reference[[1]], big.mark = ",", scientific = FALSE),
      format(income_reference[[2]], big.mark = ",", scientific = FALSE)
    )
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 12, part = "header") |>
  border_inner_h(
    border = officer::fp_border(color = "white", width = 0),
    part = "header"
  ) |>
  align(j = "Cohort", align = "left", part = "all") |>
  align(
    j = c("25th Percentile Income", "90th Percentile Income"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.5c.docx")


# 3.6

cohort_interactions_model_logit <- feglm(
  homeowner ~ greatest_silent*log_income + genx*log_income
  + millenial*log_income + married + vet + native_citizen,
  family = binomial(link = "logit"),
  data = tenure_data
)

cohort_interactions_summary <- summary(cohort_interactions_model_logit)
cohort_interactions_coefficients <- cohort_interactions_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term") |>
  mutate(Term = gsub("TRUE", "", Term)) |>
  mutate(Term = factor(Term, levels = c(
    "(Intercept)",
    "greatest_silent",
    "genx",
    "millenial",
    "greatest_silent:log_income",
    "log_income:genx",
    "log_income:millenial",
    "log_income",
    "married",
    "vet",
    "native_citizen"
  ))) |>
  arrange(Term) |>
  mutate(Term = as.character(Term))
cohort_interactions_fit <- fitstat(
  cohort_interactions_model_logit,
  c("ll", "pr2", "aic")
)

flextable(cohort_interactions_coefficients) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = c(
    "Logit Model: Homeownership by Cohort and Income Interactions",
    paste("N =", format(cohort_interactions_summary$nobs, big.mark = ",")),
    paste(
      "Standard errors:",
      attr(cohort_interactions_summary$coeftable, "vcov_type")
    )
  )) |>
  set_header_labels(`Pr(>|z|)` = "P-value") |>
  add_footer_lines(values = c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    sprintf("Log-likelihood: %.3f", as.numeric(cohort_interactions_fit["ll"])),
    sprintf("Pseudo R-squared: %.3f", as.numeric(cohort_interactions_fit["pr2"])),
    sprintf("AIC: %.3f", as.numeric(cohort_interactions_fit["aic"]))
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "footer") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(j = c("Estimate", "Std. Error", "z value"), digits = 3) |>
  colformat_double(j = "Pr(>|z|)", digits = 3) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1:3, align = "right", part = "header") |>
  align(
    j = c("Estimate", "Std. Error", "z value", "Pr(>|z|)"),
    align = "right",
    part = "all"
  ) |>
  autofit() |>
  save_as_docx(path = "answers/3.6.docx")


cohort_interactions_wald <- fixest::wald(
  cohort_interactions_model_logit,
  keep = "greatest_silentTRUE:log_income|log_income:genxTRUE|log_income:millenialTRUE"
)


cohort_income_range <- quantile(
  tenure_data$hhincome,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

cohort_income_grid <- tidyr::expand_grid(
  Cohort = factor(
    c("Greatest/Silent Generations", "Baby Boomers", "Generation X", "Millennials"),
    levels = c("Greatest/Silent Generations", "Baby Boomers", "Generation X", "Millennials")
  ),
  hhincome = seq(
    as.numeric(cohort_income_range[[1]]),
    as.numeric(cohort_income_range[[2]]),
    length.out = 100
  )
) |>
mutate(
  greatest_silent = Cohort == "Greatest/Silent Generations",
  genx = Cohort == "Generation X",
  millenial = Cohort == "Millennials",
  log_income = log(hhincome),
  married = TRUE,
  vet = veteran_reference,
  native_citizen = TRUE
)

cohort_income_grid$homeownership_probability <- as.numeric(predict(
  cohort_interactions_model_logit,
  newdata = cohort_income_grid |>
    select(greatest_silent, genx, millenial, log_income, married, vet, native_citizen),
  type = "response"
))

cohort_income_plot <- ggplot(
  cohort_income_grid,
  aes(x = hhincome, y = homeownership_probability, color = Cohort, group = Cohort)
) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(labels = scales::label_dollar()) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(
    title = "Predicted Homeownership Probability by Income",
    subtitle = sprintf(
      "For married and native-born; veteran status at sample mean (%.1f%%)",
      veteran_reference * 100
    ),
    x = "Household Income",
    y = "Probability of Homeownership",
    color = "Cohort"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/3.6.png", plot = cohort_income_plot)

