library(haven)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)

source("helpers.r")

price_data <- read_dta("data/price_data.dta")

# 1.1

naive <- price_data |>
  group_by(year) |>
  summarise(
    mean_price = mean(lnprice, na.rm = TRUE),
    median_price = median(lnprice, na.rm = TRUE),
    mean_sf = mean(sqft2, na.rm = TRUE) * 1000,
    mean_bathrooms = mean(numbath, na.rm = TRUE)
  ) |>
  mutate(
    naive_mean_index = mean_price - mean_price[year == 1990],
    naive_median_index = median_price - median_price[year == 1990]
  )

naive |>
  mutate(year = as.character(year)) |>
  new_word_table() |>
  set_header_labels(
    year = "Year",
    mean_price = "Mean Price",
    median_price = "Median Price",
    mean_sf = "Mean SF",
    mean_bathrooms = "Mean Baths",
    naive_mean_index = "Naive Mean Log Price Index",
    naive_median_index = "Naive Median Log Price Index"
  ) |>
  format_word_currency(j = c("mean_price", "median_price", "naive_mean_index", "naive_median_index")) |>
  format_word_number(j = "mean_bathrooms", digits = WORD_TABLE_ONE_DECIMAL_PLACE) |>
  format_word_number(j = "mean_sf", digits = WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES) |>
  save_word_table("answers/1.1.docx")

naive_plot <- naive |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = naive_mean_index, color = "Mean")) +
  geom_point(aes(y = naive_mean_index, color = "Mean")) +
  geom_line(aes(y = naive_median_index, color = "Median")) +
  geom_point(aes(y = naive_median_index, color = "Median")) +
  scale_color_manual(
    name = "Measure",
    values = c("Mean" = "#176B87", "Median" = "#D97732")
  ) +
  labs(
    title = "Naive Log Price Index",
    x = "Year",
    y = "Difference from 1990 Log Price"
  ) +
  theme_minimal(base_family = "Times New Roman")
ggsave("answers/1.1.png", plot = naive_plot, device = ragg::agg_png)


# 1.2

hedonic_model <- feols(
  lnprice ~ sqft2 + numbath + black_share + hisp_share +
    asian_share + med_income + i(year, ref = 1990),
  data = price_data,
  cluster = ~property_id
)

model_summary <- summary(hedonic_model)
coefficient_table <- model_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term")
model_fit <- fitstat(hedonic_model, c("rmse", "r2", "ar2"))

new_word_table(coefficient_table) |>
  add_regression_header(
    n = model_summary$nobs,
    title = "OLS Model: Hedonic Index",
    vcov_type = attr(model_summary$coeftable, "vcov_type")
  ) |>
  add_regression_footer(
    fit_stats = c(
      "RMSE" = model_fit["rmse"],
      "R-squared" = model_fit["r2"],
      "Adjusted R-squared" = model_fit["ar2"]
    )
  ) |>
  format_regression_table() |>
  save_word_table("answers/1.2.docx")

hedonic_log_index <- fixest::iplot(
  hedonic_model,
  i.select = 1,
  ci_level = 0.95,
  only.params = TRUE
)$prms

hedonic_indexlog__plot <- ggplot(hedonic_log_index, aes(x = x, y = estimate)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#176B87", alpha = 0.2) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_line(color = "#176B87") +
  geom_point(color = "#176B87") +
  labs(
    x = "Year",
    y = "Log points relative to 1990"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave(
  "answers/1.2b.png",
  plot = hedonic_indexlog__plot,
  device = ragg::agg_png
)

hedonic_level_index <- hedonic_log_index |>
  mutate(
    estimate = 100 * exp(estimate),
    ci_low = 100 * exp(ci_low),
    ci_high = 100 * exp(ci_high)
  )

hedonic_level_index_plot <- ggplot(hedonic_level_index, aes(x = x, y = estimate)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#176B87", alpha = 0.2) +
  geom_hline(yintercept = 100, color = "gray50", linetype = "dashed") +
  geom_line(color = "#176B87") +
  geom_point(color = "#176B87") +
  labs(
    x = "Year",
    y = "Price index (1990 = 100)"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave(
  "answers/1.2c.png",
  plot = hedonic_level_index_plot,
  device = ragg::agg_png
)


# 1.3

tract_fe_model <- feols(
  lnprice ~ sqft2 + numbath + i(year, ref = 1990) | tract,
  data = price_data,
  cluster = ~property_id
)


# 1.4

house_fe_model <- feols(
  lnprice ~ i(year, ref = 1990) | property_id,
  data = price_data,
  cluster = ~property_id
)

summary_variables <- c("sqft2", "numbath", "med_income", "black_share", "hisp_share", "asian_share")

repeat_sale_comparison <- price_data |>
  group_by(property_id) |>
  summarise(
    group = if_else(
      n() == 1,
      "Single Sale",
      "Multiple Sales"
    ),
    across(
      all_of(summary_variables),
      first
    ),
    .groups = "drop"
  ) |>
  group_by(group) |>
  summarise(
    across(
      all_of(summary_variables),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

repeat_sale_comparison |>
  tidyr::pivot_longer(
    cols = -group,
    names_to = "Measure",
    values_to = "Average"
  ) |>
  mutate(
    Measure = recode(
      Measure,
      sqft2 = "Mean sqft2",
      numbath = "Mean bathrooms",
      med_income = "Mean median income",
      black_share = "Mean Black share",
      hisp_share = "Mean Hispanic share",
      asian_share = "Mean Asian share"
    )
  ) |>
  tidyr::pivot_wider(names_from = group, values_from = Average) |>
  mutate(
    across(
      c(`Single Sale`, `Multiple Sales`),
      ~ case_when(
        Measure %in% c("Mean sqft2", "Mean median income") ~ .x * 1000,
        Measure %in% c("Mean Black share", "Mean Hispanic share", "Mean Asian share") ~ .x * 100,
        TRUE ~ .x
      )
    )
  ) |>
  new_word_table() |>
  set_header_labels(
    Measure = "",
    `Single Sale` = "Single Sale",
    `Multiple Sales` = "Multiple Sales"
  ) |>
  format_word_number(j = c("Single Sale", "Multiple Sales")) |>
  format_word_number(i = ~ Measure == "Mean sqft2", j = c("Single Sale", "Multiple Sales"), digits = WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES, big.mark = ",") |>
  format_word_currency(i = ~ Measure == "Mean median income", j = c("Single Sale", "Multiple Sales")) |>
  format_word_percent(i = ~ Measure %in% c("Mean Black share", "Mean Hispanic share", "Mean Asian share"), j = c("Single Sale", "Multiple Sales")) |>
  align(j = "Measure", align = "left", part = "all") |>
  align(j = c("Single Sale", "Multiple Sales"), align = "right", part = "all") |>
  save_word_table("answers/1.4.docx")


# 1.5

model_comparison <- bind_rows(
  naive |>
    transmute(year, estimate = naive_mean_index, Model = "Naive"),
  fixest::iplot(hedonic_model, i.select = 1, only.params = TRUE)$prms |>
    transmute(year = x, estimate, Model = "Hedonic"),
  fixest::iplot(tract_fe_model, i.select = 1, only.params = TRUE)$prms |>
    transmute(year = x, estimate, Model = "Tract fixed effects"),
  fixest::iplot(house_fe_model, i.select = 1, only.params = TRUE)$prms |>
    transmute(year = x, estimate, Model = "Property fixed effects")
)

model_comparison_plot <- ggplot(
  model_comparison,
  aes(x = year, y = estimate, color = Model, group = Model)
) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Log points relative to 1990",
    color = "Model"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave(
  "answers/1.5a.png",
  plot = model_comparison_plot,
  device = ragg::agg_png
)

index_comparison_plot <- model_comparison |>
  mutate(index = 100 * exp(estimate)) |>
  ggplot(aes(x = year, y = index, color = Model, group = Model)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, color = "gray50", linetype = "dashed") +
  labs(
    x = "Year",
    y = "Index (1990 = 100)",
    color = "Model"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave(
  "answers/1.5d.png",
  plot = index_comparison_plot,
  device = ragg::agg_png
)

index_comparison <- tibble::tibble(
  Model = c("Naive", "Hedonic", "Tract fixed effects", "Property fixed effects"),
  `Log Index` = c(
    naive$naive_mean_index[naive$year == 2007],
    unname(coef(hedonic_model)["year::2007"]),
    unname(coef(tract_fe_model)["year::2007"]),
    unname(coef(house_fe_model)["year::2007"])
  )
) |>
  mutate(
    `Change Index (Shortcut)` = 100 * `Log Index`,
    `Change Index` = 100 * (exp(`Log Index`) - 1),
    `Avg. Annual Growth` = 100 * (exp(`Log Index` / 17) - 1)
  )

new_word_table(index_comparison) |>
  format_word_number(j = 2:5) |>
  format_word_number(j = c("Change Index (Shortcut)", "Change Index"), digits = WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES) |>
  align(j = "Model", align = "left", part = "all") |>
  align(j = 2:5, align = "right", part = "all") |>
  save_word_table("answers/1.5b.docx")


# 1.6

tract_fe_quarterly_model <- feols(
  lnprice ~ sqft2 + numbath + i(yrquarter, ref = 19901) | tract,
  data = price_data,
  cluster = ~property_id
)

annual_tract_effects <- fixest::iplot(
  tract_fe_model,
  i.select = 1,
  ci_level = 0.95,
  only.params = TRUE
)$prms |>
  transmute(year_quarter = x, estimate, Model = "Annual")

quarterly_tract_effects <- fixest::iplot(
  tract_fe_quarterly_model,
  i.select = 1,
  ci_level = 0.95,
  only.params = TRUE
)$prms |>
  transmute(
    year_quarter = floor(x / 10) + ((x %% 10) - 1) / 4,
    estimate,
    Model = "Quarterly"
  )

tract_time_comparison <- bind_rows(annual_tract_effects, quarterly_tract_effects)

tract_time_comparison_plot <- ggplot(
  tract_time_comparison,
  aes(x = year_quarter, y = estimate, color = Model, group = Model)
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = seq(1990, 2007, by = 1),
    minor_breaks = seq(1990, 2008, by = 0.25),
    labels = seq(1990, 2007, by = 1)
  ) +
  scale_color_manual(values = c(
    "Annual" = "#176B87",
    "Quarterly" = "#D97732"
  )) +
  labs(
    x = "Year/quarter",
    y = "Log points relative to 1990",
    color = "Tract Fixed Effects"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(panel.grid.minor.x = element_line(color = "gray90", linewidth = 0.3))

ggsave(
  "answers/1.6.png",
  plot = tract_time_comparison_plot,
  device = ragg::agg_png,
  width = 12,
  height = 6
)


quarterly_sales_summary <- price_data |>
  count(yrquarter, name = "Sales") |>
  summarise(
    Mean = mean(Sales),
    Median = median(Sales),
    Minimum = min(Sales),
    Maximum = max(Sales),
    `Standard deviation` = sd(Sales)
  ) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Statistic",
    values_to = "Sales"
  )

new_word_table(quarterly_sales_summary) |>
  format_word_number(j = "Sales", digits = WORD_TABLE_ONE_DECIMAL_PLACE, big.mark = ",") |>
  format_word_number(
    i = ~ Statistic %in% c("Minimum", "Maximum"),
    j = "Sales",
    digits = WORD_TABLE_WHOLE_NUMBER_DECIMAL_PLACES,
    big.mark = ","
  ) |>
  align(j = "Sales", align = "right", part = "all") |>
  save_word_table("answers/1.6.docx")


# 1.7

median_tract_income <- median(price_data$med_income, na.rm = TRUE)
above_median_income <- filter(price_data, med_income >= median_tract_income)
below_median_income <- filter(price_data, med_income < median_tract_income)

above_median_income_tract_fe_model <- feols(
  lnprice ~ sqft2 + numbath + i(year, ref = 1990) | tract,
  data = above_median_income,
  cluster = ~property_id
)

below_median_income_tract_fe_model <- feols(
  lnprice ~ sqft2 + numbath + i(year, ref = 1990) | tract,
  data = below_median_income,
  cluster = ~property_id
)

income_group_year_effects <- bind_rows(
  fixest::iplot(
    above_median_income_tract_fe_model,
    i.select = 1,
    ci_level = 0.95,
    only.params = TRUE
  )$prms |>
    transmute(year = x, estimate, ci_low, ci_high, Group = "Above median income"),
  fixest::iplot(
    below_median_income_tract_fe_model,
    i.select = 1,
    ci_level = 0.95,
    only.params = TRUE
  )$prms |>
    transmute(year = x, estimate, ci_low, ci_high, Group = "Below median income")
)

income_group_year_effects_plot <- ggplot(
  income_group_year_effects,
  aes(x = year, y = estimate, color = Group, fill = Group, group = Group)
) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.18, color = NA) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  scale_color_manual(values = c(
    "Above median income" = "#176B87",
    "Below median income" = "#D97732"
  )) +
  scale_fill_manual(values = c(
    "Above median income" = "#176B87",
    "Below median income" = "#D97732"
  )) +
  labs(
    x = "Year",
    y = "Log points relative to 1990",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave(
  "answers/1.7.png",
  plot = income_group_year_effects_plot,
  device = ragg::agg_png
)

