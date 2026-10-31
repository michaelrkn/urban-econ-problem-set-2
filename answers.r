library(haven)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)

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
  flextable() |>
  set_header_labels(
    year = "Year",
    mean_price = "Mean Price",
    median_price = "Median Price",
    mean_sf = "Mean SF",
    mean_bathrooms = "Mean Baths",
    naive_mean_index = "Naive Mean Log Price Index",
    naive_median_index = "Naive Median Log Price Index"
  ) |>
  colformat_double(j = c("mean_price", "median_price", "naive_mean_index", "naive_median_index"), digits = 0, prefix = "$") |>
  colformat_double(j = c("mean_bathrooms"), digits = 1) |>
  colformat_double(j = c("mean_sf"), digits = 0) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/1.1.docx")

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
model_fit <- fitstat(hedonic_model, c("rmse", "ar2"))

flextable(coefficient_table) |>
  theme_booktabs() |>
  set_table_properties(opts_word = list(repeat_headers = FALSE)) |>
  add_header_lines(values = c(
    paste("N = ", format(model_summary$nobs, big.mark = ",")),
    paste("Standard errors:", attr(model_summary$coeftable, "vcov_type"))
  )) |>
  set_header_labels(`Pr(>|t|)` = "P-value") |>
  add_footer_lines(values = c(
    "*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1",
    sprintf("RMSE: %.6f", as.numeric(model_fit["rmse"])),
    sprintf("Adj. R-squared: %.6f", as.numeric(model_fit["ar2"]))
  )) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  fontsize(size = 10, part = "footer") |>
  padding(padding.top = 0, padding.bottom = 0) |>
  colformat_double(j = c("Estimate", "Std. Error", "t value"), digits = 2) |>
  colformat_double(j = "Pr(>|t|)", digits = 3) |>
  align(j = "Term", align = "left", part = "all") |>
  align(i = 1:2, align = "right", part = "header") |>
  align(j = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/1.2.docx")

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
  flextable() |>
  set_header_labels(
    Measure = "",
    `Single Sale` = "Single Sale",
    `Multiple Sales` = "Multiple Sales"
  ) |>
  colformat_double(j = c("Single Sale", "Multiple Sales"), digits = 3) |>
  colformat_double(i = ~ Measure == "Mean sqft2", j = c("Single Sale", "Multiple Sales"), digits = 0, big.mark = ",") |>
  colformat_double(i = ~ Measure == "Mean median income", j = c("Single Sale", "Multiple Sales"), digits = 0, big.mark = ",", prefix = "$") |>
  colformat_double(i = ~ Measure %in% c("Mean Black share", "Mean Hispanic share", "Mean Asian share"), j = c("Single Sale", "Multiple Sales"), digits = 1, suffix = "%") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  align(j = "Measure", align = "left", part = "all") |>
  align(j = c("Single Sale", "Multiple Sales"), align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/1.4.docx")


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

flextable(index_comparison) |>
  theme_booktabs() |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 12, part = "all") |>
  colformat_double(j = 2:5, digits = 3) |>
  colformat_double(j = c("Change Index (Shortcut)", "Change Index"), digits = 0) |>
  align(j = "Model", align = "left", part = "all") |>
  align(j = 2:5, align = "right", part = "all") |>
  autofit() |>
  save_as_docx(path = "answers/1.5b.docx")

