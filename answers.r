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
