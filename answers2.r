library(readr)
library(lubridate)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)

source("helpers.r")

options(tigris_use_cache = TRUE)

# 2.1

price_indices_wide <- read_csv("data/depaul_q4_price_indices.csv")
price_indices_wide <- mutate(price_indices_wide, yearq = lubridate::yq(yearq))
price_indices <- price_indices_wide |>
  tidyr::pivot_longer(
    cols = -yearq,
    names_to = "submarket",
    values_to = "index"
  ) |>
  tibble::as_tibble()


# 2.2

submarket_indices_plot <- price_indices |>
  filter(submarket %in% c("cook", "chicago", "suburbs")) |>
  ggplot(aes(x = yearq, y = index, color = submarket, group = submarket)) +
  geom_vline(
    xintercept = lubridate::yq(c("2007Q4", "2020Q1")),
    color = "gray50",
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point() +
  scale_color_discrete(
    name = "Submarket",
    labels = c(chicago = "Chicago", cook = "Cook", suburbs = "Suburbs")
  ) +
  labs(
    title = "Price Indices by Submarket",
    x = "Year",
    y = "Price Index"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/2.2a.png", plot = submarket_indices_plot, device = ragg::agg_png)

selected_submarket_indices_plot <- price_indices |>
  filter(submarket %in% c(
    "evanstonskokie",
    "chicagouptownrogerspark",
    "chicagobronzevillehydepark",
    "palatinebarrington",
    "calumetcityharvey"
  )) |>
  ggplot(aes(x = yearq, y = index, color = submarket, group = submarket)) +
  geom_vline(
    xintercept = lubridate::yq(c("2007Q4", "2020Q1")),
    color = "gray50",
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point() +
  scale_color_discrete(
    name = "Submarket",
    labels = c(
      calumetcityharvey = "Calumet City/Harvey",
      chicagobronzevillehydepark = "Bronzeville/Hyde Park",
      chicagouptownrogerspark = "Uptown/Roger's Park",
      evanstonskokie = "Evanston/Skokie",
      palatinebarrington = "Palatine/Barrington"
    )
  ) +
  labs(
    title = "Price Indices by Submarket",
    x = "Year",
    y = "Price Index"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/2.2b.png", plot = selected_submarket_indices_plot, device = ragg::agg_png)


# 2.3

target_submarkets <- c(
  "cook",
  "evanstonskokie",
  "chicagouptownrogerspark",
  "chicagobronzevillehydepark",
  "palatinebarrington",
  "calumetcityharvey"
)

quarter_index_summary <- price_indices |>
  filter(submarket %in% target_submarkets) |>
  group_by(submarket) |>
  group_modify(~ {
    series <- .x
    pre_2009_peak <- series |>
      filter(yearq < lubridate::yq("2009Q1")) |>
      slice_max(index, n = 1, with_ties = FALSE)
    post_2009_trough <- series |>
      filter(yearq >= lubridate::yq("2009Q1")) |>
      slice_min(index, n = 1, with_ties = FALSE)
    value_at <- function(quarter) {
      series |>
        filter(yearq == lubridate::yq(quarter)) |>
        pull(index)
    }
    format_number <- function(value, digits = WORD_TABLE_ONE_DECIMAL_PLACE) {
      format(round(value, digits), big.mark = ",", nsmall = digits, trim = TRUE)
    }
    percent_change <- function(start, end) {
      if (length(start) == 0 || length(end) == 0) NA_real_ else (end / start - 1) * 100
    }
    q2000 <- value_at("2000Q1")
    q2022 <- value_at("2022Q4")
    q2020 <- value_at("2020Q1")
    peak_value <- pre_2009_peak$index[[1]]
    trough_value <- post_2009_trough$index[[1]]
    peak_quarter <- paste0(
      lubridate::year(pre_2009_peak$yearq[[1]]),
      " Q",
      lubridate::quarter(pre_2009_peak$yearq[[1]])
    )
    tibble::tibble(
      Metric = c(
        "Boom peak value",
        "Boom peak quarter",
        "Boom peak growth from 2000 Q1",
        "Post-bust trough value",
        "Peak to trough decline",
        "Trough to 2022 Q4 recovery",
        "Is recovery above boom peak?",
        "2020 Q1 to 2022 Q4 change"
      ),
      Value = c(
        format_number(peak_value),
        peak_quarter,
        paste0(format_number(percent_change(q2000, peak_value)), "%"),
        format_number(trough_value),
        paste0(format_number(percent_change(peak_value, trough_value)), "%"),
        paste0(format_number(percent_change(trough_value, q2022)), "%"),
        ifelse(q2022[[1]] >= max(series$index, na.rm = TRUE), "Yes", "No"),
        paste0(format_number(percent_change(q2020, q2022)), "%")
      )
    )
  }) |>
  ungroup() |>
  tidyr::pivot_wider(names_from = submarket, values_from = Value) |>
  mutate(Metric = factor(Metric, levels = unique(Metric))) |>
  arrange(Metric) |>
  mutate(Metric = as.character(Metric))

new_word_table(quarter_index_summary) |>
  set_header_labels(
    Metric = "",
    cook = "Cook",
    evanstonskokie = "Evanston/Skokie",
    chicagouptownrogerspark = "Uptown/Roger's Park",
    chicagobronzevillehydepark = "Bronzeville/Hyde Park",
    palatinebarrington = "Palatine/Barrington",
    calumetcityharvey = "Calumet City/Harvey"
  ) |>
  align(j = "Metric", align = "left", part = "all") |>
  align(j = target_submarkets, align = "center", part = "all") |>
  save_word_table("answers/2.3.docx")


# 2.5

submarket_definitions <- readxl::read_excel(
  "data/2022_q4_price_index_data.xlsx",
  sheet = "Submarket_Definitions"
)

submarket_codes <- submarket_definitions |>
  filter(grepl("^[0-9]{5}$", Number)) |>
  select(Number, Submarket = `Sub Market`)

tract_sf1 <- tidycensus::get_decennial(
  geography = "tract",
  variables = c(
    population = "P001001",
    black_population = "P003004",
    hispanic_population = "P004002",
    occupied_housing_units = "H004001",
    owner_occupied_units = "H004002"
  ),
  year = 2000,
  sumfile = "sf1",
  state = "IL",
  geometry = TRUE,
  output = "wide"
)

tract_sf3 <- tidycensus::get_decennial(
  geography = "tract",
  variables = c(median_household_income = "P053001"),
  year = 2000,
  sumfile = "sf3",
  state = "IL",
  output = "wide"
)

pumas_2010 <- tigris::pumas(state = "IL", year = 2019, cb = TRUE) |>
  sf::st_transform(5070) |>
  mutate(Number = substr(GEOID10, 3, 7)) |>
  select(Number)

tract_centers <- tract_sf1 |>
  left_join(tract_sf3, by = "GEOID") |>
  sf::st_transform(5070)
sf::st_geometry(tract_centers) <- sf::st_point_on_surface(sf::st_geometry(tract_centers))

tract_demographics <- sf::st_join(
  tract_centers,
  pumas_2010,
  join = sf::st_within,
  left = TRUE
)

puma_demographics <- tract_demographics |>
  sf::st_drop_geometry() |>
  filter(!is.na(Number)) |>
  group_by(Number) |>
  summarise(
    median_household_income = weighted.mean(
      median_household_income,
      w = population,
      na.rm = TRUE
    ),
    population = sum(population, na.rm = TRUE),
    homeownership_rate = 100 * sum(owner_occupied_units, na.rm = TRUE) /
      sum(occupied_housing_units, na.rm = TRUE),
    black_share = 100 * sum(black_population, na.rm = TRUE) /
      sum(population, na.rm = TRUE),
    hispanic_share = 100 * sum(hispanic_population, na.rm = TRUE) /
      sum(population, na.rm = TRUE),
    .groups = "drop"
  )

submarket_demographics <- submarket_codes |>
  left_join(puma_demographics, by = "Number")

normalize_submarket_name <- function(name) {
  gsub("[^a-z0-9]", "", tolower(name))
}

submarket_index_lookup <- submarket_codes |>
  mutate(
    index_submarket = normalize_submarket_name(Submarket),
    index_submarket = recode(
      index_submarket,
      chicagoenglewoodgreatergrandcrossing = "chicagoenglewoodgreatergrandcros"
    )
  )

submarket_cycle_metrics <- price_indices |>
  inner_join(submarket_index_lookup, by = c("submarket" = "index_submarket")) |>
  group_by(Number, Submarket) |>
  group_modify(~ {
    series <- .x
    pre_2009_peak <- series |>
      filter(yearq < lubridate::yq("2009Q1")) |>
      slice_max(index, n = 1, with_ties = FALSE)
    post_2009_trough <- series |>
      filter(yearq >= lubridate::yq("2009Q1")) |>
      slice_min(index, n = 1, with_ties = FALSE)
    index_2022_q4 <- series |>
      filter(yearq == lubridate::yq("2022Q4")) |>
      pull(index)
    tibble::tibble(
      peak_to_trough_decline = 100 * (post_2009_trough$index[[1]] / pre_2009_peak$index[[1]] - 1),
      trough_to_2022_recovery = 100 * (index_2022_q4[[1]] / post_2009_trough$index[[1]] - 1)
    )
  }) |>
  ungroup()

submarket_cycle_demographics <- submarket_cycle_metrics |>
  left_join(submarket_demographics, by = c("Number", "Submarket"))

demographic_variables <- c(
  "median_household_income",
  "homeownership_rate",
  "black_share",
  "hispanic_share"
)
demographic_labels <- c(
  median_household_income = "Median household income (1999 $)",
  homeownership_rate = "Homeownership rate (%)",
  black_share = "Black alone share (%)",
  hispanic_share = "Hispanic/Latino share (%)"
)

demographic_scatter_data <- submarket_cycle_demographics |>
  tidyr::pivot_longer(
    cols = all_of(demographic_variables),
    names_to = "Characteristic",
    values_to = "Demographic value"
  ) |>
  tidyr::pivot_longer(
    cols = c(peak_to_trough_decline, trough_to_2022_recovery),
    names_to = "Cycle metric",
    values_to = "Index change"
  ) |>
  mutate(
    Characteristic = recode(Characteristic, !!!demographic_labels),
    `Cycle metric` = recode(
      `Cycle metric`,
      peak_to_trough_decline = "Peak-to-trough decline",
      trough_to_2022_recovery = "Trough to 2022 Q4 recovery"
    )
  )

demographic_scatter_plots <- list()
for (characteristic in unname(demographic_labels)) {
  plot_data <- demographic_scatter_data |>
    filter(Characteristic == characteristic)
  plot <- ggplot(
    plot_data,
    aes(x = `Demographic value`, y = `Index change`, color = `Cycle metric`)
  ) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.18) +
    scale_x_continuous(
      labels = if (characteristic == "Median household income (1999 $)") {
        scales::label_dollar()
      } else {
        scales::label_number()
      }
    ) +
    scale_color_manual(values = c(
      "Peak-to-trough decline" = "#D97732",
      "Trough to 2022 Q4 recovery" = "#176B87"
    )) +
    labs(
      title = paste("Housing Prices and", characteristic),
      x = characteristic,
      y = "Change in price index (%)",
      color = NULL
    ) +
    theme_minimal(base_family = "Times New Roman")
  plot_name <- names(demographic_labels)[match(characteristic, demographic_labels)]
  demographic_scatter_plots[[plot_name]] <- plot
  ggsave(
    paste0("answers/2.5_", plot_name, ".png"),
    plot = plot,
    device = ragg::agg_png
  )
}

decline_regression <- feols(
  peak_to_trough_decline ~ median_household_income + homeownership_rate + black_share + hispanic_share,
  data = submarket_cycle_demographics
)

decline_summary <- summary(decline_regression)
decline_coefficients <- decline_summary$coeftable |>
  as.data.frame() |>
  tibble::rownames_to_column("Term")
decline_fit <- fixest::fitstat(decline_regression, c("r2", "ar2", "rmse"))

new_word_table(decline_coefficients) |>
  add_regression_header(
    n = decline_summary$nobs,
    title = "OLS regression: Peak-to-trough decline",
    vcov_type = attr(decline_summary$coeftable, "vcov_type")
  ) |>
  add_regression_footer(fit_stats = c(
    "RMSE" = decline_fit["rmse"],
    "R-squared" = decline_fit["r2"],
    "Adjusted R-squared" = decline_fit["ar2"]
  )) |>
  format_regression_table() |>
  save_word_table("answers/2.5.docx")


# 2.7

submarket_sample_sizes <- readxl::read_excel(
  "data/2022_q4_price_index_data.xlsx",
  sheet = "Submarket_Annual_Sample_Size"
)

submarket_sales_plot <- submarket_sample_sizes |>
  rename(year = 1) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "submarket_code",
    values_to = "sales"
  ) |>
  group_by(submarket_code) |>
  mutate(submarket = first(sales)) |>
  ungroup() |>
  filter(!is.na(year), year != "Total") |>
  mutate(
    year = readr::parse_integer(year),
    sales = readr::parse_integer(sales)
  ) |>
  filter(submarket %in% c(
    "Evanston/Skokie",
    "Chicago--Uptown/Rogers Park",
    "Chicago--Bronzeville/Hyde Park",
    "Palatine/Barrington",
    "Calumet City/Harvey"
  )) |>
  ggplot(aes(x = year, y = sales, color = submarket, group = submarket)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Sales per Year",
    x = "Year",
    y = "Number of Sales",
    color = "Submarket"
  ) +
  theme_minimal(base_family = "Times New Roman")

ggsave("answers/2.7.png", plot = submarket_sales_plot, device = ragg::agg_png)
