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
