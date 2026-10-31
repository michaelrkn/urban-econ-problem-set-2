library(readr)
library(lubridate)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)


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
