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

