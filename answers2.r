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
		format_number <- function(value, digits = 1) {
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

flextable(quarter_index_summary) |>
	theme_booktabs() |>
	set_header_labels(
		Metric = "",
		cook = "Cook",
		evanstonskokie = "Evanston/Skokie",
		chicagouptownrogerspark = "Uptown/Roger's Park",
		chicagobronzevillehydepark = "Bronzeville/Hyde Park",
		palatinebarrington = "Palatine/Barrington",
		calumetcityharvey = "Calumet City/Harvey"
	) |>
	font(fontname = "Times New Roman", part = "all") |>
	fontsize(size = 10, part = "all") |>
	fontsize(size = 9, part = "footer") |>
	align(j = "Metric", align = "left", part = "all") |>
	align(j = target_submarkets, align = "center", part = "all") |>
	autofit() |>
	save_as_docx(path = "answers/2.3.docx")
