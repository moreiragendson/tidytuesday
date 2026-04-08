# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(countrycode)
library(testthat)
library(here)

# Load custom functions --------------------------------------------------

testthat::test_file(
  here::here("2026/2026-04-21/test_rescale_number.R"),
  stop_on_failure = TRUE
)
source(here::here("2026/2026-04-21/rescale_number.R"))


# Load data --------------------------------------------------------------

financing_schemes <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/financing_schemes.csv'
)
health_spending <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/health_spending.csv'
)
spending_purpose <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/spending_purpose.csv'
)


# Define colors and fonts ------------------------------------------------

font_add_google("Fira Sans", "fira")
font_add_google("Source Sans 3", "source")

title_font <- "fira"
body_font <- "source"

showtext_auto()
showtext_opts(dpi = 300)


txt_color <- "#ffffff"
bg_color <- "#062635"

custom_pal <- c(
  "country_value" = country_col <- "#fdbf11",
  "rest_of_the_region" = region_col <- "#0a4c6a"
)

# Data wrangling ---------------------------------------------------------

sp_data <- spending_purpose |>
  filter(
    year == 2023 &
    indicator_code == "hc6_usd2023" # Preventive care constant 2023 US$
  ) |> 
  mutate(
    region = countrycode(country_name, origin = "country.name", destination = "region")
  )

country_data <- sp_data |>
  group_by(region, country_name) |> 
  summarise(country_value = sum(value, na.rm = TRUE)) |> 
  ungroup()

world_data <- sp_data |>
  summarise(world_value = sum(value, na.rm = TRUE))

region_data <- sp_data |> 
  group_by(region) |> 
  summarise(region_value = sum(value, na.rm = TRUE), .groups = "drop")

plot_data <- country_data |> 
  mutate(
    country_name = case_when(
      country_name == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country_name == "Netherlands (Kingdom of the)" ~ "Netherlands",
      TRUE ~ country_name
    )) |> 
  left_join(region_data, by = "region") |> 
  cross_join(world_data) |> 
  mutate(
    rest_of_the_region = region_value - country_value,
    rest_of_the_world = world_value - rest_of_the_region - country_value
  ) |> 
  pivot_longer(
    cols = c(country_value, rest_of_the_region, rest_of_the_world),
    names_to = "hierarchy",
    values_to = "value"
  ) |>
  mutate(
    hierarchy = factor(hierarchy, levels = c("country_value", "rest_of_the_region", "rest_of_the_world"))
  ) |> filter(
    region == "Europe & Central Asia",
    hierarchy != "rest_of_the_world"
  ) |> 
  rescale_number(value_fmt = value) |> 
  mutate(
    label = case_when(
      hierarchy != "country_value" ~ "",
      value_fmt_suffix == "B"      ~ str_c(sprintf("%.2f", value_fmt), value_fmt_suffix),
      TRUE                         ~ str_c(round(value_fmt), value_fmt_suffix)
    ),
    text_position_x = region_value + 5e9,
    country_name_position = ifelse(hierarchy == "country_value", region_value - value - 1e9, as.numeric(NA))
  )


# Define chart texts -----------------------------------------------------

title_txt <- glue(
  "**Germany alone accounts for over a quarter of Europe & Central Asia's preventive care spending**"
)

st_txt <- glue(
  "The region spent $91.4B on preventive care in 2023 — Germany ($25.53B) and the UK ($20.6B) together represent more than half.",
   "Bars show each country's spending; the remaining length represents the rest of the region."
)

caption_txt <- glue(
  "TidyTuesday 2026 Week 16 &mdash; Global Health Spending",
  "<br>",
  "Data: WHO Global Health Expenditure Database (GHED)",
  "<br>",
  "Viz & Design: Gendson Moreira"
)

# Set theme --------------------------------------------------------------

base_theme <- theme_minimal(base_family = body_font, base_size = 8) +
  theme(
    # Chart background
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    legend.background = element_rect(fill = bg_color, color = NA),

    # Chart texts
    text = element_text(color = txt_color),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      size = rel(1.5),
      family = title_font,
      hjust = -2,
      margin = margin(t = 5, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(1.2),
      margin = margin(t = 10, b = 10, unit = "pt")
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.9),
      halign = 0.5,
      margin = margin(t = 10,b = 2, unit = "pt")
    ),

    # Margin
    plot.margin = margin(t = 10, r = 10, b = 10, l = 15, unit = "pt"),

    # Grid
    panel.grid = element_blank(),

    # Legend
    legend.position = "none",

    # Axis
    axis.title = element_blank(),
    axis.text = element_blank()
  )

ggplot2::set_theme(base_theme)

# Build chart ------------------------------------------------------------

region_data |> 
  rescale_number(region_value)

plt <- ggplot(plot_data, aes(x = value, y = reorder(country_name, value * (hierarchy == "country_value")),
    fill = hierarchy)) +
  geom_col() +
  geom_text(aes(x = text_position_x, label = label), hjust = 0.5, color = country_col, fontface = "bold") +
  geom_text(
    aes(x = country_name_position,
      label = country_name),
    hjust = 0, color = country_col, fontface = "bold") +
  scale_fill_manual(values = custom_pal) + 
  scale_x_reverse() +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

fig <- plt + ggview::canvas(width = 5.5, height = 10)
fig


# Define filenames -------------------------------------------------------

tuesday <- "2026-04-21"
tuesyear <- stringr::str_extract(
  tuesday,
  pattern = stringr::regex("\\d{4}(?=-)")
)
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Save chart -------------------------------------------------------------

ggview::save_ggplot(
  file = plt_file_name,
  plot = fig
)
