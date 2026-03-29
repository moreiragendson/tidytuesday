# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)
library(waffle)
library(countrycode)


# Load data --------------------------------------------------------------

repairs <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv'
)


# Data wrangling ---------------------------------------------------------


repairs_summary <- repairs |> 
  mutate(
    continent = countrycode(
      country,
      origin = "iso2c",
      destination = "continent"
    ),

    is_repaired = case_when(
      str_detect(str_to_lower(repaired), "ja") ~ as.character(NA),
      is.na(repaired) ~ as.character(NA),
      TRUE ~ repaired
    )
  ) |> 
  filter(!is.na(continent), !is.na(is_repaired)) |> 
  count(continent, is_repaired) |> 
  mutate(thousands = round(n/1e3)) |> 
  filter(thousands >= 1)


# Define colors and fonts ------------------------------------------------

font_add_google("Source Sans 3", "source")
font_add_google("Fira Sans", "fira")

title_font <- "fira"
body_font <- "source"

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

bg_color <- "#e8dccd"


custom_pal <- c(
  "yes" = "#003f5c" -> yes,
  "half" = "#bc5090" -> half,
  "no" = "#ffa600" -> no
)


# Define chart texts -----------------------------------------------------

title_txt <- "**A World Trying to Fix Things**"

st_txt <- stringr::str_c(
  "Since 2015, Repair Monitor has tracked every item brought to volunteer-run Repair Cafes across the globe. Each square = 1K items, colored by outcome. Africa excluded (< 1,000 total records).",
  "<br><br>",
  "<span style='color:",
  yes,
  ";'><b>**■ repaired**</b></span>    ",
  "<span style='color:",
  half,
  ";'><b>**■ partially fixed**</b></span>   ",
  "<span style='color:",
  no,
  ";'><b>**■ not repaired**</b></span>"
)

caption_txt <- "
TidyTuesday 2026 Week 14 &mdash; Repair Cafes Worldwide
<br>
Created by Gendson Moreira
"

# Set theme --------------------------------------------------------------


base_theme <- theme_minimal(base_family = body_font, base_size = 8) +
  theme(

     # Chart background
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    legend.background = element_rect(fill = bg_color, color = NA),

    # Chart texts
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      size = rel(1.4),
      family = title_font,
      halign = 0.5,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(0.9),
      halign = 0.5,
      margin = margin(b = 5)
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.8),
      halign = 0.5,
      margin = margin(t = 10, b = 2)
    ),

    # Legend
    legend.position = "none",

    # Axis
    axis.text = element_blank(),

    # Gridlines
    panel.grid = element_blank()
  )

ggplot2::set_theme(base_theme)


# Build chart ------------------------------------------------------------

# Inspiration:
# https://rud.is/rpubs/building-waffle-charts.html

plt <- ggplot(repairs_summary, aes(values = thousands, fill = is_repaired))+
  waffle::geom_waffle(
    radius = unit(1, "pt"),
    make_proportional = FALSE,
    color = bg_color,
    n_rows = 10,
    flip = TRUE,
    size = 0.25
  ) + 
  facet_wrap(
    ~ fct_reorder(continent, thousands, .fun = sum, .desc = TRUE),
     nrow = 1,
    strip.position = "bottom") + 
  scale_fill_manual(values = custom_pal) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

fig <- plt + ggview::canvas(width = 4.1, height = 3.5)
fig


# Define filenames -------------------------------------------------------

tuesday <- "2026-04-07"
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

