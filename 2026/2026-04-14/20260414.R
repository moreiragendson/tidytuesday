# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(forcats)

# Load data --------------------------------------------------------------

beaufort_scale <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv'
)
ships <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv'
)

# Data wrangling ---------------------------------------------------------


wind_data <- ships %>%
  left_join(beaufort_scale, by = "wind_speed_class") %>%
  filter(!is.na(wind_direction), !is.na(wind_speed_class)) %>%
  mutate(
    wind_description = fct_reorder(
      case_when(
        wind_description %in% c("calm", "light air") ~ "Calm",
        wind_description %in% c("light breeze", "gentle breeze") ~ "Light",
        wind_description %in% c("moderate breeze", "fresh breeze") ~ "Moderate",
        wind_description %in% c("strong breeze", "near gale") ~ "Strong",
        wind_description %in% c("gale", "strong gale", "storm", "violent storm", "hurricane") ~ "Severe",
        TRUE ~ NA_character_
      ),
      wind_speed_class
    ),
    # Shift by 11.25 degrees so North is centered
    dir_binned = cut(
      (wind_direction + 11.25) %% 360,
      breaks = seq(0, 360, by = 22.5),
      labels = c(
        "N",
        "NNE",
        "NE",
        "ENE",
        "E",
        "ESE",
        "SE",
        "SSE",
        "S",
        "SSW",
        "SW",
        "WSW",
        "W",
        "WNW",
        "NW",
        "NNW"
      ),
      include.lowest = TRUE
    )
  )

summary_data <- wind_data %>%
  count(dir_binned, wind_description) %>%
  mutate(
    pct_total = n / sum(n) * 100
  )

# Define colors and fonts ------------------------------------------------

# Inspiration
# https://windy.app/blog/how-to-read-wind-rose.html

custom_pal <- c(
  "Calm" = "#E6DFC3" -> calm,
  "Light" = "#B6D3C3" -> ligth,
  "Moderate" = "#7FBAB2" -> moderate,
  "Strong" = "#2E8586" -> strong,
  "Severe" = "#033A3A" -> severe
)


font_add_google("Source Sans 3", "source")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

txt_color <- "#100C08"
bg_color <- "#E0E0E0"

title_font <- body_font <- "source"

# Set theme --------------------------------------------------------------

base_theme <- theme_minimal(base_family = body_font, base_size = 8) +
  theme(
    # Chart texts
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      size = rel(1.4),
      family = title_font,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(0.9),
      margin = margin(t = 0, b = 10)
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.8),
      halign = 0.5,
      margin = margin(t = 5, b = 5)
    ),

    # Axis
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(1.1), face = "bold"),
    axis.title = element_blank(),

    # Legend
    legend.position = "none",

    # Gridlines
    panel.grid.major = element_line(color = "#D0D7D8", linewidth = 0.075),
  )

ggplot2::set_theme(base_theme)


# Define chart texts -----------------------------------------------------

title_txt <- "**Seabirds in the Wind**"

st_txt <- stringr::str_c(
  "Distribution of wind direction and intensity during seabird observation periods near New Zealand (1969–1990)",
  "<br><br>",
  "<span style='color:",
  calm,
  ";'><b>■ calm</b></span>    ",
  "<span style='color:",
  ligth,
  ";'><b>■ ligth</b></span>   ",
  "<span style='color:",
  moderate,
  ";'><b>■ moderate</b></span>    ",
  "<span style='color:",
  strong,
  ";'><b>■ strong</b></span>    ",
  "<span style='color:",
  severe,
  ";'><b>■ severe</b></span>"
)

caption_txt <- "
TidyTuesday 2026 Week 15 &mdash; Bird Sightings at Sea<br><br>
**Source**: Museum of New Zealand Te Papa Tongarewa
<br>
**Graphic**: Gendson Moreira
"


# Build chart ------------------------------------------------------------

plt <- summary_data |>
  ggplot(aes(x = dir_binned, y = pct_total, fill = wind_description)) +
  geom_col() +
  coord_polar(start = -pi / 16) + # slight rotation = nicer alignment
  scale_fill_manual(values = custom_pal) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )


fig <- plt + ggview::canvas(width = 3.75, height = 5)
fig

# Define filenames -------------------------------------------------------

tuesday <- "2026-04-14"
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