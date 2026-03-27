# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)


# Load data --------------------------------------------------------------


ocean_temperature <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv'
)

ocean_temperature_deployments <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature_deployments.csv'
)


# Data wrangling ---------------------------------------------------------

Sys.setlocale("LC_TIME", "en_US.UTF-8")
ocean_temperature_clean <- ocean_temperature %>%
  filter(!is.na(mean_temperature_degree_c)) %>%
  mutate(
    # Create month in English (safe fallback if locale fails)
    month = factor(month(date, label = TRUE, abbr = TRUE),
      levels = c(
        "Mar", "Apr", "May",   # Spring
        "Jun", "Jul", "Aug",   # Summer
        "Sep", "Oct", "Nov",   # Autumn
        "Dec", "Jan", "Feb"    # Winter
      )
    ),
    
    # Create season
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
    ),
    
    # Order seasons for legend consistency
    season = factor(
      season,
      levels = c("Spring", "Summer", "Autumn", "Winter")
    )
  )

# Define colors and fonts ------------------------------------------------

font_add_google("Source Sans 3", "source")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)


title_font <- body_font <- "source"


txt_color <- "#E0E0E0"
bg_color <- "#0B1D2A"

custom_pal <- c(
  "Winter" = "#4EA8DE" -> winter,
  "Spring" = "#72EFDD" -> spring,
  "Summer" = "#FFD166" -> summer,
  "Autumn" = "#F78C6B" -> autumn
)


# Set theme --------------------------------------------------------------

t <- theme_minimal(base_family = body_font, base_size = 8) +
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
      size = rel(1.3),
      family = title_font,
      margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(0.9),
      margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.8),
      hjust = -2,
      halign = 0.5,
      lineheight = 1.25,
      margin = margin(t = 10, r = 0, b = 2, l = 0, unit = "pt")
    ),

    # Grid
    panel.grid = element_blank(),

    # Legend
    legend.position = "none",

    # Axis
    axis.title = element_blank(),
    axis.text = element_text(color = txt_color)
  )

ggplot2::set_theme(t)


# Define chart texts -----------------------------------------------------

title_txt <- "**Seasonal Spread of Coastal Ocean Temperatures**"

st_txt <- paste0(
  "Daily mean temperatures (°C) recorded at a fixed coastal site in Nova Scotia over seven years. ",
  
  "Distributions widen during <span style='color:", summer, ";'><b>summer</b></span>, ",
  "indicating greater thermal variability, while ",
  
  "<span style='color:", winter, ";'><b>winter</b></span> months remain tightly constrained. White bars mark medians.<br>"
)
caption_txt <- paste0(
  "<span style='color:",
  spring,
  ";'><b>● spring</b></span>   ",
  "<span style='color:",
  summer,
  ";'><b>● summer</b></span>   ",
  "<span style='color:",
  autumn,
  ";'><b>● autumn</b></span>   ",
  "<span style='color:",
  winter,
  ";'><b>● winter</b></span>",
  "<br><br>TidyTuesday 2026 Week 13 &mdash; Coastal Ocean Temperature by Depth<br>
**Data**: Lunenburg County Water Quality Data<br>
**Created by**: Gendson Moreira
"
)


# Build chart ------------------------------------------------------------

p <- ggplot(ocean_temperature_clean,
       aes(x = mean_temperature_degree_c, y = month, color = season)) +
  geom_jitter(height = 0.2, width = 0, alpha = 0.2, size = 0.3) +
  scale_color_manual(values = custom_pal) +
  scale_y_discrete(limits = rev) + 
  stat_summary(
    fun = median,
    fun.min = median,
    fun.max = median,
    geom = "crossbar",
    width = 0.5,
    color = "white",
    linewidth = 0.4
  ) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

fig <- p + ggview::canvas(width = 4, height = 5)
fig

# Define filenames -------------------------------------------------------

tuesday <- "2026-03-31"
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
