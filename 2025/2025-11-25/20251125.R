# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(marquee)
library(scales)
library(tidyr)
library(ggbump)


# Load data --------------------------------------------------------------

spi_indicators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')


# Prepare data -----------------------------------------------------------

pivoted_data <- spi_indicators |> 
  filter(year >= 2016) |> 
  pivot_longer(
    cols = contains("score"),
    names_to = "metric",
    values_to = "score"
  ) |> 
  filter(!is.na(score)) |> 
  group_by(year, metric) |> 
  mutate(rank = row_number(desc(score))) |> 
  ungroup() |> 
  mutate(
    metric = recode(metric,
    overall_score = "Overall",
    data_use_score = "Data Use",
    data_services_score = "Data Services",
    data_products_score = "Data Products",
    data_sources_score = "Data Sources",
    data_infrastructure_score = "Data Infrastructure"
  ),
    metric = factor(metric, levels = unique(metric)),
    is_brazil = ifelse(country=="Brazil", TRUE, FALSE),
    score = round(score, 0),
    label_position = case_when(
      year == min(year) ~ year - 1/5,
      year == max(year) ~ year + 1/5,
    TRUE ~ year)) |> 
  filter(metric == "Overall")


brazil_data <- pivoted_data |> filter(is_brazil == TRUE)
brazil_lim_data <- brazil_data |> filter(year %in% c(max(year), min(year)))


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")
font_add_google("Noto Serif", "noto_serif")


# Define colors and fonts ------------------------------------------------

txt_color <- "#000000"
bg_color <- "#f2f2f2"
brazil_color <- "#55b748"
other_color <- "#d2d2d2"
txt_font <- "lato"


# Define texts -----------------------------------------------------------

title_txt <- "
**Overall Statistical Performance Indicators for <span style='color:#55b748;'>Brazil</span> (2016–2023)**
"

st_txt <- "
*Reliable, usable, high-quality statistics are vital for global prosperity and progress. The Statistical Performance Indicators (SPI) provide an open-source framework for assessing the performance of statistical systems and the efforts to improve them. The SPI focuses on five key dimensions of a country’s statistical performance: (i) data use, (ii) data services, (iii) data products, (iv) data sources, and (v) data infrastructure.*
"

caption_txt <- "
**Data**: World Bank
<br>
**Graphic**: Gendson Moreira
"


# Set theme --------------------------------------------------------------

t <- theme_minimal(base_family = txt_font) + 
  theme(

    plot.title = element_textbox_simple(
      size = 16,
      color = txt_color,
      padding = margin(3, 0, 0, 0),
      margin = margin(3, 0, 0, 0)
    ),

    plot.subtitle = element_textbox_simple(
      size = 12,
      color = txt_color,
      lineheight = 1.2,
      padding = margin(7, 0, 7, 0),
      margin = margin(7, 0, 7, 0)
    ),

    plot.caption = element_textbox_simple(
      color = txt_color,
      size = 12,
      lineheight = 1.3,
      padding = margin(7, 0, 3, 0),
      margin = margin(7, 0, 3, 0)
    ),

    # Axis
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),

    # Panel
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg_color)
)

ggplot2::set_theme(t)


# Build chart ------------------------------------------------------------


p <- ggplot() +
  geom_line(
    data = pivoted_data,
    aes(x = year, y = score, group = country),
    alpha = 0.4, color = other_color
  ) +
  geom_line(
    data = brazil_data,
    aes(x = year, y = score, group = country),
    color = brazil_color,
    size = 1
  ) + 
  geom_point(
    data = brazil_lim_data,
    aes(x = year, y = score, group = country),
    color = brazil_color
  ) +
  geom_text(
    data = brazil_lim_data,
    fontface = "bold",
    size = 5,
    aes(x = label_position, y = score, group = country, label = score),
    color = brazil_color
  ) +
  scale_x_continuous(
    breaks = year_seq <- seq(min(pivoted_data$year), max(pivoted_data$year), 1),
    labels = year_seq
  ) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

p

# Define filenames -------------------------------------------------------


tuesday <- "2025-11-25"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Save chart -------------------------------------------------------------


ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600      # Use 300 to 600 for maximum quality/print
)
