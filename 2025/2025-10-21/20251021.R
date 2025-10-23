
# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(lubridate)
library(ggtext)


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")

# Define filenames -------------------------------------------------------

tuesday <- "2025-10-21"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Load data --------------------------------------------------------------

# tuesdata <- tidytuesdayR::tt_load(tuesday)

historic_station_met <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv')
station_meta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/station_meta.csv')


# Explore data -----------------------------------------------------------

indicators <- c("tmax", "tmin", "af", "rain", "sun")

df <- historic_station_met |> 
  mutate(
    date = make_date(year = year, month = month, day = 1)) |> 
  pivot_longer(cols = all_of(indicators))


# Define colors and fonts ------------------------------------------------

txt_color <- "#f5f5f5"
bg_color <- "#0e0c0d"
txt_font <- "lato"


# Define chart texts -----------------------------------------------------

title_txt <- "Sunniest Month Revealed:\nMay Shines Brightest in Oxford"
st_txt <- "This heatmap shows monthly sunshine totals for Oxford from 2000 to 2020.\nEach row represents a different year, and each column a month.\nThe colour intensity of each tile reflects the total hours of sunshine in that year and month."
legend_txt <- "sunshine\nhours"


# Build chart ------------------------------------------------------------

segment_data <- data.frame(
      y = seq(2000, 2020, by = 5),
      xmin = 1,
      xmax = 0
    )

p <- df |>
  filter(
    station == "oxford" 
      & !is.na(value) 
      & name == "sun" 
      & year %in% 2000:2020 ) |>
  ggplot() +
  geom_segment(
    data = segment_data,
    aes(x = xmin, xend = xmax, y = y),
    color = txt_color,
    linewidth = 0.1
  ) +
  geom_point(
    data = segment_data,
    aes(x = xmax, y = y),
    color = txt_color
  ) +
  geom_tile(
    aes(x = month, y = year, fill = value),
    linewidth = 0.5,
    colour = NA,
    lineend = "round",
    linejoin = "round"
  ) +
  scale_x_continuous(
    breaks = seq(1, 12, 1),
    labels = month.abb
  ) +
  scale_y_reverse(
    breaks = seq(2000, 2020, 5),
    labels = seq(2000, 2020, 5)
  ) +
  scale_fill_viridis_c(
    option = "cividis",
    trans = "sqrt",
    name = legend_txt
  ) +
  labs(
    title = title_txt,
    subtitle = st_txt
  )+
  theme_minimal(base_family = txt_font) +
  theme(
    panel.grid = element_blank(),
    plot.background  = element_rect(fill = bg_color),
    axis.title = element_blank(),

    # Legend styling
    legend.text = element_text(color = txt_color),
    legend.title = element_text(color = txt_color, face = "bold", size = 12, vjust = 0.7),
    legend.position = "top",
    legend.key.height = unit(0.7, "cm"),
    legend.key.width = unit(3, "cm"),
    legend.margin = margin(t = 15, b = 20),

    # Axis styling
    axis.text.y = element_text(face = "bold", size = 16, color = txt_color),
    axis.text.x = element_text(face = "bold", size = 15, color = txt_color),

    # Title styling
    plot.title = element_textbox(
      size = 24,
      face = "bold",
      color = txt_color,
      hjust = 0,
      margin = margin(t = 20, b = 10, l = 30, r = 30)
    ),

    plot.subtitle = element_textbox_simple(
      size = 16,
      colour = txt_color,
      hjust = 0,
      margin = margin(b = 30, l = 30, r = 30),
      width = unit(0.9, "npc")
    ),

    # Using new margin system from ggplot2 4.0
    plot.margin = margin(30, 20, 30, 20)

  )


# Save chart -------------------------------------------------------------
p

ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 11,
  height = 13,
  units = "in",
  dpi = 300
)
