# Load libraries ---------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(skimr)
library(treemapify)
library(janitor)


# Load data --------------------------------------------------------------

# tuesdata <- tidytuesdayR::tt_load("2025-10-07")
# euroleague_basketball <- tuesdata$euroleague_basketball

# Option 2: Read directly from GitHub

euroleague_basketball <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv') |> 
  janitor::clean_names()


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")

# Define colors and fonts ------------------------------------------------

bg_color <- "#ffffff"
txt_color <- "#001219"
color_one <- "#12719e"
txt_font <- "lato"
title_font_size <- 16
st_font_size <- 12


# Chart texts ------------------------------------------------------------

title_txt <- "Where the Titles Live"
st_txt <- "EuroLeague Basketball Wins by Country and City"
caption_txt <- "\nData: EuroleagueBasketball R package (curated from Wikipedia and EuroLeague records)"


# Chart ------------------------------------------------------------------


plt <- euroleague_basketball |>
  group_by(country, home_city) |>
  summarise(titles_won = sum(titles_won, na.rm = TRUE)) |>
  ggplot(aes(area = titles_won, subgroup = country)) +
  geom_treemap(radius = unit(0.2, "line"), color = "white", size = 2, fill = color_one) +
  geom_treemap_text(aes(label = paste0(home_city, ": ", titles_won)), color = bg_color) +
  geom_treemap_subgroup_text(
    color = bg_color,
    grow = TRUE,
    alpha = 0.25
  ) +
  geom_treemap_subgroup_border(
    color = bg_color
  )+
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )+
  theme_bw(base_family = txt_font) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = bg_color),
    title = element_text(face = "bold", size = title_font_size),
    plot.subtitle = element_text(face = "italic", size = st_font_size)
  )

plt


# Save chart -------------------------------------------------------------


ggsave(
  filename = "2025/2025-10-07/20251007.png",
  plot = plt,
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600      # Use 300 to 600 for maximum quality/print
)
