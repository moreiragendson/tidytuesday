
# Load libraries ---------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)

# Load data --------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-30")
cranes <- tuesdata$cranes

# Option 2: Read directly from GitHub

# cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")

# Define colors and fonts ------------------------------------------------

bg_color <- "#ffffff"
txt_color <- "#001219"
color_one <- "#1696d2"
color_two <- "#001219"
color_three <- "#d2d2d2"
txt_font <- "lato"
title_font_size <- 16
st_font_size <- 12

# Data wrangling ---------------------------------------------------------

glimpse(cranes)

grouped_data <- cranes |> 
  mutate(
    year = year(date),
    day_of_year = yday(date),
    month = month(date, label = TRUE, abbr = FALSE),
    year_month = floor_date(date, "month"),
    season = case_when(
      month(date) %in% 3:5 ~ "Spring",
      month(date) %in% 9:11 ~ "Fall",
      TRUE ~ "Other"),
    season = factor(season, levels = c("Spring", "Fall", "Other"))
) |> 
  group_by(year, season) |> 
  summarise(count = sum(observations, na.rm = TRUE))



# Set scales -------------------------------------------------------------


min_year <- grouped_data |> pull(year) |> min()
max_year <- grouped_data |> pull(year) |> max()
max_count <- grouped_data |> pull(count) |> max()


# Set texts --------------------------------------------------------------


title_txt <- "Crane Observations at Lake Hornborgasjön, Sweden (1994–2024)"
st_txt <- "For more than 30 years, cranes stopping at the Lake Hornborgasjön ('Lake Hornborga') in Västergötland, Sweden have been \ncounted from the Hornborgasjön field station in the spring and the fall as they pass by during their yearly migration.\nIn 2019, more than 500 thousands cranes stopped at the Lake Hornborgasjön."
caption_txt <- "\nData: Transtatistik ('crane statistics'), Naturum, Hornborgasjön, Västra Götaland County Administrative Board"


# Chart ------------------------------------------------------------------

final_plt <- grouped_data |>
  ggplot() +
  geom_area(aes(x = year, y = count, fill = season)) +
  scale_x_continuous(breaks = seq(min_year, max_year, 5)) +
  scale_fill_manual(values = c("Spring" = color_one, "Fall" = color_two, "Other" = color_three))+
  scale_y_continuous(limits = c(0,6e5))+
  theme_minimal(base_family = txt_font) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  ) +
  theme(
    plot.title = element_text(face = "bold", color = txt_color, size = title_font_size),
    plot.subtitle = element_text(face = "italic", color = txt_color, size = st_font_size),
    plot.caption = element_text(color = txt_color),
    plot.caption.position = "plot",
    legend.position = "top",
    legend.text = element_text(color = txt_color),
    legend.title = element_blank(),
    plot.background = element_rect(fill = bg_color),
    axis.text.x = element_text(
      family = txt_font,
      face = "bold",
      color = txt_color
    ),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )

final_plt

ggsave(
  filename = "2025/2025-09-30/20250930.png",
  plot = final_plt,
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600      # Use 300 to 600 for maximum quality/print
)
