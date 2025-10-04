
# Load libraries ---------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)

# Load data --------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-30")
cranes <- tuesdata$cranes

# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")

# Define colors and fonts ------------------------------------------------

bg_color <- "#001219"
txt_color <- "#ffffff"
color_one <- "#fdbf11"
color_two <- "#1696d2"
txt_font <- "lato"

# Data wrangling ---------------------------------------------------------

glimpse(cranes)

grouped_data <- cranes |> 
  mutate(
    year = year(date),
    day_of_year = yday(date),
    month = month(date, label = TRUE, abbr = FALSE),
    year_month = floor_date(date, "month")
) |> 
  group_by(year) |> 
  summarise(count = sum(observations, na.rm = TRUE))


# Scales and texts -------------------------------------------------------

min_year <- grouped_data |> pull(year) |> min()
max_year <- grouped_data |> pull(year) |> max()

max_count <- grouped_data |> pull(count) |> max()

title_txt <- "Crane Observations at Lake Hornborgasjön, Sweden (1994–2024)"
st_txt <- "For more than 30 years, cranes stopping at the Lake Hornborgasjön ('Lake Hornborga') in Västergötland, Sweden have been \ncounted from the Hornborgasjön field station in the spring and the fall as they pass by during their yearly migration.\nIn 2019, more than 500 thousands cranes stopped at the Lake Hornborgasjön."
caption_txt <- "\nData: Transtatistik ('crane statistics'), Naturum, Hornborgasjön, Västra Götaland County Administrative Board"


# Chart ------------------------------------------------------------------

plt_data <- grouped_data |>
  mutate(
    highest_value = case_when(
      count == max_count ~ as.double(count),
      TRUE ~ as.double(NA)
    ),
    highest_value_in = case_when(
      count == max_count ~ as.double(year),
      TRUE ~ as.double(NA)
    ),
    cat = ""
  )


ref_year <- plt_data |> pull(highest_value_in) |> max(na.rm = TRUE)
ref_value <- plt_data |> pull(highest_value) |> max(na.rm = TRUE)

final_plt <- plt_data |>
  ggplot() +
  geom_area(aes(x = year, y = count), fill = color_two) +
  scale_x_continuous(breaks = seq(min_year, max_year, 5)) +
  scale_y_continuous(limits = c(0,6e5))+
  theme_minimal(base_family = txt_color) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  ) +
  theme(
    plot.title = element_text(face = "bold", color = txt_color, size = 16),
    plot.subtitle = element_text(face = "plain", color = txt_color, size = 12),
    plot.caption = element_text(color = txt_color),
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_color),
    axis.text.x = element_text(
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