

# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)

# Load data --------------------------------------------------------------

absolute_judgements <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv'
)
pairwise_comparisons <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/pairwise_comparisons.csv'
)
respondent_metadata <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/respondent_metadata.csv'
)


# Data wrangling ---------------------------------------------------------

plot_data_distinct <- absolute_judgements %>%
  inner_join(respondent_metadata, by = "response_id") %>%
  filter(age_band %in% c("Under 18", "18-24")) %>%
  group_by(term, age_band) %>%
  summarize(med_prob = median(probability, na.rm = TRUE), .groups = "drop") %>%
  group_by(term) %>%
  filter(n_distinct(med_prob) >= 2) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, med_prob, .fun = median))


# Load fonts -------------------------------------------------------------

font_add_google("Inter")


# Define colors and fonts ------------------------------------------------

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)


custom_pal <- c(
  "Under 18" = "#1696d2",
  "18-24"    = "#ec008b")


txt_color <- "#100C08"
bg_color <- "#E0E0E0"

body_font <- "Inter"
title_font <- "Inter"

# Set theme --------------------------------------------------------------

t <- theme_minimal(base_family = body_font, base_size = 8) +
  theme(
    plot.title.position = "plot", 
    plot.caption.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      size = rel(1.4),
      family = title_font,
      margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(0.9),
      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.8),
      hjust = -2,
      halign = 0,
      lineheight = 1.25,
      colour = "grey30",
      margin = margin(t = 10, r = 0, b = 2, l = 0, unit = "pt")
    ),
    axis.text = element_text(size = rel(0.9), face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggplot2::set_theme(t)


# Define chart texts -----------------------------------------------------

title_txt <- "**'Likely' is interpreted as more likely among the <span style='color:#1696d2;'>youngest</span> people**"

st_txt <- "Comparison of median numerical interpretations for probability phrases among people <span style='color:#1696d2;'>**under 18**</span> and <span style='color:#ec008b;'>**between 18 and 24**</span> years."

caption_txt <- "
**Data**: TidyTuesday 2026 Week 10 &mdash; How likely is 'likely'?
  <br>
**Graphic**: Gendson Moreira"


# Build chart ------------------------------------------------------------

p <- plot_data_distinct |>
  ggplot(aes(x = med_prob, y = term)) +
  geom_vline(xintercept = c(0, 25, 50, 75, 100), linetype = "dashed", color = "gray80", size = 0.5) +
  geom_line(aes(group = term), color = "gray80", size = 0.5) +
  geom_point(aes(color = age_band), size = 2.5) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_color_manual(values = custom_pal) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

 fig <- p + ggview::canvas(width = 5, height = 6)


# Define filenames -------------------------------------------------------

tuesday <- "2026-03-10"
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
