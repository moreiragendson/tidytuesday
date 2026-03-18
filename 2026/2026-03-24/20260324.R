# Load libraries ---------------------------------------------------------


library(tidyverse)
library(showtext)
library(ggtext)
library(testthat)


# Load data --------------------------------------------------------------


pi_digits <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-24/pi_digits.csv"
)


# Data wrangling ---------------------------------------------------------


test_passed <- test_that("rle correctly detects runs", {
  test_pi <- tibble(
    digit_position = 1:10,
    digit = c(3, 1, 4, 1, 5, 5, 5, 2, 2, 9)
  )

  runs <- rle(test_pi$digit)

  expect_equal(runs$values, c(3, 1, 4, 1, 5, 2, 9))
  expect_equal(runs$lengths, c(1, 1, 1, 1, 3, 2, 1))
})

if (test_passed) {
  runs <- rle(pi_digits$digit)
}

run_df <- tibble(
  digit = runs$values,
  run_length = runs$lengths
)

# count occurrences
run_counts <- run_df %>%
  count(digit, run_length) |>
  mutate(
    n_formatted = case_when(
      n >= 1e3 ~ n / 1e3,
      TRUE ~ n
    ),
    n_suffix = case_when(
      n >= 1e3 ~ "K",
      TRUE ~ ""
    ),
    label = stringr::str_c(round(n_formatted, 0), n_suffix)
  )


# Define colors and fonts ------------------------------------------------


font_add_google("Fira Sans")
font_add_google("Open Sans")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)


title_font <- "Fira Sans"
body_font <- "Open Sans"

dot_color <- "#5c5859"
streak_color <- "#db2b27"


# Set theme --------------------------------------------------------------


t <- theme_minimal(base_family = body_font, base_size = 8) +
  theme(
    # Chart texts
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      size = rel(1.4),
      family = title_font,
      margin = margin(t = 5, r = 0, b = 5, l = 0, unit = "pt")
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = rel(1),
      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    ),
    plot.caption = ggtext::element_textbox_simple(
      size = rel(0.8),
      hjust = -2,
      halign = 0.5,
      lineheight = 1.25,
      colour = "grey30",
      margin = margin(t = 10, r = 0, b = 2, l = 0, unit = "pt")
    ),

    # Legend
    legend.position = "none",

    # Axis
    axis.text.y = element_text(size = rel(1)),
    axis.text.x = element_text(size = rel(1.1)),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggplot2::set_theme(t)


# Define chart texts -----------------------------------------------------


title_txt <- "**Seven 3s in a Row: The Longest Digit Streak in One Million Digits of π**"

st_txt <- "Each bubble shows how often a digit repeats consecutively in the first one million digits of π. Rows represent digits (0–9) and columns show <span style='color:#db2b27;'>**streak length**</span>, while bubble sizes are scaled using log₁₀ to reveal both common short streaks and rare long ones."

caption_txt <- "
TidyTuesday 2026 Week 12 &mdash; One Million Digits of Pi<br>
**Created by**: Gendson Moreira
"


# Build chart ------------------------------------------------------------


# bubble matrix plot
p <- run_counts |>
  ggplot(aes(x = run_length, y = digit)) +
  geom_point(aes(size = n), color = dot_color, alpha = 0.9) +
  geom_text(
    aes(label = label, x = run_length - 0.15),
    size = 2.5,
    hjust = 1,
    color = dot_color
  ) + 
  scale_x_continuous(
    breaks = 1:7,
    labels = 1:7,
    limits = c(0.5, 8.5)
  ) +
  scale_y_continuous(
    breaks = 0:9,
    labels = paste("Digit", 0:9),
    limits = c(0, 9)
  ) +
  scale_size_continuous(trans = "log10") +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  ) +
  # Add annotation for streak length
  annotate(
    "curve",
    x = 7.5,
    y = 0.75,
    xend = 7,
    yend = 0,
    curvature = 0.3,
    arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
    color = streak_color,
    size = 0.5
  ) +
  annotate(
    "text",
    x = 7.6,
    y = 0.75,
    label = "Streak length",
    hjust = 0,
    vjust = 0.5,
    color = streak_color,
    size = 3
  ) +
  # Add annotation for seven 3s
  annotate(
    "curve",
    x = 7,
    y = 3.5,
    xend = 7,
    yend = 3.1,
    curvature = 0.3,
    arrow = arrow(length = unit(0.005, "npc"), type = "closed"),
    color = dot_color,
    size = 0.5
  ) +
  annotate(
    "text",
    x = 7.1,
    y = 3.5,
    label = "Seven 3s in a Row",
    hjust = 0,
    vjust = 0.5,
    color = dot_color,
    size = 3
  )

fig <- p + ggview::canvas(width = 10, height = 8)
fig


# Define filenames -------------------------------------------------------

tuesday <- "2026-03-24"
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
