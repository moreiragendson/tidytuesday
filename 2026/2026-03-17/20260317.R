# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(giscoR)
library(patchwork)
library(scales)


# Load data --------------------------------------------------------------

monthly_losses_data <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv'
)
monthly_mortality_data <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_mortality_data.csv'
)

# Data wrangling ---------------------------------------------------------

selected_regions <- c(
  "Vestland",
  "Trøndelag",
  "Nordland"
)


df <- monthly_mortality_data |>
  filter(region %in% selected_regions, species == "salmon") |> 
  mutate(region = factor(region, levels = selected_regions))


norway <- giscoR::gisco_get_nuts(
  country = "NO",
  nuts_level = 3, # counties
  year = 2021,
  resolution = "20"
) |>
  mutate(
    highlight = ifelse(NAME_LATN %in% selected_regions, NAME_LATN, "Other")
  )

# Check mortality measure unit
summary(monthly_mortality_data$median)

# Define colors and fonts ------------------------------------------------

font_add_google("Fira Sans")
font_add_google("Inter")
showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)


title_font <- "Fira Sans"
body_font <- "Inter"


txt_color <- "#100C08"
bg_color <- "#E0E0E0"


custom_pal <- c(
      "Vestland" = "#ec008b",
      "Trøndelag" = "#1696d2",
      "Nordland" = "#55b748",
      "Other" = "#d2d2d2"
    )


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

    # Legend
    legend.position = "none",

    # Facets
    strip.text = element_text(size = rel(0.9), family = title_font),

    # Axis
    axis.text.x = element_text(size = rel(0.9), face = "bold"),
    axis.text.y = element_text(size = rel(0.75)),
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggplot2::set_theme(t)


# Define chart texts -----------------------------------------------------

title_txt <- "**Salmon mortality rate (%) varies across Norway’s major aquaculture regions**"

st_txt <- "Monthly median mortality of farmed salmon with interquartile range (Q1–Q3). Focus on three key production regions: <span style='color:#ec008b;'>**Vestland**</span>, <span style='color:#1696d2;'>**Trøndelag**</span>, and <span style='color:#55b748;'>**Nordland**</span>."

caption_txt <- "
TidyTuesday 2026 Week 11 &mdash; Salmonid Mortality Data<br>
**Data**: Norwegian Veterinary Institute<br>
**Created by**: Gendson Moreira
"


# Build chart ------------------------------------------------------------


ribbon_pal <- alpha(custom_pal, 0.2)

p <- df |>
  ggplot(aes(x = date, y = median, color = region, fill = region)) +
  geom_ribbon(
    aes(ymin = q1, ymax = q3),
    linewidth = 0
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~region, nrow = 3) +
  scale_color_manual(values = custom_pal) +
  scale_fill_manual(values = ribbon_pal) +
  scale_x_date(
  date_breaks = "1 year",
  date_labels = "%Y"
)


# Build map --------------------------------------------------------------


m <- norway |>
  ggplot() +
  geom_sf(aes(fill = highlight), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = custom_pal) +
  coord_sf(datum = NA)


# Join vizzes ------------------------------------------------------------


fig <- (p + m + plot_layout(widths = c(2, 1.25))) + 
  plot_annotation(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  ) + 
  ggview::canvas(width = 8, height = 6)

fig


# Define filenames -------------------------------------------------------

tuesday <- "2026-03-17"
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
