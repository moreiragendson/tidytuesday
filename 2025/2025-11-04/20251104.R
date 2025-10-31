
# Load libraries ---------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(geomtextpath)
library(glue)
library(ggtext)


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")


# Define colors and fonts ------------------------------------------------

txt_color <- "#ffffff"
line_color <- "#ffffff"
bg_color <- "#000000"
txt_font <- "lato"
two_group_cat_palette <- c("#1696d2", "#fdbf11") 


# Define chart texts -----------------------------------------------------

title_txt <- "Lead concentration in Flint water samples in 2015"
st_txt <- glue::glue("The data this week includes samples collected by the Michigan Department of Environment (MDEQ) and data from a citizen science project coordinated by Prof Marc Edwards and colleagues at Virginia Tech. Community-sourced samples were collected after concerns were raised about the MDEQ excluding samples from their data")
caption_txt <- ""


# Define filenames -------------------------------------------------------

tuesday <- "2025-11-04"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Load data --------------------------------------------------------------

# tuesdata <- tidytuesdayR::tt_load(tuesday)
flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')
flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')


# Join data --------------------------------------------------------------

df <- bind_rows(
  flint_mdeq |> mutate(study = "MDEQ"),
  flint_vt |> mutate(study = "Virginia Tech")
)

# Explore data -----------------------------------------------------------

n_labels <- df %>%
  group_by(study) %>%
  summarise(n = n())


# Build chart ------------------------------------------------------------

p <- df |>
  ggplot(aes(x = lead, fill = study, color = study)) +
  geom_dotplot(
    position = "jitter",
    binwidth = 2,
    dotsize = 1,
    binpositions = "all",
    alpha = 0.25
  ) +
  geomtextpath::geom_textdensity(
    aes(label = study),
    linecolour = line_color,
    textcolour = txt_color,
    linewidth = 0.5
) +
  scale_fill_manual(values = two_group_cat_palette) +
  scale_color_manual(values = two_group_cat_palette) +
  facet_wrap(~study) +
  geom_text(
    data = n_labels,
    aes(
      x = Inf, y = Inf, 
      label = paste0("n = ", n),
      color = study
    ),
    hjust = 1.1, vjust = 1.5,
    size = 4,
    inherit.aes = FALSE
  ) +
  labs(
    title = title_txt,
    subtitle = st_txt
  ) +
  theme_minimal(base_family = txt_font) +
  theme(
    plot.background = element_rect(fill = bg_color),
    panel.grid = element_blank(),

    legend.position = "none",

    # Axis
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = txt_color),
    axis.title.x = element_blank(),

    # Facet
    strip.text = element_blank(),
    strip.background = element_blank(),

    title = element_textbox_simple(
      margin = margin(r = 5, b = 5, t = 5, l = 10),
      color = txt_color,
      face = "bold",
      size = 16
    ),

    plot.subtitle = element_textbox_simple(
      margin = margin(r =5, b = 5, t = 5, l = 5),
      color = txt_color,
      size = 12,
      face = "italic"
    )
  )

p

# Save chart -------------------------------------------------------------


ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600
)
