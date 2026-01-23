# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(marquee)
library(tidytext)
library(textdata)
library(treemapify)


# Load data --------------------------------------------------------------

apod <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-20/apod.csv'
)


# Define stop words ------------------------------------------------------

stop_apod_boilerplate <- tibble(
  word = c(
    "image",
    "images",
    "pictured",
    "shown",
    "shows",
    "showing",
    "photo",
    "photograph",
    "view",
    "views",
    "seen",
    "apod",
    "today",
    "daily"
  )
)

stop_observation_verbs <- tibble(
  word = c(
    "observed",
    "observe",
    "observing",
    "detected",
    "detect",
    "measured",
    "measuring",
    "recorded",
    "recording",
    "captured",
    "capture"
  )
)

stop_scale_terms <- tibble(
  word = c(
    "large",
    "larger",
    "largest",
    "small",
    "smaller",
    "massive",
    "huge",
    "giant",
    "tiny",
    "vast",
    "enormous"
  )
)


stop_time_space <- tibble(
  word = c(
    "near",
    "far",
    "distant",
    "recent",
    "recently",
    "early",
    "earlier",
    "later",
    "first",
    "second",
    "located",
    "location"
  )
)

stop_hedging <- tibble(
  word = c(
    "may",
    "might",
    "could",
    "likely",
    "possibly",
    "suggests",
    "suggested",
    "appears",
    "appeared"
  )
)

stop_media_context <- tibble(
  word = c(
    "telescope",
    "telescopes",
    "camera",
    "instrument",
    "observatory",
    "data",
    "dataset",
    "infrared",
    "optical",
    "xray"
  )
)

astro_stop_words <- bind_rows(
  stop_apod_boilerplate,
  stop_observation_verbs,
  stop_scale_terms,
  stop_time_space,
  stop_hedging,
  stop_media_context
)


# Prepare data -----------------------------------------------------------

apod_tidy <- apod |>
  mutate(planet = case_when(str_detect(string = title, pattern = "Mercury") ~ "Mercury",
                            str_detect(string = title, pattern = "Venus") ~ "Venus",
                            str_detect(string = title, pattern = "Earth") ~ "Earth",
                            str_detect(string = title, pattern = "Mars") ~ "Mars",
                            str_detect(string = title, pattern = "Jupiter") ~ "Jupiter",
                            str_detect(string = title, pattern = "Saturn") ~ "Saturn",
                            str_detect(string = title, pattern = "Uranus") ~ "Uranus",
                            str_detect(string = title, pattern = "Neptune") ~ "Neptune",
                            .default = NA),
         planet = fct(planet, levels = c("Mercury", "Venus", "Earth", "Mars",
                                         "Jupiter", "Saturn", "Uranus", "Neptune"))
         ) |> 
  drop_na(planet) |> 
  select(planet, title, explanation) |>
  unnest_tokens(
    output = word,
    input = explanation,
    token = "words"
  )

apod_clean <- apod_tidy |>
  anti_join(stop_words, by = "word") |>
  anti_join(astro_stop_words, by = "word")

# The top 10 most frequent words were excluded from subsequent sentiment analysis.
apod_tidy |>
  semi_join(astro_stop_words, by = "word") |>
  count(word, sort = TRUE)

nrc <- textdata::lexicon_nrc(dir = tempdir())


apod_nrc <- apod_clean |>
  inner_join(nrc, by = "word")

apod_nrc_planet <- apod_nrc |>
  count(planet, sentiment) |>
  filter(!sentiment %in% c("positive", "negative"))


# Load fonts -------------------------------------------------------------

font_add_google("Space Grotesk")
font_add_google("Orbitron")


# Define colors and fonts ------------------------------------------------


save_width <- 1920
save_height <- 1080

showtext_opts(dpi = 72) 
showtext_auto()


txt_color <- "#ffffff"
bg_color <- "#100C08"

title_font <- "Orbitron"
body_font <- "Space Grotesk"

space_palette <- c(
  anger        = "#FF6B6B",
  anticipation = "#E09F3E",
  disgust      = "#52796F",
  fear         = "#00F5D4",
  joy          = "#48CAE4",
  sadness      = "#4361EE",
  surprise     = "#B5179E",
  trust        = "#F72585"
)

# Set theme --------------------------------------------------------------



t <- theme_minimal(base_family = body_font) + 
  theme(

    # Plot

    plot.margin = margin(t = 50, r = 40, b = 50, l = 40, unit = "pt"), 
    plot.title = element_textbox_simple(
      family = title_font,
      size = 54,
      color = txt_color,
      margin = margin(t = 5, b = 15)
    ),

    plot.subtitle = element_textbox_simple(
      size = 32,
      color = txt_color,
      lineheight = 1.2,
      margin = margin(b = 25)
    ),

    plot.caption = element_textbox_simple(
      color = txt_color,
      size = 28,
      lineheight = 1.3,
      margin = margin(t = 20)
    ),

    # Axis
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),

    # Panel
    panel.grid = element_blank(),
    plot.background = element_rect(fill = bg_color),

    # Legend
    legend.position = "none",

    # Facet
    strip.text = element_text(family = title_font, size = 28, color = txt_color),
    strip.background = element_rect(fill = bg_color, color = NA),
    panel.spacing = unit(2, "lines")
)

ggplot2::set_theme(t)


# Define chart texts -----------------------------------------------------

title_txt <- "Cosmic Sentiments <span style='color:#48CAE4;'>&mdash;</span> Mapping the Language of Space"

st_txt <- "How do we translate celestial observation into human language? By applying the NRC lexicon to nearly two decades of NASA's APOD archives (2007–2025), we reveal the emotional architecture of discovery. <span style='color:#F72585;'>**Trust**</span> and <span style='color:#E09F3E;'>**Anticipation**</span> emerge as the primary linguistic pillars of science—reflecting a confidence in established data and the forward-looking momentum of exploration."

caption_txt <- "
**Data**: TidyTuesday 2026 W03 &mdash; Astronomy Picture of the Day (APOD) archive
<br>
**Lexicon**: NRC Word-Emotion Association Lexicon
<br>
**Graphic**: Gendson Moreira
"


# Build chart ------------------------------------------------------------


p <- ggplot(apod_nrc_planet,
       aes(
         area = n,
         fill = sentiment,
         label = sentiment
       )) +
  geom_treemap(colour = NA) +
  geom_treemap_text(
    colour = txt_color,
    place = "centre",
    grow = TRUE,
    alpha = 0.9,
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm")
  ) +
  facet_wrap(~ planet, scales = "free", ncol = 4) +
  scale_fill_manual(values = space_palette) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )
p


# Define filenames -------------------------------------------------------

tuesday <- "2026-01-20"
tuesyear <- stringr::str_extract(
  tuesday,
  pattern = stringr::regex("\\d{4}(?=-)")
)
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Save chart -------------------------------------------------------------

ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = save_width,
  height = save_height,
  units = "px",
  dpi = 72
)
