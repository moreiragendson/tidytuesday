
# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(packcircles)
library(ggforce)
library(ggtext)
library(marquee)


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")


# Define colors and fonts ------------------------------------------------

bg_color <- "#353535"
circle_txt_color <- "#353535"
circle_color <- "#d2d2d2"
txt_color <- "#f5f5f5"
txt_font <- "lato"


# Define chart texts -----------------------------------------------------


title_txt <- "**Worldwide Threatened Languages by Region**"
st_txt <- "
This week we're exploring The Languages of the World, curated from Glottolog 5.2.1, an open-access database in linguistics, maintained by the Max Planck Institute for Evolutionary Anthropology.
"

legend_txt <- "
**Data**: TidyTuesday 2025 W51 
<br>
**Graphic**: Gendson Moreira
"

# Define filenames -------------------------------------------------------

tuesday <- "2025-12-23"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Load data --------------------------------------------------------------

# Option 2: Read directly from GitHub

endangered_status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/endangered_status.csv')
families <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/families.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/languages.csv')


# Join data --------------------------------------------------------------

df <- languages |> 
  left_join(endangered_status, by = "id") |> 
  left_join(families, by = c("family_id" = "id"))


# Explore data -----------------------------------------------------------


t_lang <- df |> 
  filter(status_label=="threatened") |> 
  group_by(macroarea) |> 
  count() |> 
  filter(
    macroarea != "Eurasia;Papunesia" & !is.na(macroarea)
  )


# Build theme ------------------------------------------------------------

t <- theme_minimal(base_family = txt_font) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(bg_color),
    legend.position = "none",

    plot.title = marquee::element_marquee(
      size = 20,
      color = txt_color
    ),

    plot.subtitle = element_textbox_simple(
      size = 10,
      color = txt_color,
      lineheight = 1.2,
      padding = margin(3, 0, 3, 0),
      margin = margin(3, 0, 3, 0)
    ),

    plot.caption = element_textbox_simple(
      color = txt_color,
      size = 12,
      lineheight = 1.3,
    )
  )

set_theme(t)


# Build chart ------------------------------------------------------------

packing <- circleProgressiveLayout(
  x = t_lang$n,
  sizetype = "area"
)

circle_data <- cbind(t_lang, packing) |> 
  as_tibble() |> 
  arrange(desc(n)) |> 
  mutate(
    rank = row_number()
  )

p <- ggplot() +
  geom_circle(
    data = circle_data,
    mapping = aes(
      x0 = x,
      y0 = y,
      r = radius
    ),
    fill = circle_color,
    color = "transparent"
  ) + 
  geom_text(
    data = circle_data |> filter(n >= 100),
    mapping = aes(
      x = x,
      y = y,
      label = macroarea,
      size = case_when(
        rank <= 5 ~ 6,
        rank > 5 ~ 4
      )
    ),
    fontface = "bold",
    check_overlap = TRUE
  ) +
  geom_text(
    data = circle_data |> filter(n >= 100),
    mapping = aes(
      x = x,
      y = y - 2,
      label = n,
      size = case_when(
        rank <= 5 ~ 6,
        rank > 5 ~ 4
      )
    ),
    check_overlap = TRUE
  ) + 
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = legend_txt
  )

p

# Save chart -------------------------------------------------------------

ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 6.5,
  height = 9,
  units = "in",
  dpi = 600
)
