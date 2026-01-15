
# Load libraries ---------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(marquee)
library(scales)
library(spData) # for get worldwide countries boundaries
library(sf)


# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")




# Define filenames -------------------------------------------------------

tuesday <- "2026-01-13"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Load data --------------------------------------------------------------

africa <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-13/africa.csv') |> 
  group_by(country) |> 
  summarise(one = n())


# Prepare data -----------------------------------------------------------


africa_sf <- world |> 
  filter(continent == "Africa", !is.na(iso_a2)) |> 
  left_join(worldbank_df, by = "iso_a2") |> 
  select(country = name, subregion) |> 
  mutate(
    country = case_when(
      country %in% c("Democratic Republic of the Congo", "Republic of Congo")  ~ "Congo",
      country == "Cote d'Ivoire" ~ "Ivory Coast",
      country ==  "The Gambia" ~ "Gambia",
      country == "Guinea-Bissau" ~ "Guinea",
      TRUE ~ country
    )) |> 
  st_transform("ESRI:102022") |> 
  st_make_valid() |> 
  st_collection_extract("POLYGON")

africa_joined <- africa |> 
  right_join(africa_sf, join_by(country == country))


# Define colors and fonts ------------------------------------------------

txt_color <- "#f2f2f2"
bg_color <- "#000000"
txt_font <- "lato"

pal1 <- c("#FFF2CF","#FCE39E","#FDD870","#FCCB41","#FDBF11","#E88E2D","#CA5800","#843215")
pal2 <- c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB","#1696D2","#12719E","#0A4C6A","#062635")

var_colors <- c(rev(pal1), pal2)

# Define chart texts -----------------------------------------------------

title_txt <- "**Language Diversity Across African Nations**"
st_txt <- "*Number of spoken languages by country*"
caption_txt <- "
**Data**: TidyTuesday 2026 W02
<br>
**Graphic**: Gendson Moreira
"


# Set theme --------------------------------------------------------------

t <- theme_minimal(base_family = txt_font) + 
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(bg_color),
    plot.margin = margin(0, 10, 0, 10),
    axis.text = element_blank(),

    # Legend
    legend.position = c(0.2, 0.15),
    legend.direction = "horizontal",
    legend.text = element_text(color = txt_color),
    legend.title = element_blank(),


    plot.title = marquee::element_marquee(
      size = 16,
      color = txt_color
    ),

    plot.subtitle = element_textbox_simple(
      size = 12,
      color = txt_color,
      lineheight = 1.2,
      padding = margin(0, 3, 3, 0),
      margin = margin(0, 3, 3, 0)
    ),

    plot.caption = element_textbox_simple(
      color = txt_color,
      size = 12,
      lineheight = 1.3,
      padding = margin(3, 3, 0, 0),
      margin = margin(3, 3, 0, 0)
    )
  )

ggplot2::set_theme(t)

# Build chart ------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = africa_joined,
    aes(fill = one, geometry = geom, label = country),
    na.rm = TRUE) +
  scale_fill_gradientn(
    colours = pal2,
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    na.value = bg_color
  ) +
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = caption_txt
  )

p


# Save chart -------------------------------------------------------------


ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 600
)
