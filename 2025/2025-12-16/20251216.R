
# Load libraries ---------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
# library(glue)
library(marquee)
library(skimr)
library(geofi)
library(sf)


# Load fonts -------------------------------------------------------------


font_add_google("Lato", "lato")

text <- "<span style='font-family:fa-brands;'>&#xf08c;</span> linkedin.com/in/yourname"

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(caption = text) +
  theme(
    plot.caption = element_textbox_simple(size = 10, hjust = 0)
  )

# Save the plot
ggsave("plot.png", width = 8, height = 6, dpi = 300)

# Define colors and fonts ------------------------------------------------


txt_color <- "#f5f5f5"
bg_color <- "#353535"
border_color <- "#d2d2d2"
helsinki_color <- "#ec008b"
other_color <- "#fdbf11"
txt_font <- "lato"


# Define chart texts -----------------------------------------------------

title_txt <- "**Roundabouts across the** ~~world~~ **Finland**"
st_txt <- "
<span style='color:#ec008b;'>**Helsinki**</span> and 
<span style='color:#fdbf11;'>**Other**</span> counties roundabouts locations<br>
<br>
This week we are exploring data from the **{roundabouts}** package by Emil Hvitfeldt. 
The roundabouts package provides an R friendly way to access the roundabouts database which is compiled by Kittelson & Associates and contains information about the location, configuration, and construction of roundabout intersections around the world.
"

legend_txt <- "**Data**: TidyTuesday 2025 W50<br>**Graphic**: Gendson Moreira"


# Define filenames -------------------------------------------------------


tuesday <- "2025-12-16"
tuesyear <- stringr::str_extract(tuesday, pattern = stringr::regex("\\d{4}(?=-)"))
tuesday_file <- stringr::str_remove_all(tuesday, pattern = stringr::regex("-"))
tuespng <- stringr::str_c(tuesday_file, ".png")
plt_file_name <- stringr::str_c(tuesyear, tuesday, tuespng, sep = "/")


# Load data --------------------------------------------------------------


roundabouts_clean <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv') |> 
  filter(country == "Finland" & status == "Existing") |> 
  mutate(color = ifelse(county_area == "Helsinki", county_area, "Other"))


# Transform data ---------------------------------------------------------


# Convert to sf with CORRECT initial CRS (4326 for lat/long)
# Transform both to same CRS (3067 is better for Finland)


pts <- st_as_sf(
  roundabouts_clean,
  crs = 4326,
  coords = c("lat", "long"),
  remove = FALSE
) |> 
  st_transform(crs = 3067)


muni <- 
  geofi::get_municipalities() |> 
  st_transform(crs = 3067)


# Build chart ------------------------------------------------------------




p <- ggplot() +
  geom_sf(data = muni, fill = NA, color = border_color, size = 0.04) +
  geom_sf(data = pts, size = 0.01, aes(color = color)) + 
  scale_color_manual(
    values = c(
      "Helsinki" = helsinki_color,
      "Other" = other_color
    )
  )+
  coord_sf()+
  labs(
    title = title_txt,
    subtitle = st_txt,
    caption = legend_txt
  )+
  theme_minimal(base_family = txt_font)+
  theme(

    # Texts
    axis.text = element_blank(),

    plot.title = marquee::element_marquee(
      size = 14,
      color = txt_color),

    plot.subtitle = element_textbox_simple(
      size = 7,
      color = txt_color,
      lineheight = 1.2,
      padding = margin(3, 0, 3, 0),
      margin = margin(3, 0, 3, 0)
    ),
    plot.caption = element_textbox_simple(
      color = txt_color,
       size = 10,
       lineheight = 1.3,
),

    # Panel
    panel.grid = element_blank(),
    panel.background = element_blank(),

    # Plot
    plot.background = element_rect(fill = bg_color),
    legend.position = "none"
    
  )

p

# Save chart -------------------------------------------------------------


ggsave(
  filename = plt_file_name,
  plot = p,
  device = "png",
  width = 4,
  height = 7,
  units = "in",
  dpi = 600
)



