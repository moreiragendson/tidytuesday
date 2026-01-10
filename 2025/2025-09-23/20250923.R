
# Load libraries ---------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(showtext)

# Load fonts -------------------------------------------------------------

font_add_google("Lato", "lato")

# Define colors and fonts ------------------------------------------------


txt_color <- "#001219"
bg_color <- "#ffffff"
negative_color <- "#fdbf11"
positive_color <- "#1696d2"
txt_font <- "lato"

# Load data --------------------------------------------------------------

# Option 2: Read directly from GitHub

fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

# Explore data -----------------------------------------------------------

check_unique_combo <- function(df, vars) {
  df %>%
    summarise(all_unique = n_distinct(across(all_of(vars))) == n()) %>%
    pull(all_unique)
}

dfs <- list(
  august = fide_ratings_august,
  september = fide_ratings_september
)

map(dfs, check_unique_combo, vars = c("id", "name"))


# Join data --------------------------------------------------------------

fide_joined <- inner_join(fide_ratings_september, 
                         fide_ratings_august, 
                         by = c("id", "name"), 
                         suffix = c("_sept", "_aug"))


# Prepare data -----------------------------------------------------------


offset <- 10

fide_top20 <- fide_joined %>%
  mutate(
    rank_sept = dense_rank(desc(rating_sept)),
    rank_aug  = dense_rank(desc(rating_aug)),
    rank_diff = rank_aug - rank_sept
  ) %>%
  filter(!is.na(rank_sept) & !is.na(rank_aug)) %>%
  arrange(desc(abs(rank_diff))) %>%
  slice_head(n = 20) %>%
  select(id, name, fed_sept, rating_sept, rating_aug, rank_sept, rank_aug, rank_diff) %>%
  mutate(
    mover_type = ifelse(rank_diff > 0, "Winner", "Loser"),
    bar_label = ifelse(
      mover_type == "Winner",
      paste0(name, " (", fed_sept, ")"),
      paste0(name, " (", fed_sept, ")")
    ),
    label_y = ifelse(mover_type == "Winner", offset, -offset)  # offset from zero
  )


# Build chart ------------------------------------------------------------



p <- ggplot(fide_top20, aes(x = reorder(name, rank_diff), y = rank_diff, fill = mover_type)) +
  geom_col() +
  geom_text(
    aes(
      y = ifelse(mover_type == "Winner", 0, 0),
      label = bar_label,
      color = txt_color,
      hjust = ifelse(mover_type == "Winner", 1.05, -0.05)
    ),
    size = 3,
    family = "lato",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("Winner" = positive_color, "Loser" = negative_color)) +
  scale_color_identity() +
  labs(
    title = "FIDE Chess Ratings: Clash of the Titans",
    subtitle = "Tracking the sharpest ranking jumps from Aug to Sept 2025 across the International Chess Federation"
  ) +
  theme_bw(base_family = "lato") +
  theme(
    plot.background = element_rect(fill = bg_color),
    legend.position = "none",
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    rect  = element_blank(),
    panel.grid = element_blank()
  )

p


# Save chart -------------------------------------------------------------



ggsave(
  filename = "2025/2025-09-23/20250923.png",
  plot = p,
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600      # Use 300 to 600 for maximum quality/print
)
