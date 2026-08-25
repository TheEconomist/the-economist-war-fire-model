# Map of the manual exclusion zones in source-data/forest_fire_locations_2022_2024.csv,
# with war fires overlaid, so the boxes can be checked against the fires they suppress.

library(tidyverse)
library(sf)
library(rnaturalearth)

ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# ── Load exclusion boxes ──────────────────────────────────────────────────────
# Note: dates in this file are day-first (%d/%m/%Y), and lat1 is the northern edge.
excl <- read_csv('source-data/forest_fire_locations_2022_2024.csv',
                 show_col_types = FALSE) %>%
  mutate(row = row_number(),
         start_date = as.Date(start_date, format = '%d/%m/%Y'),
         end_date   = as.Date(end_date,   format = '%d/%m/%Y'),
         xmin = pmin(lng1, lng2), xmax = pmax(lng1, lng2),
         ymin = pmin(lat1, lat2), ymax = pmax(lat1, lat2),
         active = Sys.Date() >= start_date & Sys.Date() <= end_date,
         period = paste(format(start_date, '%b %Y'), '-', format(end_date, '%b %Y')))

stopifnot(!any(is.na(c(excl$start_date, excl$end_date))))

ukraine <- ne_countries(scale = 'medium', country = 'Ukraine', returnclass = 'sf')

# ── War fires from the window each box covers, to show what it suppresses ─────
fires <- read_csv('output-data/ukraine_war_fires.csv', show_col_types = FALSE) %>%
  filter(date >= Sys.Date() - 30)

# ── Map 1: every box ever drawn, labelled by row number ───────────────────────
ggplot() +
  geom_sf(data = ukraine, fill = 'gray95', colour = 'gray60') +
  geom_rect(data = excl, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                             colour = active), fill = NA, linewidth = 0.4) +
  geom_text(data = excl, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2,
                             label = row, colour = active), size = 3) +
  scale_colour_manual(values = c(`TRUE` = 'red', `FALSE` = 'gray50'),
                      labels = c(`TRUE` = 'Active today', `FALSE` = 'Expired'),
                      name = '') +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = 'white', colour = NA),
        legend.position = 'bottom') +
  labs(title = 'Manual exclusion zones', x = '', y = '',
       subtitle = 'Numbers are row numbers in forest_fire_locations_2022_2024.csv')
ggsave('plots/exclusion_zones_map.png', width = 10, height = 8)

# ── Map 2: currently active boxes against the last 30 days of war fires ───────
active <- excl %>% filter(active)

if (nrow(active) > 0) {
  ggplot() +
    geom_sf(data = ukraine, fill = 'gray95', colour = 'gray60') +
    geom_point(data = fires, aes(x = LONGITUDE, y = LATITUDE),
               colour = 'darkred', alpha = 0.15, size = 0.6) +
    geom_rect(data = active, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, colour = 'blue', linewidth = 0.5) +
    geom_text(data = active, aes(x = (xmin + xmax) / 2, y = ymax + 0.15, label = row),
              colour = 'blue', size = 3) +
    theme_minimal(base_size = 13) +
    theme(plot.background = element_rect(fill = 'white', colour = NA)) +
    labs(title = paste0('Exclusion zones active on ', Sys.Date(),
                        ', with war fires of the past 30 days'),
         subtitle = 'Fires shown are those that survived exclusion - boxes should look empty',
         x = '', y = '')
  ggsave('plots/exclusion_zones_active_map.png', width = 10, height = 8)
}

# ── Table for cross-checking ─────────────────────────────────────────────────
excl %>%
  select(row, xmin, xmax, ymin, ymax, period, active, note) %>%
  print(n = Inf, width = Inf)
