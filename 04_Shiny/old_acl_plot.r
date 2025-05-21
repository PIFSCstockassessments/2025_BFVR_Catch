library(ggplot2)
library(ggimage)
library(dplyr)

# Fish icon URL
fish_icon_url <- "~/R/2025_BFVR_Catch-feat/04_Shiny/fishicon.png"

old_total_fish <- 101

# Old allocation percentages and counts
old_commercial <- round(493/1008*100)
old_noncommercial <- 100 - old_commercial

old_commercial_fish <- 49
old_noncommercial_fish <- old_total_fish - old_commercial_fish

# Grid dimensions
old_cols <- 11
old_rows <- 10

# Create labels
old_labels <- c(
  rep("Commercial - CML reported", old_commercial_fish),
  rep("Non-commercial - BFVR approach and CML unreported", old_noncommercial_fish)
)

# Create grid positions
old_all_positions <- expand.grid(x = 1:old_cols, y = 1:old_rows)
old_all_positions <- old_all_positions[1:old_total_fish, ]

old_all_positions <- old_all_positions %>%
  mutate(
    sector = old_labels,
    image = fish_icon_url,
    alpha = 1  # full opacity for real points
  )

# Define consistent factor levels
sector_levels <- c(
  "Commercial - CML reported",
  "Non-commercial - BFVR approach and CML unreported",
  "Non-commercial - BFVR approach",
  "Commercial - CML unreported"
)

# Apply factor levels
old_all_positions$sector <- factor(old_all_positions$sector, levels = sector_levels)

# Add dummy rows for missing sectors to force legend keys
missing_levels <- setdiff(sector_levels, levels(droplevels(old_all_positions$sector)))
if(length(missing_levels) > 0) {
  dummy_rows <- data.frame(
    x = NA, y = NA,
    image = fish_icon_url,
    sector = factor(missing_levels, levels = sector_levels),
    alpha = 0   # invisible on plot
  )
  old_all_positions <- bind_rows(old_all_positions, dummy_rows)
}

# Color scale
shared_color_scale <- scale_color_manual(
  values = c(
    "Commercial - CML reported" = "#F09008FF",
    "Non-commercial - BFVR approach and CML unreported" = "#4D6A8A",
    "Non-commercial - BFVR approach" = "#488820FF",
    "Commercial - CML unreported" = "#7868C0FF"
  ),
  name = "Sector",
  na.translate = FALSE,
  drop = FALSE
)

scale <- 1 / old_cols

# Plot
old_acl_plot <- ggplot(old_all_positions, aes(x = x, y = y, image = image, color = sector, alpha = alpha)) +
  geom_image(size = scale, key_glyph = draw_key_image, show.legend = TRUE) +
  shared_color_scale +
  scale_alpha(range = c(0, 1), guide = "none") +  # hide alpha legend
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 5, t = 5))
  ) +
  labs(
    title = "Allocation of Total ACL Between Sectors (2024)",
    subtitle = paste0(
      "Commercial: ", old_commercial_fish * 10, " thousand lbs (", old_commercial, "%)\n",
      "Non-commercial and CML unreported: ", old_noncommercial_fish * 10, " thousand lbs (", old_noncommercial, "%)"
    )
  ) +
  coord_fixed(ratio = 1) 
