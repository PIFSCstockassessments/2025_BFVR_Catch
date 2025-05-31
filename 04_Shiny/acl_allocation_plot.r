library(ggplot2)
library(ggimage)
library(dplyr)
library(patchwork)

aclplot <- function(data){
   options(ggimage.keytype = "image")
  # Fish icon URL
  fish_icon_url <- "www/fishicon.png"
  
  # New number of fish in each category
  commercial_fish <- data$FishCount[1]
  noncommercial_fish <- data$FishCount[2]
  unreported_fish <- data$FishCount[3]
  
  total_fish <- commercial_fish + noncommercial_fish + unreported_fish
  
  # Create labels for every new fish
  labels <- c(
    rep("Commercial - CML reported", commercial_fish),
    rep("Non-commercial - BFVR approach", noncommercial_fish),
    rep("Commercial - CML unreported", unreported_fish)
  )
  
  # Calculate grid dimensions (trying to make it somewhat square)
  cols <- ceiling(sqrt(total_fish))
  rows <- ceiling(total_fish / cols)
  
  old_total_fish <- 101
  
  # Old allocation
  old_commercial <- round(493/1008*100)
  old_noncommercial <- 100 - old_commercial
  
  # Calculate actual fish counts based on percentages
  old_commercial_fish <- 49
  old_noncommercial_fish <- old_total_fish - old_commercial_fish
  
  # Create labels for every old fish
  old_labels <- c(
    rep("Commercial - CML reported", old_commercial_fish),
    rep("MRIP (Non-commercial and CML unreported)", old_noncommercial_fish)
  )
  
  num <- total_fish - old_total_fish
  if (num > 0) {
    old_labels <- c(old_labels, rep(NA_character_, num))
  }
  
  # Create positions for all fish
  old_all_positions <- expand.grid(x = 1:cols, y = 1:rows)
  old_all_positions <- old_all_positions[1:total_fish, ]
  
  old_all_positions <- old_all_positions %>%
    mutate(
      sector = old_labels,
      image = fish_icon_url
    )
  
  # Create positions for all new fish
  all_positions <- expand.grid(x = 1:cols, y = 1:rows)
  all_positions <- all_positions[1:total_fish, ]
  
  all_positions <- all_positions %>%
    mutate(
      sector = labels,
      image = fish_icon_url
    )
  
  # Define consistent factor levels for all sector types
  sector_levels <- c(
    "Commercial - CML reported",
    "MRIP (Non-commercial and CML unreported)",
    "Non-commercial - BFVR approach",
    "Commercial - CML unreported"
  )
  
  # Apply factor levels to both datasets, even if some sectors aren't used
  old_all_positions$sector <- factor(old_all_positions$sector, levels = sector_levels)
  all_positions$sector <- factor(all_positions$sector, levels = sector_levels)
  
  # Add alpha = 1 for real points
  old_all_positions$alpha <- 1
  all_positions$alpha <- 1
  
  # Add invisible dummy points for missing sectors to force legend keys
  missing_levels_old <- setdiff(sector_levels, levels(droplevels(old_all_positions$sector)))
  if (length(missing_levels_old) > 0) {
    dummy_old <- data.frame(
      x = NA, y = NA, image = fish_icon_url,
      sector = factor(missing_levels_old, levels = sector_levels),
      alpha = 0
    )
    old_all_positions <- bind_rows(old_all_positions, dummy_old)
  }
  
  missing_levels_new <- setdiff(sector_levels, levels(droplevels(all_positions$sector)))
  if (length(missing_levels_new) > 0) {
    dummy_new <- data.frame(
      x = NA, y = NA, image = fish_icon_url,
      sector = factor(missing_levels_new, levels = sector_levels),
      alpha = 0
    )
    all_positions <- bind_rows(all_positions, dummy_new)
  }
  
  scale <- 1 / cols
  
  shared_color_scale <- scale_color_manual(
    values = c(
      "Commercial - CML reported" = "#F09008FF",
      "MRIP (Non-commercial and CML unreported)" = "#4D6A8A",
      "Non-commercial - BFVR approach" = "#488820FF",
      "Commercial - CML unreported" = "#7868C0FF"
    ),
    name = "Sector",
    na.translate = FALSE,
    drop = FALSE
  )
  
  # Plot 1: old allocation
  p1 <- ggplot(old_all_positions, aes(x = x, y = y, image = image, color = sector, alpha = alpha)) +
    geom_image(aes(image = image), size = scale, key_glyph = "point") +
    #geom_image(size = scale, key_glyph = draw_key_image, show.legend = FALSE) +
    shared_color_scale +
    scale_alpha(range = c(0, 1), guide = "none") +  # hide alpha from legend
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_void() +
    theme(
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
  
  # Plot 2: new allocation
  p2 <- ggplot(all_positions, aes(x = x, y = y, image = image, color = sector, alpha = alpha)) +
    geom_image(aes(image = image), size = scale, key_glyph = "point") +
    #geom_image(size = scale, key_glyph = draw_key_image, show.legend = FALSE) +
    shared_color_scale +
    scale_alpha(range = c(0, 1), guide = "none") +  # hide alpha from legend
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 5, t = 5))
    ) +
    labs(
      title = "Allocation of Total ACL Between Sectors (BFVR)",
      subtitle = paste0(
        "Commercial: ", commercial_fish * 10, " thousand lbs (", round(data$Allocation[1]), "%)\n",
        "Non-commercial: ", noncommercial_fish * 10, " thousand lbs (", round(data$Allocation[2]), "%)\n",
        "CML unreported: ", unreported_fish * 10, " thousand lbs (", round(data$Allocation[3]), "%)"
      )
    ) +
    coord_fixed(ratio = 1) 
  
  # Combine plots with shared legend at bottom
  return(
    (p1 + plot_spacer() + p2) +
      plot_layout(widths = c(1, 0.1, 1), guides = "collect") &
      theme(legend.position = "bottom", legend.text = element_text(size = 12)) 
  )
}
