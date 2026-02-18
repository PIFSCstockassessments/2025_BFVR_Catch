new_acl_plot <- function(data){
  # library(ggplot2)
  # library(ggimage)
  # library(dplyr)
   options(ggimage.keytype = "image")
  # Fish icon URL
  fish_icon_url <- "www/fishicon.png"
  
  # Number of fish in each category
  commercial_fish <- data$FishCount[1]
  noncommercial_fish <- data$FishCount[2]
  unreported_fish <- data$FishCount[3]
  
  total_fish <- commercial_fish + noncommercial_fish + unreported_fish
  
  # Create labels
  labels <- c(
    rep("Commercial - CML reported", commercial_fish),
    rep("Non-commercial - BFVR approach", noncommercial_fish),
    rep("Commercial - CML unreported", unreported_fish),
    rep(NA_character_, 101 - total_fish)  # fill up to 101
  )
  
  # Create grid positions
  all_positions <- expand.grid(x = 1:11, y = 1:10)[1:101, ]
  
  all_positions <- all_positions %>%
    mutate(
      sector = labels,
      image = fish_icon_url,
      alpha = ifelse(is.na(sector), 0, 1)  # invisible for filler NAs
    )
  
  # Define consistent factor levels
  sector_levels <- c(
    "Commercial - CML reported",
    "MRIP (Non-commercial and CML unreported)",
    "Non-commercial - BFVR approach",
    "Commercial - CML unreported"
  )
  all_positions$sector <- factor(all_positions$sector, levels = sector_levels)
  
  # Add dummy points for missing sectors
  missing_levels <- setdiff(sector_levels, levels(droplevels(all_positions$sector)))
  if (length(missing_levels) > 0) {
    dummy_rows <- data.frame(
      x = NA, y = NA,
      image = fish_icon_url,
      sector = factor(missing_levels, levels = sector_levels),
      alpha = 0
    )
    all_positions <- bind_rows(all_positions, dummy_rows)
  }
  
  # Color scale
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
  
  scale <- 1 / 11
  
  # Plot
  new_acl_plot <- ggplot(all_positions, aes(x = x, y = y, image = image, color = sector, alpha = alpha)) +
    geom_image(aes(image = image), size = scale, key_glyph = "point") +
    #geom_image(size = scale, key_glyph = draw_key_image) +
    shared_color_scale +
    scale_alpha(range = c(0, 1), guide = "none") +  # hide alpha from legend
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 5, t = 5))
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
  
  return(new_acl_plot)
}
