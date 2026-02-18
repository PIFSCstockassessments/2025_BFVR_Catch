create_assessment_plot <- function() {
  fish_icon_url <- "04_Shiny/fishicon.png"
  assessment_total_fish <- round(1008000/5000)
  assessment_commercial_percent <- 47
  
  # Pre-compute assessment fish counts
  assessment_commercial_fish <- round((assessment_commercial_percent / 100) * assessment_total_fish)
  assessment_noncommercial_fish <- assessment_total_fish - assessment_commercial_fish
  
  # Calculate grid dimensions
  cols <- ceiling(sqrt(assessment_total_fish))
  rows <- ceiling(assessment_total_fish / cols)
  
  # Calculate scale
  scale <- min(1.1 / cols, 1.1 / rows)
  
  # Create positions grid
  all_positions <- expand.grid(x = 1:cols, y = 1:rows)[1:assessment_total_fish, ]
  
  # Create the assessment plot data frame
  assessment_df <- all_positions
  assessment_df$sector <- c(
    rep("Commercial", assessment_commercial_fish),
    rep("Non-commercial", assessment_noncommercial_fish)
  )[1:assessment_total_fish]
  assessment_df$image <- fish_icon_url
  
  # Common plot settings
  common_theme <- theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  common_scale <- scale_color_manual(
    values = c("Commercial" = "#F09008FF", "Non-commercial" = "#488820FF", " " = "#FFFFFF"),
    name = "Sector"
  )
  
  # Create the assessment plot
  ggplot(assessment_df, aes(x = x, y = y, image = image, color = sector)) +
    geom_image(size = scale) +
    common_scale +
    common_theme +
    labs(
      title = "Allocation of Total ACL Between Sectors (2024)",
      subtitle = paste0(
        "Commercial: ", assessment_commercial_fish * 5, " thousand lbs (", assessment_commercial_percent, "%), ",
        "Non-commercial: ", assessment_noncommercial_fish * 5, " thousand lbs (", 100 - assessment_commercial_percent, "%)"
      )
    ) +
    coord_fixed(ratio = 1)
}

# The main plot function now only needs to create p2
aclplot <- function(data) {
  # Constants
  fish_icon_url <- "04_Shiny/fishicon.png"
  
  # Calculate actual fish counts
  commercial_fish <- data$FishCount[1]
  noncommercial_fish <- data$FishCount[2]
  total_fish <- commercial_fish + noncommercial_fish
  
  # Calculate grid dimensions (make it somewhat square)
  cols <- ceiling(sqrt(total_fish))
  rows <- ceiling(total_fish / cols)
  
  # Calculate scale
  scale <- min(1.1 / cols, 1.1 / rows)
  
  # Create positions grid
  all_positions <- expand.grid(x = 1:cols, y = 1:rows)[1:total_fish, ]
  
  # Create the new plot data frame
  new_df <- all_positions
  new_df$sector <- c(
    rep("Commercial", commercial_fish),
    rep("Non-commercial", noncommercial_fish)
  )[1:total_fish]
  new_df$image <- fish_icon_url
  
  # Common plot settings
  common_theme <- theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  common_scale <- scale_color_manual(
    values = c("Commercial" = "#F09008FF", "Non-commercial" = "#488820FF", " " = "#FFFFFF"),
    name = "Sector"
  )
  
  # Create the new plot (only p2 needs to be generated each time)
  p2 <- ggplot(new_df, aes(x = x, y = y, image = image, color = sector)) +
    geom_image(size = scale) +
    common_scale +
    common_theme +
    labs(
      title = "Allocation of Total ACL Between Sectors (New)",
      subtitle = paste0(
        "Commercial: ", commercial_fish * 5, " thousand lbs (", round(data$Allocation[1]), "%), ",
        "Non-commercial: ", noncommercial_fish * 5, " thousand lbs (", round(data$Allocation[2]), "%)"
      )
    ) +
    coord_fixed(ratio = 1)
  
}