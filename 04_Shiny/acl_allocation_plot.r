 library(ggimage)
 
 aclplot <- function(data){
 # Create the new visualization with 200 individual fish using geom_image
   library(patchwork)
   
   # Fish icon URL
   fish_icon_url <- "~/R/2025_BFVR_Catch-feat/04_Shiny/fishicon.png"
   
   old_total_fish <- round(1008000/5000)
   
   # Old allocation
   old_commercial <- 47
   old_non_commercial <- 100 - old_commercial
   
   # Calculate actual fish counts based on percentages
   old_commercial_fish <- round((old_commercial / 100) * old_total_fish)
   old_noncommercial_fish <- old_total_fish - old_commercial_fish
   
   # Calculate grid dimensions (trying to make it somewhat square)
   old_cols <- ceiling(sqrt(old_total_fish))
   old_rows <- ceiling(old_total_fish / old_cols)
   
   # New umber of fish in each category
   commercial_fish <- data$FishCount[1]
   noncommercial_fish <- data$FishCount[2]
   
   total_fish <- commercial_fish+noncommercial_fish
   
   # Calculate grid dimensions (trying to make it somewhat square)
   cols <- ceiling(sqrt(total_fish))
   rows <- ceiling(total_fish / cols)
   
   old_cols <- max(old_cols, cols)
   old_rows <- max(old_rows, rows)
   
   # Create labels for every old fish
   old_labels <- c(
     rep("Commercial", old_commercial_fish),
     rep("Non-commercial", old_noncommercial_fish)
   )
   
   # Create labels for every new fish
   labels <- c(
     rep("Commercial", commercial_fish),
     rep("Non-commercial", noncommercial_fish)
     #rep(" ", old_total_fish-total_fish)
   )
   
   if (total_fish > old_total_fish) {
     num_pad <- total_fish - old_total_fish
     if (num_pad > 0) {
       old_labels <- append(old_labels, rep(" ", num_pad), after = length(old_labels))
     }
     old_total_fish <- total_fish
   } else {
     num_pad <- old_total_fish - total_fish
     if (num_pad > 0) {
       labels <- append(labels, rep(" ", num_pad), after = length(labels))
     }
   }
   
   
   # Create positions for all fish
   old_all_positions <- expand.grid(x = 1:old_cols, y = 1:old_rows)
   old_all_positions <- old_all_positions[1:old_total_fish, ]
   
   old_all_positions <- old_all_positions %>%
     mutate(
       sector = old_labels,
       image = fish_icon_url
     )
   
   # Create positions for all new fish
   all_positions <- expand.grid(x = 1:old_cols, y = 1:old_rows)
   all_positions <- all_positions[1:old_total_fish, ]
   
   all_positions <- all_positions %>%
     mutate(
       sector = labels,
       image = fish_icon_url
     )
   
   scale <- min(1.1 / old_cols, 1.1 / old_rows)
   
   
   # Create the plot using ggplot2 and geom_image
   p1 <- ggplot(old_all_positions, aes(x = x, y = y, image = image, color = sector)) +
     geom_image(size = scale) +  # Adjust size as needed
     scale_color_manual(values = c("Commercial" = "#F09008FF", "Non-commercial" = "#488820FF", " "="#FFFFFF"),
                        name = "Sector") +
     theme_void() +
     theme(
       legend.position = "bottom",
       plot.title = element_text(hjust = 0.5, size = 16),
       plot.subtitle = element_text(hjust = 0.5, size = 9)
     ) +
     labs(
       title = "Allocation of Total ACL Between Sectors (2024)",
       subtitle = paste0(
         "Commercial: ", old_commercial_fish*5, " thousand lbs (", old_commercial, "%), ",
         "Non-commercial: ", old_noncommercial_fish*5, " thousand lbs (", old_non_commercial, "%)"
       )
     ) +
     coord_fixed(ratio = 1) +
     # Increase plot margins to avoid truncation
     theme(plot.margin = margin(10, 10, 10, 10))
   
   # Create the plot using ggplot2 and geom_image
   p2 <- ggplot(all_positions, aes(x = x, y = y, image = image, color = sector)) +
     geom_image(size = scale) +  # Adjust size as needed
     scale_color_manual(values = c("Commercial" = "#F09008FF", "Non-commercial" = "#488820FF", " "="#FFFFFF"),
                        name = "Sector") +
     theme_void() +
     theme(
       legend.position = "bottom",
       plot.title = element_text(hjust = 0.5, size = 16),
       plot.subtitle = element_text(hjust = 0.5, size = 9)
     ) +
     labs(
       title = "Allocation of Total ACL Between Sectors (New)",
       subtitle = paste0(
         "Commercial: ", commercial_fish*5, " thousand lbs (", round(data$Allocation[1]), "%), ",
         "Non-commercial: ", noncommercial_fish*5, " thousand lbs (", round(data$Allocation[2]), "%)"
       )
     ) +
     coord_fixed(ratio = 1) +
     # Increase plot margins to avoid truncation
     theme(plot.margin = margin(10, 10, 10, 10))
   
   return(p1 + p2)
   
 }
 
