 library(ggimage)
 # Calculate the allocations based on slider input
  allocations <- reactive({
    commercial <- input$commercial_prop
    non_commercial <- 100 - commercial
    
    # Calculate actual fish counts based on percentages
    commercial_fish <- round((commercial / 100) * 200)
    noncommercial_fish <- 200 - commercial_fish
    
    data.frame(
      Sector = c("Commercial", "Non-commercial"),
      Allocation = c(commercial, non_commercial),
      FishCount = c(commercial_fish, noncommercial_fish)
    )
  })
  
  # Display the exact percentages and fish counts
  output$allocation_text <- renderUI({
    data <- allocations()
    HTML(paste0(
      "<b>Commercial:</b> ", data$Allocation[1], "% (", data$FishCount[1], " fish)<br>",
      "<b>Non-commercial:</b> ", data$Allocation[2], "% (", data$FishCount[2], " fish)"
    ))
  })
  
  # Create the visualization with 200 individual fish using geom_image
  output$allocation_plot <- renderPlot({
    # Get allocation data
    data <- allocations()
    
    # Total number of fish
    total_fish <- 200
    
    # Number of fish in each category
    commercial_fish <- data$FishCount[1]
    noncommercial_fish <- data$FishCount[2]
    
    # Calculate grid dimensions (trying to make it somewhat square)
    cols <- ceiling(sqrt(total_fish))
    rows <- ceiling(total_fish / cols)
    
    # Fish icon URL
    fish_icon_url <- "04_Shiny/fishicon.png"
    
    # Create positions for all fish
    all_positions <- expand.grid(
      x = 1:cols,
      y = 1:rows
    ) %>%
      head(total_fish) %>%
      mutate(
        # Assign sectors based on counts
        sector = c(
          rep("Commercial", commercial_fish),
          rep("Non-commercial", noncommercial_fish)
        ),
        # Add the image URL to all points
        image = fish_icon_url
      )
    
    # Create the plot using ggplot2 and geom_image
    ggplot(all_positions, aes(x = x, y = y, image = image, color = sector)) +
      geom_image(size = 0.08) +  # Adjust size as needed
      scale_color_manual(values = c("Commercial" = "#1E88E5", "Non-commercial" = "#FFC107"),
                         name = "Sector") +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      ) +
      labs(
        title = "Allocation of ACL Between Sectors",
        subtitle = paste0(
          "Commercial: ", commercial_fish, " thousand lbs (", data$Allocation[1], "%), ",
          "Non-commercial: ", noncommercial_fish, " thousand lbs (", data$Allocation[2], "%)"
        )
      ) +
      coord_equal() +
      # Increase plot margins to avoid truncation
      theme(plot.margin = margin(10, 10, 10, 10))
  })
