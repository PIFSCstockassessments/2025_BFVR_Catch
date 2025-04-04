# Shiny functions 
create_catch_plot <- function(plot_data, tc_all_sp, common_name_id, 
year_labels, colors, show_legend = TRUE) {
  # Basic layout settings
  base_layout <- list(
    xaxis = list(
      title = "Year",
      tickmode = "array",
      tickvals = year_labels,
      ticktext = year_labels,
      tickangle = 0,
      dtick = 1
    ),
    yaxis = list(title = "Catch (lbs)"),
    hovermode = "closest",
    legend = list(title = list(text = "Type")),
    margin = list(t = 70)  # Increase top margin for title
  )
  
  # Create plot
  p <- plot_ly(data = plot_data %>% filter(common_name == common_name_id), 
                x = ~year, y = ~catch, color = ~type, 
                type = "bar",
                colors = colors) %>%
        add_trace(data = tc_all_sp %>% filter(common_name == common_name_id), 
                x = ~year, y = ~catch, type = "scatter",
                mode = "lines+markers",
                line = list(color = "grey", width = 2),
                marker = list(color = "grey", size = 3),
                name = "Total catch used in the 2024 assessment") %>%
    layout(
      annotations = list( 
          list( 
            x = 0.2,  
            y = 1.0,  
            text = common_name_id,  
            xref = "paper",  
            yref = "paper",  
            xanchor = "center",  
            yanchor = "bottom",  
            showarrow = FALSE,
            font = list(size = 14, color = "black")
          )),
    showlegend = show_legend,
    xaxis = base_layout$xaxis,
    yaxis = base_layout$yaxis,
    hovermode = base_layout$hovermode,
    margin = base_layout$margin,
    barmode = 'stack'
  )
  
  return(p)
}

# Plot Species-specific results
prepare_catch_plot_data <- reactive({
  req(Final.all.sp())

options <- extras() # currently only extra is if you want to plot underreporting
    
# First create data for BFVR Non-Commercial
non_commercial_data <- Final.all.sp() %>%
  group_by(year, species) %>% 
  summarise(catch = quantile(lbs_caught, .5)) %>% 
  mutate(type = "BFVR Non-Commercial") %>%
  filter(year < 2023)
  
# Decide whether to include unreported CML based on proportion
if (options$prop_unreported > 0) {
  # Create data with both regular CML and unreported CML and NC
  local_cml_all_sp <- cml.all.sp %>%
    mutate(catch = (catch * options$prop_unreported) + (non_commercial_data$catch * options$prop_unreported),
            type = "Unreported") %>% 
    bind_rows(cml.all.sp)
  
  # Combine all data
  plot_data <- bind_rows(non_commercial_data, local_cml_all_sp)
} else {
  # Only include the regular CML data (no unreported)
  plot_data <- bind_rows(non_commercial_data, cml.all.sp)
}

# Set type of catch as a factor to control order of bars in plot
# and set which colors to use for each type
colors <- c("CML" = "#FC8D62", 
            "Unreported" = "#8DA0CB", 
            "BFVR Non-Commercial" = "#66C2A5")

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data$year))

plot_data <- plot_data %>%
  left_join(SP.id, by = c("species" = "sp_frs_id")) %>% 
  filter(species != "d7") %>%
  mutate(type = factor(type, levels = c("BFVR Non-Commercial", "Unreported", "CML")))

common_name_vec <- unique(plot_data$common_name)

return(list(
  plot_data = plot_data,
  colors = colors, 
  all_years = all_years,
  common_name_vec = common_name_vec
))

})


# # For a single plot
# output$plot <- renderPlotly({
#   create_catch_plot(
#     plot_data = plot_data,
#     tc_all_sp = tc.all.sp,
#     common_name = input$selected_species,  # Could be from a dropdown input
#     all_years = all_years,
#     colors = colors
#   )
# })

# # For multiple plots (like in a loop)
# output$plots <- renderUI({
#   plot_output_list <- lapply(1:length(common_name_vec), function(i) {
#     show_legend <- if(i == 1) TRUE else FALSE
#     plotlyOutput(paste0("plot", i))
#   })
  
#   do.call(tagList, plot_output_list)
# })

# # Then create each plot
# observe({
#   for(i in 1:length(common_name_vec)) {
#     local({
#       local_i <- i
#       output_id <- paste0("plot", local_i)
#       show_legend <- if(local_i == 1) TRUE else FALSE
      
#       output[[output_id]] <- renderPlotly({
#         create_catch_plot(
#           plot_data = plot_data,
#           tc_all_sp = tc.all.sp,
#           common_name = common_name_vec[local_i],
#           all_years = all_years,
#           colors = colors,
#           show_legend = show_legend
#         )
#       })
#     })
#   }
# })