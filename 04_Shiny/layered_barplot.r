# Function to create a layered barplot for annual catch estimates. 
# Plot function used in the BFVR Shiny app

create_layered_catchplot <- function(plot_data, assessment_catch, colors, visibility_states = NULL, source_id = "barPlot"){
  
  segments <- unique(plot_data$type) 
  all_years <- sort(unique(plot_data$year))

  # Default visibility states if none provided
  if(is.null(visibility_states)) {
    visibility_states <- list(
      "Total Catch" = "legendonly",
      "Total catch used in the 2024 assessment" = "legendonly"
    )
    for(segment in segments) {
      visibility_states[[segment]] <- ifelse(segment == "Non-commercial - BFVR approach", TRUE, "legendonly")
    }
  }
  
  # Helper function to safely get visibility state with fallback
  get_visibility <- function(name, default = "legendonly") {
    if(!is.null(visibility_states[[name]])) {
      return(visibility_states[[name]])
    }
    return(default)
  }

  # create the first layer of total catch bars
  # Start with an empty plot with the correct source ID
  plot <- plot_ly(source = source_id)
  # Add total catch bars
  total_data <- plot_data %>% 
    group_by(year) %>% 
    summarise(annual_total = sum(catch))
  
  plot <- plot %>% add_trace(
    data = total_data,
    type = "bar",
    x = ~year,
    y = ~annual_total,
    name = "Total Catch",
    marker = list(color = "#D3D3D3"),
    width = 0.8,
    visible = get_visibility("Total Catch"),
    hoverinfo = "text",
    hovertext = ~paste0("Year: ", year, 
                        "<br>Catch: ", format(round(annual_total/1000, 1), big.mark=","), 
                        "K")
  ) %>%
  # add line for total catch used in 2024 assessment
  add_trace(data = assessment_catch, x = ~year, y = ~catch, type = "scatter",
  mode = "lines+markers", 
  line = list(color = "grey", width = 2),
  marker = list(color = "grey", size = 3),
  name = "Total catch used in the 2024 assessment",
  hoverinfo = "text",
  hovertext = ~paste0("Year: ", year, 
              "<br>Catch: ", format(round(catch/1000, 1), big.mark=","), 
              "K"),
  visible = get_visibility("Total catch used in the 2024 assessment"))

  # add layers for different catch segments
  for (segment in segments) {

    segment_data <- filter(plot_data, type == segment)

    plot <- plot %>%
      add_trace(
        type = "bar",
        x = segment_data$year,
        y = segment_data$catch,
        name = segment,
        marker = list(color = colors[segment],
                      line = list(color = "#D3D3D3", width = 1)),
        width = 0.2,
        offset =  ifelse(segment == "Commercial - CML reported", -0.3, 
                      ifelse(segment == "Non-commercial - BFVR approach", 0.1, -0.1)),
        visible = get_visibility(segment, 
                             default = ifelse(segment == "Non-commercial - BFVR approach", 
                                            TRUE, "legendonly")),
        hoverinfo = "text",
        hovertext = paste0("Year: ", segment_data$year, 
                          "<br>Catch: ", format(round(segment_data$catch/1000, 1), big.mark=","), 
                          "K") 
      )
  }
  
  plot <- plot %>%
          layout(yaxis = list(title = "Catch (lbs)",
            fixedrange = FALSE, # Prevents axis from disappearing
            rangemode = "tozero",
            # tickmode = "linear",
            # tick0 = 0,
            # dtick = 50000,
            # tickformat =  ",~s", 
            zeroline = FALSE,
            tickfont = list(size = 14)),  
            xaxis = list(
            title = "Year",
            tickmode = "array",
            tickvals = all_years,
            ticktext = all_years,
            tickangle = 0,
            dtick = 1,
            showline = FALSE,
            tickfont = list(size = 14)
            ), 
            legend = list(x = 1, y = 1,
            xanchor = "right",
            yanchor = "top",
            font = list(size = 16)))
  
  plot <- event_register(plot, "plotly_legendclick")
   cat("Registered plotly_legendclick event for source:", source_id, "\n")
  return(plot)

}

# p <- create_layered_catchplot(plot_data_all, tc.all, colors)
# p %>% layout(yaxis = list(tickmode = "linear",
#             tick0 = 0,
#             dtick = 50000,
#             tickformat =  ",~s"))


create_county_n_fishers_plot <- function(FC, county_filter, color){

  county_fishers <- FC %>% 
    filter(year < 2023 & county == county_filter)  
  
  all_years <- sort(unique(county_fishers$year))

  n_fishers_plot <- plot_ly()%>%
    add_trace(
      type = "bar",
      data = county_fishers,
      x = ~year,
      y = ~n_bf_fishers,
      name = county_filter,
      marker = list(color = color),
      width = 0.8
    ) %>%
  layout(yaxis = list(title = "Number of active non-commercial Deep7 fishers", 
              zeroline = FALSE,
              range  = list(0,800), 
              tickfont = list(size = 14)),  
            xaxis = list(
              title = "Year",
              tickmode = "array",
              tickvals = all_years,
              ticktext = all_years,
              tickangle = 0,
              dtick = 1,
              showline = FALSE, 
              tickfont = list(size = 14)
            ), 
            legend = list(x = 1, y = 1,
            xanchor = "right",
            yanchor = "top",
            font = list(size = 16)))

  n_fishers_plot


}

#create_layered_n_fishers_plot(FC, "Honolulu County", "#B71300")
