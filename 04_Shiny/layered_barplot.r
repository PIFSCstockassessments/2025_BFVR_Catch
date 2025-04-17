# Function to create a layered barplot for annual catch estimates. 
# Plot function used in the BFVR Shiny app

create_layered_catchplot <- function(plot_data, assessment_catch, colors){
  # create the first layer of total catch bars
  plot <- plot_data %>% 
          group_by(year) %>% 
          summarise(annual_total = sum(catch)) %>%
          plot_ly() %>%
          # Add the gray background bars for totals
          add_trace(
            type = "bar",
            x = ~year,
            y = ~annual_total,
            name = "Total Catch",
            marker = list(color = "#D3D3D3"),
            width = 0.8,
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
                      "K"))
  segments <- unique(plot_data$type) 
  all_years <- sort(unique(plot_data$year))
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
        hoverinfo = "text",
        hovertext = paste0("Year: ", segment_data$year, 
                          "<br>Catch: ", format(round(segment_data$catch/1000, 1), big.mark=","), 
                          "K")
      )
  }
  
  plot <- plot %>%
          layout(yaxis = list(title = "Catch (lbs)",
            tickmode = "linear",
            tick0 = 0,
            dtick = 50000,
            tickformat =  ",~s", zeroline = FALSE),  
            xaxis = list(
            title = "Year",
            tickmode = "array",
            tickvals = all_years,
            ticktext = all_years,
            tickangle = 0,
            dtick = 1,
            showline = FALSE
            ), 
            legend = list(x = 1, y = 1,
            xanchor = "right",
            yanchor = "top"))
  
  return(plot)

}

#create_layered_catchplot(plot_data_all, tc.all, colors)

