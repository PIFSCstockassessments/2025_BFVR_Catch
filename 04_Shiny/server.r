
# Define server
server <- function(input, output, session) {
  
# Reactive values to store results
results <- reactiveVal(NULL)

# Create a reactive trigger that changes whenever run 
# analysis button is clicked
sim_trigger <- reactive({
# Dependencies: filtered data and simulation inputs
list(
    input$run_analysis      # Also allow manual triggering
)
# Return the current time as a convenient non-NULL value
return(Sys.time())
})

# Function to load data and run analysis
# In server
data_prep <- eventReactive(sim_trigger(), {
    
    percent_inactive <- (input$percent_inactive/100)
    only_bf_registered <- input$only_bf_registered 
    selected_quantile <- input$selected_quantile
    filter_level_id <- input$which_filter_level
    filter_taxa_id <- input$which_filter_taxa_level
    percent_unregistered <- (input$percent_unregistered/100)
    
    # Apply the correction related to the assumed # of BF registered fishers 
    # catching at least one deep7 in any given year
    # and the percentage of boats that are not registered in the BFVR
    FC_sim <- FC %>% 
        mutate(n_bf_fishers = n_bf_fishers - (n_bf_fishers * percent_inactive),
        n_bf_fishers = n_bf_fishers + n_bf_fishers * (1 - percent_unregistered)) 

    # Create a new column for future data manipulation
    F2$trip_type <- 0

    if(filter_level_id == "Trip" | filter_level_id == "Both"){
      QT_trip <- QT_trip %>% 
        filter(quantiles == selected_quantile) %>% 
        as.data.table()

      F2[d7 > QT_trip[sp_frs_id=="d7"]$value]$trip_type <- 1
      
      if(filter_taxa_id == "All taxa" ){
  
        F2[s17 > QT_trip[sp_frs_id=="s17"]$value| 
            s19 > QT_trip[sp_frs_id=="s19"]$value| 
            s21 > QT_trip[sp_frs_id=="s21"]$value|
            s22 > QT_trip[sp_frs_id=="s22"]$value|
            s97 > QT_trip[sp_frs_id=="s97"]$value| 
            s20 > QT_trip[sp_frs_id=="s20"]$value 
          ]$trip_type <- 1
      }
    }

     # Sum catches to annual-level per fisher
    F3 <- F2 %>% 
      group_by(year, cml_no.fs, bf_registr, cml_registr, county) %>%
      summarize_if(is.numeric, sum)
    
    # Re-classify "trip_type" into "fisher_type"
    F3 <- F3 %>% 
      mutate(fisher_type = if_else(trip_type > 0, "Comm", "NC")) %>%
      select(-trip_type) %>% as.data.table()
    # Apply the annual-level filters to classify fishers as comm. vs non-comm.
    F3$annual_type <- "NC"

    if(filter_level_id == "Annual" | filter_level_id == "Both"){
      QT_annual <- QT_annual %>% 
          filter(quantiles == selected_quantile) %>% as.data.table()

      F3[d7 > QT_annual[sp_frs_id=="d7"]$value]$annual_type <- "Comm"
        
        if(filter_taxa_id == "All taxa" ){
        
          F3[s17 > QT_annual[sp_frs_id=="s17"]$value| 
            s19 > QT_annual[sp_frs_id=="s19"]$value| 
            s21 > QT_annual[sp_frs_id=="s21"]$value|
            s22 > QT_annual[sp_frs_id=="s22"]$value|
            s97 > QT_annual[sp_frs_id=="s97"]$value| 
            s20 > QT_annual[sp_frs_id=="s20"]$value 
            ]$annual_type <- "Comm"
        }
    }
    F3 <- F3 %>% 
    mutate(fisher_type=if_else(annual_type=="Comm","Comm",fisher_type))

    # Apply filters on the fishers
    if (input$only_bf_registered == "Y") { 
      F3 <- F3 %>% filter(bf_registr == "Y") 
    }
    
    F3 <- F3 %>% 
      filter(fisher_type == "NC")
    
    # Add the # of NC vessels by year x County to the catch data
    F3 <- F3 %>% 
      left_join(FC_sim, by = join_by(year, county), relationship = "many-to-many") %>% 
      relocate(n_bf_fishers, .after = county)

    return(list(
        QT_sim = QT_sim,
        FC_sim = FC_sim, 
        F3 = F3
    ))

})

QT_sim <- reactive({
  data_prep()$QT_sim
})

FC_sim <- reactive({
  data_prep()$FC_sim
})

F3 <- reactive({
  data_prep()$F3
})

output$n_bf_fishers_plot <- renderPlotly({
  F3 <- F3()

  total_fishers <- F3 %>% 
    distinct(year, county, n_bf_fishers) %>%
    filter(year < 2023) %>%
    group_by(year) %>%
    mutate(total_fishers = sum(n_bf_fishers)) %>%
    distinct(year, total_fishers) 
  county_fishers <- F3 %>% 
    distinct(year, county, n_bf_fishers) %>% 
    filter(year < 2023)  
  
  n_fishers_plot <- plot_ly() %>%
  add_trace(
    type = "bar",
    data = total_fishers, 
    x = ~year, 
    y = ~total_fishers,
    name = "Total number of BF fishers", 
    marker = list(color = "#D3D3D3"),
    width = 0.8
  )

  segments <- unique(county_fishers$county) 
  all_years <- sort(unique(county_fishers$year))
  colors <- c("Kauai County" = "#002364",
  "Maui County" = "#317F3F", "Hawaii County" = "#826087",
  "Honolulu County" = "#B71300")
 
  for (segment in segments) {

  segment_data <- filter(county_fishers, county == segment) %>%
                  mutate(offset = case_when(
                    county == "Honolulu County" ~ -.4, 
                    county == "Hawaii County" ~ -.2,
                    county == "Maui County" ~ 0,
                    county == "Kauai County" ~ .2,
                    ))

  n_fishers_plot <- n_fishers_plot %>%
    add_trace(
      type = "bar",
      x = segment_data$year,
      y = segment_data$n_bf_fishers,
      name = segment_data$county,
      marker = list(color = colors[segment_data$county],
                    line = list(color = "#D3D3D3", width = 1)),
      width = 0.2,
      offset =  segment_data$offset
      # hoverinfo = "text",
      # hovertext = paste0("Year: ", segment_data$year, 
      #                   "<br>Catch: ", format(round(segment_data$catch/1000, 1), big.mark=","), 
      #                   "K")
    )
  }
  # Configure layout
n_fishers_plot <- n_fishers_plot %>%
  layout(yaxis = list(title = "Number of Fishers", zeroline = FALSE),  
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

n_fishers_plot

})

run_sim <- eventReactive(sim_trigger(), {
  # Create empty results list to fill
  Results <- list() 
  # Get the actual data frame from the reactive function
  f3_data <- F3()
  
  # Wrap the entire loop in withProgress instead of each iteration
  withProgress(message = 'Running simulations', value = 0, {
    set.seed(1234)
    # Sample "n" times from the annual catch data set, where "n" is the number of
    # non-commercial BF fishers in a given Year x County.
    for (i in 1:100) {
      # Update progress bar
      incProgress(1/100, detail = paste("Iteration", i, "of", 100))
      
      aSample <- f3_data %>% # Use f3_data instead of F3
        group_by(year, county) %>% 
        sample_n(n_bf_fishers[1], replace=T) %>% 
        add_column(iter=i) %>% relocate(iter, .before=year)
      
      Results <- append(Results, list(aSample))
    }
  })
  
  # Bind results together and sum fisher-specific catch by iteration.
  Results <- rbindlist(Results)
  
  Final.County <- Results %>% group_by(iter, year, county) %>%
           summarize_at(vars(s15:allsp), sum) 
  
  # Global results
  Final.all <- Final.County %>% 
    group_by(iter,year) %>% 
    summarize_at(vars(s15:allsp),sum)

  # Species specific global results
  Final.all.sp <- Final.all %>% 
      pivot_longer(cols=s15:s97,names_to="species",values_to="lbs_caught") %>%
      select(iter, year, species, lbs_caught)

  return(
      list(
          Final.County = Final.County,
          Final.all = Final.all,
          Final.all.sp = Final.all.sp
      )
  )
}) #end of run_sim eventReactive

# Helper reactives to extract individual simulation objects  
Final.County <- reactive({
    req(run_sim())
    run_sim()$Final.County
})

Final.all <- reactive({
  req(run_sim())
  run_sim()$Final.all
})

Final.all.sp <- reactive({
  req(run_sim())
  run_sim()$Final.all.sp
})

 # For display parameters (not part of run_analysis)
  extras <- reactive({
    # These inputs can change without triggering the simulation
    list(
      prop_unreported = input$prop_unreported/100
    )
  })

# Plot Species-specific results
output$species_plot <- renderPlotly({
  req(Final.all.sp())

options <- extras() # currently only extra is if you want to plot underreporting
    
# First create data for Non-commercial - BFVR approach
non_commercial_data <- Final.all.sp() %>%
  group_by(year, species) %>% 
  summarise(catch = quantile(lbs_caught, .5)) %>% 
  mutate(type = "Non-commercial - BFVR approach") %>%
  filter(year < 2023)
  
# Decide whether to include unreported CML based on proportion
if (options$prop_unreported > 0) {
  # Create data with both regular CML and unreported CML and NC
  local_cml_all_sp <- cml.all.sp %>%
    mutate(catch = (catch * options$prop_unreported),
            type = "Commercial - CML unreported") %>% 
    bind_rows(cml.all.sp)
  
  # Combine all data
  plot_data <- bind_rows(non_commercial_data, local_cml_all_sp)
} else {
  # Only include the regular CML data (no unreported)
  plot_data <- bind_rows(non_commercial_data, cml.all.sp)
}
# Set type of catch as a factor to control order of bars in plot
# and set which colors to use for each type
colors <- c("Commercial - CML reported" = "#F09008FF", 
            "Commercial - CML unreported" = "#7868C0FF", 
            "Non-commercial - BFVR approach" = "#488820FF",
            "2024 Assessment total catch" = "grey")

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data$year))

plot_data <- plot_data %>%
  left_join(SP.id, by = c("species" = "sp_frs_id"), relationship = "many-to-many") %>% 
  filter(species != "d7") %>%
  mutate(type = factor(type, levels = c("Non-commercial - BFVR approach", 
  "Commercial - CML unreported", "Commercial - CML reported"))) %>%
  distinct(year, species, type, .keep_all = TRUE) 

# Create a list to store individual plots
plot_list <- list()
  # Basic layout settings for all plots
base_layout <- list(
  xaxis = list(
    tickmode = "array",
    tickvals = all_years,
    ticktext = all_years,
    tickangle = -45,
    dtick = 1
  ),
  hovermode = "closest",
  legend = list(title = list(text = "Type")),
  margin = list(t = 70)  # Increase top margin for title
)
common_name_vec <- unique(plot_data$common_name)
# Create a plot for each facet value
for (i in 1:length(common_name_vec)) { 
  
  current_sps <- common_name_vec[i]
  # Filter data for this facet
  facet_data <- plot_data %>% 
    filter(common_name == current_sps)
  tc.sp <- tc.all.sp %>% 
  filter(common_name == current_sps)

  facet_data$hover_text <- paste0("Year: ", facet_data$year, 
                                 "<br>Catch: ", format(round(facet_data$catch/1000, 1), big.mark=","), 
                                 "K")
  
  tc.sp$hover_text <- paste0("Year: ", tc.sp$year, 
                            "<br>Total catch: ", format(round(tc.sp$catch/1000, 1), big.mark=","), 
                            "K")
  
  # Create individual plot

      p <- plot_ly(data = facet_data, x = ~year, y = ~round(catch, digits = 0), color = ~type, 
                  type = "bar",
                  colors = colors,
                  showlegend = (i ==1),
                  text = ~hover_text,
                  hoverinfo = "text",
                  textposition = "none") %>%
            add_trace(data = tc.sp, x = ~year, y = ~catch, type = "scatter",
                  mode = "lines+markers",
                  line = list(color = "grey", width = 2),
                  marker = list(color = "grey", size = 3),
                  name = "Total catch used in the 2024 assessment", showlegend = (i ==1),
                  text = ~hover_text, hoverinfo = "text",
                  textposition = "none") %>%
        layout(
          annotations = list( 
              list( 
                x = 0.2,  
                y = 1.0,  
                text = current_sps,  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )),
        xaxis = base_layout$xaxis,
        yaxis = list(
        title = "Catch (lbs)",
        autorange = TRUE,
        hoverformat = ".0f"  
        ),
        hovermode = base_layout$hovermode,
        margin = base_layout$margin,
        barmode = 'stack'
      )                 
  
  plot_list[[i]] <- p
}

p.sp <- subplot(
        plot_list, 
        nrows = 3, 
        shareY = FALSE,
        titleX = FALSE,
        margin = 0.07  # Increase margin for titles
      ) %>%
  layout(
  annotations = list(
            list(
              text = "Catch (lbs)",
              textangle = -90,
              x = -.04,       # Position from left edge
              y = 0.5,        # Middle of plot area 
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 14)
            ),
            list(
              text = "Year",
              x = .5,       # Position from left edge
              y = 0,        # Middle of plot area 
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 14),
              yshift = -40  
          )),
    showlegend = TRUE, 
    legend = list(orientation = "h",
    x = 0.5, y = -0.1, xanchor = "center"))
  p.sp

}) #end of species-specific plots

# Create the dataframe that will be shared for Deep7 plot and ACL table
total_catch_df <- reactive({
  req(Final.all())

  options <- extras()

  # First create the data for Non-commercial - BFVR approach
  non_commercial_data <- Final.all() %>%
    group_by(year) %>% 
    summarise(catch = quantile(d7, .5)) %>% 
    mutate(type = "Non-commercial - BFVR approach") %>%
    filter(year < 2023)
    
  # Decide whether to include unreported CML based on proportion
  if (options$prop_unreported > 0) {
    # Create data with both regular CML and unreported CML and NC
    local_cml_all <- cml.all %>%
    mutate(catch = (catch * options$prop_unreported), 
    type = "Commercial - CML unreported") %>%
      bind_rows(cml.all)
    
    # Combine all data
    plot_data <- bind_rows(non_commercial_data, local_cml_all)
  } else {
    # Only include the regular CML data (no unreported)
    plot_data <- bind_rows(non_commercial_data, cml.all) 
  }

  # Set type of catch as a factor to control order of bars in plot
  # and set which colors to use for each type
  plot_data <- plot_data %>%
    mutate(type = factor(type, 
    levels = c("Non-commercial - BFVR approach", "Commercial - CML unreported", 
    "Commercial - CML reported")))

  return(plot_data)

}) #end of total_catch_df

# Create a total Deep7 plot
output$combined_plot <- renderPlotly({
  req(total_catch_df())
  plot_data <- total_catch_df()
  colors <- c("Commercial - CML reported" = "#F09008FF", 
              "Commercial - CML unreported" = "#7868C0FF", 
              "Non-commercial - BFVR approach" = "#488820FF",
              "2024 Assessment total catch" = "grey")

  # Get all unique years from the data to use as breaks
  all_years <- sort(unique(plot_data$year))
  segments <- unique(plot_data$type) 


  # Create interactive plot with plot_ly
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
                        "K")) %>% 
  add_trace(data = tc.all, x = ~year, y = ~catch, type = "scatter",
            mode = "lines+markers", 
            line = list(color = "grey", width = 2),
            marker = list(color = "grey", size = 3),
            name = "Total catch used in the 2024 assessment",
            hoverinfo = "text",
            hovertext = ~paste0("Year: ", year, 
                        "<br>Catch: ", format(round(catch/1000, 1), big.mark=","), 
                        "K")) 
for (segment in segments) {

  segment_data <- filter(plot_data, type == segment)

  plot <- plot %>%
    add_trace(
      type = "bar",
      x = segment_data$year,
      y = segment_data$catch,
      name = segment,
      marker = list(color = colors[segment],
                    line = list(color = "D3D3D3", width = 1)),
      width = 0.2,
      offset =  ifelse(segment == "Commercial - CML reported", -0.3, 
                     ifelse(segment == "Non-commercial - BFVR approach", 0.1, -0.1)),
      hoverinfo = "text",
      hovertext = paste0("Year: ", segment_data$year, 
                        "<br>Catch: ", format(round(segment_data$catch/1000, 1), big.mark=","), 
                        "K")
    )
}

  # Configure layout
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
  plot

}) # end of Deep 7 catch plot


output$acl_table <- reactable::renderReactable({

    req(total_catch_df())

     # Create a helper function to format values differently based on row
    formatCell <- function(value, row_index) {
      if (row_index == 3) {
        percent(value, accuracy = 0.1)
      } else {
        comma(value, accuracy = 1)
      }
    }
    
    recent_catch <- total_catch_df() %>% group_by(year) %>% filter(year >= 2018) %>% 
    summarise(total_catch = sum(catch)) %>% ungroup() %>%
    summarise(recent_catch = mean(total_catch)) %>% pull(recent_catch)

    model_management_table <- total_catch_df() %>% group_by(year) %>% 
      summarise(total_catch = sum(catch)) %>% 
      summarise(mean_catch = mean(total_catch)/1000) %>%
      mutate(biomass_2023 =( 0.022957 * mean_catch + 1.502262),
              ACL = 2.26047 * mean_catch + 15.97866,
              recent_catch = recent_catch/1000,
              percent_acl = (recent_catch/ACL))
    df <- data.frame(
      "type" = c("ACL (total catch)", "Recent catch", "Recent catch relative to ACL"),
      "Assessment_2024" = c(1105027, 395400, .36), # recent catch: TC %>% filter(Year < 2023 & Year >= 2018) %>% summarise(mean(d7))
      "New_Scenario" = c(model_management_table$ACL*1000, 
                  model_management_table$recent_catch*1000,
                  model_management_table$percent_acl))
    reactable(
      df,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        align = "center",
        headerStyle = list(fontWeight = "bold", background = "#f7f7f7")
      ),
      columns = list(
        type = colDef(
          name = "Measure",
          align = "left",
          minWidth = 200
        ),
        Assessment_2024 = colDef(
          name = "Assessment 2024",
          cell = function(value, index) {
            if (index == 3) {
              # Format as percentage for row 3
              percent(value, accuracy = 0.1)
            } else {
              # Format with commas for rows 1 and 2
              comma(value, accuracy = 1)
            }
          }
        ),
        New_Scenario = colDef(
          name = "New Scenario",
          cell = function(value, index) {
            if (index == 3) {
              # Format as percentage for row 3
              percent(value, accuracy = 0.1)
            } else {
              # Format with commas for rows 1 and 2
              comma(value, accuracy = 1)
            }
          }
        )
      ),
      theme = reactableTheme(
        borderColor = "#ddd",
        headerStyle = list(
          borderColor = "#555"
        )
      )
    ) 
  })

} #end of server


