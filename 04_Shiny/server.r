
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
    
    proportion_inactive <- input$proportion_inactive 
    only_bf_registered <- input$only_bf_registered 
    selected_quantile <- input$selected_quantile
    filter_id <- input$which_filter_level
    multiplier_unregistered <- (input$multiplier_unregistered/100)
    
    # Apply the correction related to the assumed # of BF registered fishers 
    # catching at least one deep7 in any given year
    # and the percentage of boats that are not registered in the BFVR
    FC_sim <- FC %>% 
        mutate(n_bf_fishers = n_bf_fishers - (n_bf_fishers * (proportion_inactive/100)),
        n_bf_fishers = n_bf_fishers * (1/multiplier_unregistered)) 

    # Create a new column for future data manipulation
    F2$trip_type <- 0

    if(filter_id == "Trip" | filter_id == "Both"){
      QT_trip <- QT_trip %>% 
        filter(quantiles == selected_quantile) %>% 
        as.data.table()
      F2[d7  > QT_trip[sp_frs_id=="d7"]$value|
      s17 > QT_trip[sp_frs_id=="s17"]$value| 
      s19 > QT_trip[sp_frs_id=="s19"]$value| 
      s21 > QT_trip[sp_frs_id=="s21"]$value|
      s22 > QT_trip[sp_frs_id=="s22"]$value|
      s97 > QT_trip[sp_frs_id=="s97"]$value| 
      s20 > QT_trip[sp_frs_id=="s20"]$value 
      ]$trip_type <- 1
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

    if(filter_id == "Annual" | filter_id == "Both"){
      QT_annual <- QT_annual %>% 
          filter(quantiles == selected_quantile) %>% as.data.table()
          F3[d7  > QT_annual[sp_frs_id=="d7"]$value|
            s17 > QT_annual[sp_frs_id=="s17"]$value| 
            s19 > QT_annual[sp_frs_id=="s19"]$value| 
            s21 > QT_annual[sp_frs_id=="s21"]$value|
            s22 > QT_annual[sp_frs_id=="s22"]$value|
            s97 > QT_annual[sp_frs_id=="s97"]$value| 
            s20 > QT_annual[sp_frs_id=="s20"]$value 
            ]$annual_type <- "Comm"
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
    mutate(catch = (catch * options$prop_unreported) + (non_commercial_data$catch * options$prop_unreported),
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
colors <- c("Commercial - CML reported" = "#FC8D62", 
            "Commercial - CML unreported" = "#8DA0CB", 
            "Non-commercial - BFVR approach" = "#66C2A5",
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
  
  # Create individual plot

      p <- plot_ly(data = facet_data, x = ~year, y = ~round(catch, digits = 0), color = ~type, 
                  type = "bar",
                  colors = colors,
                  hovertemplate = "Year: %{x} <br> Catch (lbs): %{y}") %>%
            add_trace(data = tc.sp, x = ~year, y = ~catch, type = "scatter",
                    mode = "lines+markers",
                    line = list(color = "grey", width = 2),
                    marker = list(color = "grey", size = 3),
                    name = "Total catch used in the 2024 assessment") %>%
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
        showlegend = (i == 1),  # Only show legend on first plot
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
              x = -.03,       # Position from left edge
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
          )) )
  p.sp

}) #end of species-specific plots

# Create a total Deep7 plot
output$combined_plot <- renderPlotly({
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
  mutate(catch = (catch * options$prop_unreported) + (non_commercial_data$catch * options$prop_unreported), 
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
colors <- c("Commercial - CML reported" = "#FC8D62", 
            "Commercial - CML unreported" = "#8DA0CB", 
            "Non-commercial - BFVR approach" = "#66C2A5",
            "2024 Assessment total catch" = "grey")

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data$year))

# Create interactive plot with plot_ly
p <-plot_ly(plot_data, x = ~year, y = ~round(catch, digits = 0), color = ~type, 
        type = "bar", 
        colors = colors,
        hovertemplate = "Year: %{x} <br> Catch (lbs): %{y}") %>% 
    add_trace(data = tc.all, x = ~year, y = ~catch, type = "scatter",
        mode = "lines+markers",
        line = list(color = "grey", width = 2),
        marker = list(color = "grey", size = 3),
        name = "Total catch used in the 2024 assessment") %>%
    layout(yaxis = list(title = "Catch (lbs)"), 
          xaxis = list(
            title = "Year",
            tickmode = "array",
            tickvals = all_years,
            ticktext = all_years,
            tickangle = 0,
            dtick = 1
          ), 
          yaxis = list(
          hoverformat = ".0f"
          ),
          legend = list(x = 1, y = 1,
          xanchor = "right",
          yanchor = "top"),
          barmode = 'stack') %>%
    # Add text annotations for the annual sum above each bar
    add_annotations(
      data = plot_data %>% group_by(year) %>% summarize(total = round(sum(catch), digits = 0)),
      x = ~year,
      y = ~total,
      text = ~paste(format(total, big.mark = ","), "lbs"),
      showarrow = FALSE,
      yshift = 16,  # Adjust as needed to position text above bars
      font = list(size = 14)
    )

}) # end of Deep 7 catch plot

} #end of server


