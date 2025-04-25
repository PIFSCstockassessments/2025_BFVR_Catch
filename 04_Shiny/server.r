
# Define server
server <- function(input, output, session) {
  
# Reactive values to store results
results <- reactiveVal(NULL)

# Create a reactive trigger that changes whenever run 
# analysis button is clicked
# sim_trigger <- reactive({
# # Dependencies: filtered data and simulation inputs
# list(
#     input$run_analysis      # Also allow manual triggering
# )
# # Return the current time as a convenient non-NULL value
# return(Sys.time())
# })

FC_sim <- reactive({

  percent_active <- (input$percent_active/100)
  percent_unregistered <- (input$percent_unregistered/100)
  # Apply the correction related to the assumed # of BF registered fishers 
  # catching at least one deep7 in any given year
  # and the percentage of boats that are not registered in the BFVR
  FC_sim <- FC %>% 
      mutate(n_bf_fishers = n_bf_fishers * percent_active,
      n_bf_fishers = n_bf_fishers + n_bf_fishers * (1 - percent_unregistered)) 

})

output$honolulu_fishers_plot <- renderPlotly({
  req(FC_sim)
  FC <- FC_sim()
  
  create_county_n_fishers_plot(FC, "Honolulu County", "#B71300")

})

output$hawaii_fishers_plot <- renderPlotly({
  req(FC_sim)
  FC <- FC_sim()
  
  create_county_n_fishers_plot(FC, "Hawaii County", "#826087")

})

output$kauai_fishers_plot <- renderPlotly({
  req(FC_sim)
  FC <- FC_sim()
  
  create_county_n_fishers_plot(FC, "Kauai County", "#002364")

})

output$maui_fishers_plot <- renderPlotly({
  req(FC_sim)
  FC <- FC_sim()
  
  create_county_n_fishers_plot(FC, "Maui County", "#317F3F")

})

output$total_fishers_plot <- renderPlotly({
  req(FC_sim)
  FC <- FC_sim()
  
  total_fishers <- FC %>% 
    #distinct(year, county, n_bf_fishers) %>%
    filter(year < 2023) %>%
    group_by(year) %>%
    summarise(total_fishers = sum(n_bf_fishers))
  all_years <- sort(unique(total_fishers$year))
  
  plot_ly() %>%
      add_trace(
        type = "bar",
        data = total_fishers, 
        x = ~year, 
        y = ~total_fishers,
        name = "Total number of BF fishers", 
        marker = list(color = "#696969"),
        width = 0.8
      ) %>%
  layout(yaxis = list(title = "Number of active non-commercial Deep7 fishers", zeroline = FALSE,
                      range  = list(0,800)),  
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

})

# Function to load data and run analysis
# In server
data_prep <- reactive({
    req(FC_sim())
    FC_sim <- FC_sim()
    #only_bf_registered <- input$only_bf_registered 
    catch_cutoff <- input$catch_cutoff
    filter_level_id <- "Trip" #input$which_filter_level
    # filter_taxa_id <- input$which_filter_taxa_level
        
    # Create a new column for future data manipulation
    F2$trip_type <- 0

    if(filter_level_id == "Trip" | filter_level_id == "Both"){
      # QT_trip <- QT_trip %>% 
      #   filter(quantiles == catch_cutoff) %>% 
      #   as.data.table()

      if(catch_cutoff == "low"){
        F2[d7 > 50]$trip_type <- 1
      }
      if(catch_cutoff == "med"){
        F2[d7 > 70]$trip_type <- 1
      }
      if(catch_cutoff == "high"){
        F2[d7 > 100]$trip_type <- 1
      }
      # if(filter_taxa_id == "All taxa" ){
  
        # F2[s17 > QT_trip[sp_frs_id=="s17"]$value| 
        #     s19 > QT_trip[sp_frs_id=="s19"]$value| 
        #     s21 > QT_trip[sp_frs_id=="s21"]$value|
        #     s22 > QT_trip[sp_frs_id=="s22"]$value|
        #     s97 > QT_trip[sp_frs_id=="s97"]$value| 
        #     s20 > QT_trip[sp_frs_id=="s20"]$value 
        #   ]$trip_type <- 1
      # }
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

    # if(filter_level_id == "Annual" | filter_level_id == "Both"){
    #   # QT_annual <- QT_annual %>% 
    #   #     filter(quantiles == catch_cutoff) %>% as.data.table()
    #   if(catch_cutoff == "low"){
    #     F3[d7 > 450]$annual_type <- "Comm"
    #   }else{
    #     F3[d7 > 500]$annual_type <- "Comm"
    #   }

        # if(filter_taxa_id == "All taxa" ){
        
        #   F3[s17 > QT_annual[sp_frs_id=="s17"]$value| 
        #     s19 > QT_annual[sp_frs_id=="s19"]$value| 
        #     s21 > QT_annual[sp_frs_id=="s21"]$value|
        #     s22 > QT_annual[sp_frs_id=="s22"]$value|
        #     s97 > QT_annual[sp_frs_id=="s97"]$value| 
        #     s20 > QT_annual[sp_frs_id=="s20"]$value 
        #     ]$annual_type <- "Comm"
        # }
    #}
    F3 <- F3 %>% 
    mutate(fisher_type=if_else(annual_type=="Comm","Comm",fisher_type))

    # Apply filters on the fishers
    # if (input$only_bf_registered == "Y") { 
    #   F3 <- F3 %>% filter(bf_registr == "Y") 
    # }
    
    F3 <- F3 %>% 
      filter(fisher_type == "NC")
    
    # Add the # of NC vessels by year x County to the catch data
    F3 <- F3 %>% 
      left_join(FC_sim, by = join_by(year, county), relationship = "many-to-many") %>% 
      relocate(n_bf_fishers, .after = county)

    return(list(
        QT_sim = QT_sim,
        F3 = F3
    ))

})

QT_sim <- reactive({
  data_prep()$QT_sim
})


F3 <- reactive({
  data_prep()$F3
})

run_sim <- reactive({
  # Create empty results list to fill
  Results <- list() 
  # Get the actual data frame from the reactive function
  f3_data <- F3()
  
  # Wrap the entire loop in withProgress instead of each iteration
  withProgress(message = 'Running simulations', value = 0, {
    set.seed(1234)
    # Sample "n" times from the annual catch data set, where "n" is the number of
    # non-commercial BF fishers in a given Year x County.
    for (i in 1:50) {
      # Update progress bar
      incProgress(1/50, detail = paste("Iteration", i, "of", 50))
      
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

# create species-specific catch df for plots
total_catch_sp <- reactive({
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
  plot_data <- plot_data %>%
  left_join(SP.id, by = c("species" = "sp_frs_id"), relationship = "many-to-many") %>% 
  mutate(type = factor(type, levels = c("Non-commercial - BFVR approach", 
  "Commercial - CML unreported", "Commercial - CML reported"))) %>%
  distinct(year, species, type, .keep_all = TRUE) 
})

# Plot Species-specific results
output$opaka_plot <- renderPlotly({
  req(total_catch_sp())
  opaka_catch <- total_catch_sp() %>% filter(species == "s19")
  tc.opaka <- tc.all.sp %>% filter(species == "s19")
  create_layered_catchplot(opaka_catch, tc.opaka, catch_colors)
})

output$onaga_plot <- renderPlotly({
  req(total_catch_sp())
  onaga_catch <- total_catch_sp() %>% filter(species == "s22")
  tc.onaga <- tc.all.sp %>% filter(species == "s22")
  create_layered_catchplot(onaga_catch, tc.onaga, catch_colors)
})

output$ehu_plot <- renderPlotly({
  req(total_catch_sp())
  ehu_catch <- total_catch_sp() %>% filter(species == "s21")
  tc.ehu <- tc.all.sp %>% filter(species == "s21")
  create_layered_catchplot(ehu_catch, tc.ehu, catch_colors)
})

output$kale_plot <- renderPlotly({
  req(total_catch_sp())
  Kale_catch <- total_catch_sp() %>% filter(species == "s17")
  tc.Kale <- tc.all.sp %>% filter(species == "s17")
  create_layered_catchplot(Kale_catch, tc.Kale, catch_colors)
})

output$gindai_plot <- renderPlotly({
  req(total_catch_sp())
  gindai_catch <- total_catch_sp() %>% filter(species == "s97")
  tc.gindai <- tc.all.sp %>% filter(species == "s97")
  create_layered_catchplot(gindai_catch, tc.gindai, catch_colors)
})

output$hapu_plot <- renderPlotly({
  req(total_catch_sp())
  hapu_catch <- total_catch_sp() %>% filter(species == "s15")
  tc.hapu <- tc.all.sp %>% filter(species == "s15")
  create_layered_catchplot(hapu_catch, tc.hapu, catch_colors)
})

output$lehi_plot <- renderPlotly({
  req(total_catch_sp())
  lehi_catch <- total_catch_sp() %>% filter(species == "s58")
  tc.lehi <- tc.all.sp %>% filter(species == "s58")
  create_layered_catchplot(lehi_catch, tc.lehi, catch_colors)
})

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

legendState <- reactiveVal(NULL)

# Create a total Deep7 plot
output$combined_plot <- renderPlotly({
  req(total_catch_df())
  plot_data <- total_catch_df()
  

  p <- create_layered_catchplot(plot_data, tc.all, catch_colors, legendState(), source_id = "combined_plot") 
  p <- event_register(p, "plotly_legendclick")
  return(p)
  # p %>% layout(yaxis = list(tickmode = "linear",
  #           tick0 = 0,
  #           dtick = 50000,
  #           tickformat =  ",~s"))

}) # end of Deep 7 catch plot

 # Observer for legend clicks
  observeEvent(event_data("plotly_legendclick", source = "combined_plot"), {
    click_data <- event_data("plotly_legendclick", source = "combined_plot")
    req(total_catch_df())
    plot_data <- total_catch_df()
    # Get the name of the clicked trace
    clicked_name <- click_data$name
    
    # Update the legend state
    current_state <- legendState()
    if(is.null(current_state)) {
      # Initialize if null
      segments <- unique(plot_data$type)
      current_state <- list(
        "Total catch used in the 2024 assessment" = 'legendonly',
        'Total Catch' = 'legendonly',
        "Commercial - CML unreported" = 'legendonly'
      )
      for(segment in segments) {
        current_state[[segment]] <- ifelse(segment == "Non-commercial - BFVR approach", TRUE, 'legendonly')
      }
    }
    
    # Toggle visibility
    current_state[[clicked_name]] <- if(current_state[[clicked_name]] == TRUE) 'legendonly' else TRUE
    
    # Update the reactive value
    legendState(current_state)
  })

output$acl_table <- reactable::renderReactable({

    req(total_catch_df())
    
    recent_catch <- total_catch_df() %>% 
      filter(year >= 2018 & type == "Commercial - CML reported") %>% 
      summarise(recent_catch = mean(catch)) %>% pull(recent_catch)

    recent_cml_prop <- total_catch_df() %>% 
    pivot_wider(names_from = "type", values_from = "catch") %>%
    mutate(total_catch = rowSums(across(2:last_col())),
       cml_prop = `Commercial - CML reported`/total_catch) %>% 
    #filter(year >= 2018) %>%
    summarise(mean_cml_prop = mean(cml_prop)) %>% pull(mean_cml_prop)

    model_management_table <- total_catch_df() %>% group_by(year) %>% 
      #filter(year >= 2018 & year < 2023) %>%
      summarise(total_catch = sum(catch)) %>% 
      summarise(mean_catch = mean(total_catch)/1000) %>%
      mutate(biomass_2023 =( 0.022997 * mean_catch + 1.502262),
              ACL_total = (0.002265 * mean_catch + 0.01598)*1000,
              ACL = (ACL_total * recent_cml_prop) - 40, #-40 for demonstration purposes to get the ACL closer to assessment ACL 
              recent_catch = recent_catch/1000,
              percent_acl = (recent_catch/ACL),
              percent_recent = recent_catch/mean_catch)
    df <- data.frame(
      "type" = c("ACL (reported commercial catch)", "Recent reported commercial catch (2018-2022)",
      "Recent reported commercial catch (2018-2022) relative to total catch", 
      "Recent reported commercial catch (2018-2022) relative to ACL"),
      "Assessment_2024" = c(493000, 186360, .47, .38), # recent catch: cml %>% filter(year < 2023 & year >= 2018) %>% summarise(mean(d7)), rep comm catch to total catch: cml %>% filter(year < 2023 & year >= 2018) %>% summarise(mean(d7))/TC %>% filter(Year < 2023 & Year >= 2018) %>% summarise(mean(d7))
      "New_Scenario" = c(model_management_table$ACL*1000, 
                  model_management_table$recent_catch*1000,
                  model_management_table$percent_recent,
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
            if (index == 3 | index == 4) {
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
            if (index == 3| index == 4) {
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


