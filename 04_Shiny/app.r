library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(ggthemes)
library(plotly)

#root_dir <- here(..=1)

QT <- fread(file.path("mrip_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")

# Get species codes and names for plots
SP.id <- QT[,1:3]

FC <- fread(file.path("Fisher_counts.csv")) %>%
        filter(cml_registr == "N")

F2 <- fread(file.path("FRS_trips_annon.csv"))

# Read in CML catches and tidy
cml <- fread(file.path("CML_catches.csv"))

cml.all.sp <- cml %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  mutate(type = "CML") 

cml.all <- cml %>% 
  mutate(catch = d7,
          type = "CML") %>% 
  select(c("year", "catch", "type"))

TC <- fread(file.path("Total_catches_2024.csv"))

tc.all <- TC %>% 
  select(c("Year", "d7")) %>% 
  rename("year" = "Year", "catch" = "d7") %>%
  mutate(type = "Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023) 

tc.all.sp <- TC %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  rename("year" = "Year") %>%
  mutate(type = "Assessment total catch",
  catch = as.numeric(catch)) %>%
  filter(year < 2023 & species != "d7") %>%
  left_join(SP.id, by = c("species" = "sp_frs_id")) 

# Define a common theme with improved font sizes
# my_theme <- theme_minimal() +
#   theme(
#     axis.title = element_text(size = 14, face = "bold"),
#     axis.text = element_text(size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#     strip.text = element_text(size = 12, face = "bold"),
#     legend.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 12),
#     plot.title = element_text(size = 16, face = "bold")
#   )

# Define UI
ui <- page_sidebar(
  title = "Deep7 Non-Commercial Catch Analysis",
  
  sidebar = sidebar(

    h4("CML catches"),

    sliderInput("prop_unreported", 
                 "What percentage of the commercial catch is unreported?", 
                  min = 0, max = 100, value = 0, step = 10, post = "%"),

          # SEPARATOR
    hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),

    h4("Number of active non-commercial fishers"),
    
    selectInput("multiplier_unregistered", 
                 "How many Deep 7 fishers don't register on the BFVR?", 
                 choices = list(
                    "They all register" = 1,
                    "Half as many as registered" = 1.5,
                    "Equal number as those registered" = 2
                 ),
                 selected = 1),

    sliderInput("proportion_inactive", 
                 "What percentage of Bottomfish registered 
                 boats are inactive?", 
                 min = 0, max = 100, value = 0, step = 10, post = "%"), 

          # SEPARATOR
    hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),

    h4("How should we select fishers from the FRS?"),

    radioButtons("only_bf_registered", 
                 "Should we only include BF-registered fishers?", 
                 choices = c("Yes" = "Y", "No" = "N"), 
                 selected = "Y"),
    
    selectInput("which_mrip_lamson",
                "At what level should we filter the catch data?",
                choices = c("Trip", "Annual", "Both"),
                selected = "Trip"),

    selectInput("selected_quantile", 
                "What cut off should we use?", 
                choices =  c("90%" = "q90", "95%" = "q95", 
                "99%" = "q99", "Maximum" = "max"), 
                selected = "99%"),

    actionButton("run_analysis", "Run Analysis", class = "btn-primary")
    
  ),
  
   layout_columns(
    card(
        card_header("Deep7 Non-Commercial Catch by Year"),
        plotlyOutput("combined_plot") 
    )
  ),
  layout_columns(
    card(
      card_header("Deep7 Non-Commercial Catch by Species"),
      plotlyOutput("species_plot", height = "500px")
    )
  )
 
)

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

    multiplier_unregistered <- as.numeric(input$multiplier_unregistered)
    # Select the quantile to use
    QT_sim <- QT %>% 
        filter(quantiles == selected_quantile) %>% as.data.table()
    
    # Apply the correction related to the assumed # of BF registered fishers 
    # catching at least one deep7 in any given year
    # and the percentage of boats that are not registered in the BFVR
    FC_sim <- FC %>% 
        mutate(n_bf_fishers = n_bf_fishers - (n_bf_fishers * (proportion_inactive/100)),
        n_bf_fishers = n_bf_fishers * multiplier_unregistered) 

## TODO: add Lamson option (radio button, MRIP/Lamson/both)
    # Create a new column for future data manipulation
    F2$trip_type <- 0
    F2[d7  > QT_sim[sp_frs_id=="d7"]$value|
      s17 > QT_sim[sp_frs_id=="s17"]$value| 
      s19 > QT_sim[sp_frs_id=="s19"]$value| 
      s21 > QT_sim[sp_frs_id=="s21"]$value|
      s22 > QT_sim[sp_frs_id=="s22"]$value|
      s97 > QT_sim[sp_frs_id=="s97"]$value| 
      s20 > QT_sim[sp_frs_id=="s20"]$value 
      ]$trip_type <- 1

     # Sum catches to annual-level per fisher
    F3 <- F2 %>% 
      group_by(year, cml_no.fs, bf_registr, cml_registr, county) %>%
      summarize(across(where(is.numeric), sum), .groups = "drop")
    
    # Re-classify "trip_type" into "fisher_type"
    F3 <- F3 %>% 
      mutate(fisher_type = if_else(trip_type > 0, "Comm", "NC")) %>%
      select(-trip_type)
    
    # Apply filters on the fishers
    if (input$only_bf_registered == "Y") { 
      F3 <- F3 %>% filter(bf_registr == "Y") 
    }
    
    F3 <- F3 %>% 
      filter(fisher_type == "NC")
    
    # Add the # of NC vessels by year x County to the catch data
    F3 <- F3 %>% 
      left_join(FC_sim, by = join_by(year, county)) %>% 
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
    for (i in 1:200) {
      # Update progress bar
      incProgress(1/200, detail = paste("Iteration", i, "of", 200))
      
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
  display_options <- reactive({
    # These inputs can change without triggering the simulation
    list(
      prop_unreported = input$prop_unreported/100
    )
  })

# Plot Species-specific results
output$species_plot <- renderPlotly({
  req(Final.all.sp())

options <- display_options()
    
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

# Create a list to store individual plots
plot_list <- list()
  # Basic layout settings for all plots
base_layout <- list(
  xaxis = list(
    #title = "Year",
    tickmode = "array",
    tickvals = all_years,
    ticktext = all_years,
    tickangle = 0,
    dtick = 1
  ),
  #yaxis = list(title = "Catch (lbs)"),
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

      p <- plot_ly(data = facet_data, x = ~year, y = ~catch, color = ~type, 
                  type = "bar",
                  colors = colors) %>%
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
        autorange = TRUE  
        ),
        hovermode = base_layout$hovermode,
        margin = base_layout$margin,
        barmode = 'stack'
      )                 
  
  plot_list[[i]] <- p
}

## TODO: why are species plot values not correct????
p.sp <- subplot(
        plot_list, 
        nrows = 3, 
        shareY = FALSE,
        titleX = FALSE,
        margin = 0.07  # Increase margin for titles
      ) %>%
  layout(title = 'Catch by Species',
  layout = list(autosize = TRUE),
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
            ))
  )
p.sp

})

# Create a total Deep7 plot
output$combined_plot <- renderPlotly({
  req(Final.all())

options <- display_options()

# First create the data for BFVR Non-Commercial
non_commercial_data <- Final.all() %>%
  group_by(year) %>% 
  summarise(catch = quantile(d7, .5)) %>% 
  mutate(type = "BFVR Non-Commercial") %>%
  filter(year < 2023)
  
# Decide whether to include unreported CML based on proportion
if (options$prop_unreported > 0) {
  # Create data with both regular CML and unreported CML and NC
  local_cml_all <- cml.all %>%
  mutate(catch = (catch * options$prop_unreported) + (non_commercial_data$catch * options$prop_unreported), 
  type = "Unreported") %>%
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
  mutate(type = factor(type, levels = c("BFVR Non-Commercial", "Unreported", "CML")))
colors <- c("CML" = "#FC8D62", 
            "Unreported" = "#8DA0CB", 
            "BFVR Non-Commercial" = "#66C2A5")

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data$year))

# Create interactive plot with plot_ly
p <-plot_ly(plot_data, x = ~year, y = ~catch, color = ~type, 
        type = "bar", 
        colors = colors) %>% 
        add_trace(data = tc.all, x = ~year, y = ~catch, type = "scatter",
        mode = "lines+markers",
        line = list(color = "grey", width = 2),
        marker = list(color = "grey", size = 3),
        name = "Total catch used in the 2024 assessment") %>%
        layout(yaxis = list(title = "Catch (lbs)"), 
        xaxis = list(title = "Year"), barmode = 'stack')
p

})

}

shinyApp(ui, server)
