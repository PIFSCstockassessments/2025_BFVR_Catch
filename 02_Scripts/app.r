library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(ggthemes)
library(plotly)
#library(viridisLite)
#library(here)
#root_dir <- here(..=1)

QT <- fread(file.path("..","03_Outputs","mrip_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")

# Get species codes and names for plots
SP.id <- QT[,1:3]

FC <- fread(file.path("..","03_Outputs", "Fisher_counts.csv")) %>%
        filter(cml_registr == "N")

F2 <- fread(file.path("..","03_Outputs","FRS_trips_annon.csv"))

# Read in CML catches and tidy
cml <- fread(file.path("..", "03_Outputs", "CML_catches.csv"))

cml.all.sp <- cml %>% 
  pivot_longer(cols = d7:s97, names_to = "species", values_to = "catch") %>% 
  mutate(type = "CML")

cml.all <- cml %>% 
  mutate(catch = d7,
          type = "CML") %>% 
  select(c("year", "catch", "type"))


# Define a common theme with improved font sizes
my_theme <- theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  )

# Define UI
ui <- page_sidebar(
  title = "Deep7 Non-Commercial Catch Analysis",
  
  sidebar = sidebar(
    h4("Analysis Parameters"),
    
    numericInput("prop_caught_d7", 
                 "What percentage of Bottomfish registered 
                 boats catch Deep7?", 
                 value = 100, min = 0, max = 100, step = 10), 
    
    radioButtons("only_bf_registered", 
                 "Should we only include BF-registered fishers?", 
                 choices = c("Yes" = "Y", "No" = "N"), 
                 selected = "Y"),
    
    selectInput("selected_quantile", 
                "What percentile of catch from MRIP interviews should we keep?", 
                choices =  c("90%" = "q90", "95%" = "q95", 
                "99%" = "q99", "Maximum" = "max"), 
                selected = "99%"),
    
    actionButton("run_analysis", "Run Analysis", class = "btn-primary"),

          # SEPARATOR
    hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),
    
    # DISPLAY OPTIONS SECTION (not included in run_analysis)
    h4("Plotting Options"),

    # Add your new inputs here that won't be part of run_analysis
    numericInput("prop_unreported", 
                 "What proportion of the commercial catch is unreported?", 
                 value = 0, min = 0, max = 1, step = 0.1),
    
    # Add more display options as needed
    #checkboxInput("stack_bars", "Stack bars in plots", value = TRUE),
    
  ),
  
   layout_columns(
    card(
        card_header("Deep7 Non-Commercial Catch by Year"),
        plotOutput("combined_plot") 
    )
  ),
  layout_columns(
    card(
      card_header("Deep7 Non-Commercial Catch by Species"),
      plotOutput("species_plot")
    )
  )
 
)

# Define server
server <- function(input, output, session) {
  
# Reactive values to store results
results <- reactiveVal(NULL)
  
# Function to load data and run analysis
# In server
data_prep <- eventReactive(
    list(input$prop_caught_d7, 
    input$only_bf_registered, 
    input$selected_quantile), {
    
    # Select the quantile to use
    QT_sim <- QT %>% 
        filter(quantiles == input$selected_quantile) %>% as.data.table()
    
    # Apply the correction related to the assumed # of BF registered fishers 
    # catching at least one deep7 in any given year
    FC_sim <- FC %>% 
        mutate(n_bf_fishers = n_bf_fishers * input$prop_caught_d7/100)

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
      left_join(FC, by = join_by(year, county)) %>% 
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
      prop_unreported = input$prop_unreported
    )
  })

# Plot Species-specific results
output$species_plot <- renderPlot({
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

all_years <- sort(unique(plot_data$year)) 

plot_data %>% 
  left_join(SP.id, by = c("species" = "sp_frs_id"), 
  relationship = "many-to-many") %>% 
  mutate(type = factor(type, levels = c("CML", "Unreported", "BFVR Non-Commercial"))) %>% 
  filter(species != "d7" & year < 2023) %>%
  ggplot()+
  geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
  my_theme +
  labs(y="Year",x="Catch (lbs)")+
  facet_wrap(~common_name,ncol=2,scales = "free_y") +
  scale_fill_manual(values = c("CML" = "#FC8D62", 
                            "Unreported" = "#8DA0CB", 
                            "BFVR Non-Commercial" = "#66C2A5")) +
  scale_x_continuous(breaks = all_years) 

})

# Create a total Deep7 plot
output$combined_plot <- renderPlot({
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

# Get all unique years from the data to use as breaks
all_years <- sort(unique(plot_data$year))

plot_data %>%
  mutate(type = factor(type, levels = c("CML", "Unreported", "BFVR Non-Commercial"))) %>% 
  ggplot() +
  geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
  my_theme +
  theme(axis.text.x=element_text(angle=45,hjust=1),
          strip.text = element_text(size = 14))+
  scale_fill_manual(values = c("CML" = "#FC8D62", 
                            "Unreported" = "#8DA0CB", 
                            "BFVR Non-Commercial" = "#66C2A5")) +
  scale_x_continuous(breaks = all_years) + 
  labs(x="Year",y="Catch (lbs)")

# ggplotly(p, tooltip = "catch") %>%
#     layout(hoverlabel = list(bgcolor = "white", 
#                             font = list(family = "Arial", size = 12)))

})

}

shinyApp(ui, server)
