library(shiny)
library(bslib)
library(tidyverse)
library(data.table)
library(ggthemes)
#library(DT)
library(viridisLite)
#library(here)
#root_dir <- here(..=1)

QT <- fread(file.path("..","03_Outputs","mrip_quantiles.csv")) %>%
        pivot_longer(cols = q90:max, names_to="quantiles", values_to="value")

FC <- fread(file.path("..","03_Outputs", "Fisher_counts.csv")) %>%
        filter(cml_registr == "N")

F2 <- fread(file.path("..","03_Outputs","FRS_trips.csv"))
##TODO: read in CML catch csvs (total and by species)

# Create a new column for future data manipulation
F2$trip_type <- 0

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
                 "Proportion of BF-registered boats that caught D7:", 
                 value = 1, min = 0, max = 1, step = 0.1), 

    # proportion of unreported catch, apply to CML and NC catches
    numericInput("prop_underreported", 
                 "Proportion of catch that is underreported", 
                 value = 1, min = 0, max = 1, step = 0.1), 
    
    radioButtons("only_bf_registered", 
                 "Only BF-registered fishers:", 
                 choices = c("Yes" = "Y", "No" = "N"), 
                 selected = "Y"),
    
    selectInput("selected_quantile", 
                "MRIP interview quantile:", 
                choices = c("q90", "q95", "q99", "max"), 
                selected = "q99"),
    
    numericInput("iterations", 
                 "Number of iterations:", 
                 value = 50, min = 10, max = 500, step = 10),
    
    actionButton("run_analysis", "Run Analysis", class = "btn-primary")
  ),
   layout_columns(
    card(
        card_header("Deep7 Non-Commercial Catch by Year"),
        plotOutput("combined_plot", height = "400px")
    )
  ),
  layout_columns(
    card(
      card_header("Deep7 Non-Commercial Catch by Species"),
      plotOutput("species_plot", height = "400px")
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
        mutate(n_bf_fishers = n_bf_fishers * input$prop_caught_d7)

## TODO: add Lamson option (radio button, MRIP/Lamson/both)

    F2$trip_type[F2$d7 > QT_sim[QT_sim$sp_frs_id == "d7", "value"] |
                F2$s17 > QT_sim[QT_sim$sp_frs_id == "s17", "value"] |
                F2$s19 > QT_sim[QT_sim$sp_frs_id == "s19", "value"] |
                F2$s21 > QT_sim[QT_sim$sp_frs_id == "s21", "value"] |
                F2$s22 > QT_sim[QT_sim$sp_frs_id == "s22", "value"] |
                F2$s97 > QT_sim[QT_sim$sp_frs_id == "s97", "value"] |
                F2$s20 > QT_sim[QT_sim$sp_frs_id == "s20", "value"]] <- 1

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


# Create a reactive trigger that changes whenever data_prep changes
  # or when simulation parameters change
sim_trigger <- reactive({
# Dependencies: filtered data and simulation inputs
list(
    #data_prep(),         # Trigger when filtered data changes
    #input$iterations,    # Trigger when sample size changes
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
    # TODO: add a seed for reproducibility?
    # Sample "n" times from the annual catch data set, where "n" is the number of
    # non-commercial BF fishers in a given Year x County.
    for (i in 1:input$iterations) {
      # Update progress bar
      incProgress(1/input$iterations, detail = paste("Iteration", i, "of", input$iterations))
      
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
           summarize_at(vars(s15:allsp), sum) #TODO: make sure no s20 (uku) are in code
  
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
  
# Plot Species-specific results
output$species_plot <- renderPlot({
  req(Final.all.sp())
  
  # Create a species-specific plot
  #TODO: add in reference lines (CML and MRIP non-sold)
  #TODO: add all species
Final.all.sp() %>%
group_by(year, species) %>% 
summarise(catch = quantile(lbs_caught, .5)) %>% 
mutate(type = "BFVR Non-Commercial") %>% 
#bind_rows(past_catch) %>%  #TODO: add CML
ggplot()+
      geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
      my_theme +
      labs(y="Year",x="Non-commercial catch (lbs)")+
      facet_wrap(~species,ncol=3) +
      scale_fill_viridis_d(option = "H") +
    guides(fill=FALSE)  # Remove redundant legend since we have facets
})

output$combined_plot <- renderPlot({
  req(Final.all())

    # Deep7 global results.
    Final.all() %>%
    group_by(year) %>% 
    summarise(catch = quantile(d7, .5)) %>% 
    mutate(type = "BFVR Non-Commercial") %>% 
    #bind_rows(past_catch) %>% #TODO: add CML and underreporting %
    ggplot()+
        geom_bar(aes(x = year, y=catch, group = type, fill = type), position="stack", stat="identity") +
        my_theme +
        theme(axis.text.x=element_text(angle=45,hjust=1),
                strip.text = element_text(size = 14))+
        scale_fill_viridis_d(option = "H") +
        labs(x="Year",y="Non-commercial catch (lbs)")

#TODO: add in reference lines (CML and MRIP non-sold)
#   Final.all() %>% 
#   group_by(year, species) %>% 
#   summarise(median = quantile(lbs_caught, .5)) %>% #TODO:calculate median
# ggplot(data=Final.all(),aes(x=factor(year),y=d7))+
#       geom_boxplot(fill="lightblue",alpha=0.6,outlier.size=0.6)+ #TODO: stacked barplot with CML and NC and underreporting %
#       my_theme(base_size=13)+
#       theme(axis.text.x=element_text(angle=45,hjust=1),
#             strip.text = element_text(size = 14))+
#       labs(y="Year",x="Non-commercial catch (lbs)")

})

}

shinyApp(ui, server)
