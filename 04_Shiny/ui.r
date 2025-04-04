# Define UI
ui <- page(
  title = "Deep7 Non-Commercial Catch Analysis",
  
  # Create a tabbed interface
  navset_tab(
    # Introduction Tab
    nav_panel(
      title = "Introduction",
      card(
        card_header(
          h3("Welcome to the Deep7 Non-Commercial Catch Analysis Tool", class = "text-center")
        ),
        card_body(
          div(
            style = "max-width: 800px; margin: 0 auto;",
            h4("About This Tool"),
            p("This application helps estimate the non-commercial catch of Deep7 bottomfish species in Hawaii."),
            p("The tool combines data from multiple sources and allows users to adjust various parameters 
              that affect the final estimates."),
            
            h4("Key Features"),
            tags$ul(
              tags$li(strong("Commercial Catch Adjustment:"), " Account for unreported commercial catches"),
              tags$li(strong("Fisher Count Estimation:"), " Adjust for unregistered vessels and inactive registered vessels"),
              tags$li(strong("FRS Data Filtering:"), " Select appropriate filtering levels and percentile cutoffs")
            ),
            
            h4("How to Use"),
            p("1. Navigate to the 'Analysis' tab"),
            p("2. Adjust the parameters in the sidebar to reflect your assumptions"),
            p("3. Click the 'Run Analysis' button to generate results"),
            p("4. Review the analysis results in the main panel"),
            
            h4("Background"),
            p("The Deep7 bottomfish species are economically and culturally important to Hawaii. 
              Accurate catch estimates for both commercial and non-commercial sectors are essential 
              for sustainable fisheries management. This tool helps estimate the non-commercial 
              component based on the best available data."),
            
            hr(),
            p(class = "text-center", "Click on the 'Analysis' tab above to begin.")
          )
        )
      )
    ),
    
    # Main Analysis Tab
    nav_panel(
      title = "Analysis",
      page_sidebar(
        sidebar = sidebar(
          width = 400,
          h4("CML catches"),
          
          sliderInput("prop_unreported", 
                        label = tooltip("What percentage of the commercial catch is unreported?",
                        "For example catch that is sold via social media.",
                        position = "right"), 
                      min = 0, max = 100, value = 0, step = 10, post = "%"),
          
          # SEPARATOR
          hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),
          
          h4("Number of active non-commercial fishers"),
          
          sliderInput("multiplier_unregistered", 
                        label = tooltip("What percentage of boats fishing for bottomfish are registered in the BFVR?",
                        "Based on your experience, how many boats do you see fishing for bottomfish
                        that are not registered in the BFVR compared to boats that are registered? 
                        It is assumed that if the boat has a BF sticker, it is registered in the BFVR. For example,
                        if every boat you see has a BF sticker, choose 100%, if about half of the boats have a BF sticker,
                        choose 50%.",
                        placement = "right"
                        ),
                     min = 25, max = 100, value = 100, step = 25, post = "%"),
          
          sliderInput("proportion_inactive",
                      label = tooltip("What percentage of Bottomfish registered 
                     boats are inactive?",
                     "This is the percentage of boats that are registered in the BFVR
                     but never go fishing for bottomfish or are no longer fishing.", 
                     placement = "right"), 
                     min = 0, max = 100, value = 0, step = 10, post = "%"), 
          
          # SEPARATOR
          hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),
          
          h4("How should we select fishers from the FRS?"),
          
          radioButtons("only_bf_registered", 
                     "Should we only include BF-registered fishers?",
                     choices = c("Yes" = "Y", "No" = "N"), 
                     selected = "Y"),
          
          selectInput("which_filter_level",
                    label = tooltip("At what level should we filter the catch data?",
                    "Should we filter out extreme catches by total catch per year 
                    or per fishing trip?",
                    placement = "right"
                    ),
                    choices = c("Trip", "Annual", "Both"),
                    selected = "Trip"),
          
          selectInput("selected_quantile", 
                    label = tooltip("What cut off should we use?", 
                    "At what point should we remove extremly high catches. For example, 
                    95% would mean that we take 95% percent of the catches and remove 
                    the top 5% of really high catches. Note that Maximum means that no catches
                    will be removed.",
                    placement = "right"
                    ),
                    choices =  c("90%" = "q90", "95%" = "q95", 
                    "99%" = "q99", "Maximum" = "max"), 
                    selected = "q99"),
          
          actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
  
    card(
        card_header("Deep7 Catch by Year"),
        card_body(
          plotlyOutput("combined_plot")
        )
    ),
    card(
      card_header("Deep7 Catch by Species"),
      card_body(
        plotlyOutput("species_plot")
      )
    )
      )
    )
  )
)