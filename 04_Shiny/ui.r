# Define UI
ui <- page(
  title = "Deep7 Commercial and Non-Commercial Catch Analysis",
  
  # Create a tabbed interface
  navset_tab(
    # Introduction Tab
    nav_panel(
      title = "Introduction",
      card(
        card_header(
          h3("Welcome to the Deep7 Catch Analysis Tool", class = "text-center")
        ),
        card_body(
          div(
            style = "max-width: 800px; margin: 0 auto;",
            h4("About This Tool"),
            p("This application helps understand the impact of key decision points on the estimation of
            commercial and non-commercial catch of Deep7 bottomfish species in Hawaii."),
            p("The tool uses vessel counts from the Bottomfish Vessel Registry (BFVR) and catch reports
            from the Fisher Reporting System (FRS). It allows users to adjust various key decision points 
              on how to process these data sources and to see how that affects the final catch estimates. It
              then compares these estimates to the total catch used in the 2024 assessment."),
            
            h4("Key Decision Points"),
            tags$ul(
              tags$li(strong("Commercial Catch Adjustment:"), " Account for unreported commercial catches"),
              tags$li(strong("Non-commercial Fisher Count Estimation:"), " Adjust for unregistered vessels and inactive vessels"),
              tags$li(strong("FRS Data Filtering:"), " Select appropriate catch cutoff points to identify non-commercial proxy fishers")
            ),
            
            h4("How to Use"),
            p("1. Navigate to the 'Analysis' tab"),
            p("2. Adjust the parameters in the sidebar to reflect your assumptions"),
            p("3. Click the 'Run Analysis' button to generate results"),
            p("4. Review the analysis results in the main panel"),
            
            h4("Background"),
            p("The Deep7 bottomfish species are economically and culturally important to Hawaii. 
              Accurate catch estimates for both commercial and non-commercial sectors are essential 
              for sustainable fisheries management. This tool helps inform our estimates of the non-commercial 
              and commercial components based on the best available data."),
            
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
                        "This is the catch caught by CML holders but not reported in the FRS.
                        For example, catch that is sold via social media or unsold, where some 
                        fishers may not feel the need to enter it in the FRS.",
                        position = "right"), 
                      min = 0, max = 100, value = 0, step = 10, post = "%"),
          
          # SEPARATOR
          hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"), 

          h4("Number of active non-commercial fishers"),

          sliderInput("percent_inactive",
            label = tooltip("What percentage of BF-registered 
            boats are not actively trying to catch Deep7 in any given year?",
            "This is the percentage of boats that are registered in the BFVR
            but never go fishing for Deep7 or are simply not fishing.", 
            placement = "right"), 
            min = 0, max = 100, value = 0, step = 10, post = "%"), 
          
          sliderInput("percent_unregistered", 
                        label = tooltip("Of the boats actively fishing for Deep7, what percentage of 
                        boats are registered in the BFVR?",
                        "Based on your experience, how often do you see or hear about vessels fishing for Deep7
                        that are not registered in the BFVR?",
                        placement = "right"
                        ),
                     min = 25, max = 100, value = 100, step = 25, post = "%"),
   
          # SEPARATOR
          hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),
          
          h4("How should we select non-commercial fisher proxies from the FRS?"),
          
          radioButtons("only_bf_registered", 
                     "Only include data from fishers on the BF registry?",
                     choices = c("Yes" = "Y", "No" = "N"), 
                     selected = "Y"),
          
          radioButtons("which_filter_taxa_level",
                      "Should we classify fishers based only on their total Deep7 catch or also their catch by species?",
                      choices = c("Deep7 only", "All taxa"), 
                      selected = "All taxa"),

          selectInput("which_filter_level",
                    label = tooltip("At what level should we filter the catch data?",
                    "Should we use MRIP trip-level interviews and/or annual catch
                    estimates from the Lamson (2007) study.",
                    placement = "right"
                    ),
                    choices = c("Trip", "Annual", "Both"),
                    selected = "Trip"),
          
          selectInput("selected_quantile", 
                    label = tooltip("What cut off should we use?", 
                    "What cut off point should we use to select non-commercial fishers in the FRS.
                    For example, 95% would mean that fishers reporting a trip with a catch higher than 95% of all
                    non-commercial HMRFS interviews would be classified as commercial and filtered out.",
                    placement = "right"
                    ),
                    choices =  c("90%" = "q90", "95%" = "q95", 
                    "99%" = "q99", "Maximum" = "max"), 
                    selected = "q99"),
          
          actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        accordion(
        
        accordion_panel(
          title = "Number of Non Commercial BF Fishers by County",
          plotlyOutput("n_bf_fishers_plot")
        ),

        accordion_panel(
          title = "Deep7 Catch by Year",
          plotlyOutput("combined_plot")
        ),
        
        accordion_panel(
          title = "ACL Table",
          reactable::reactableOutput("acl_table")
        ),
        
        accordion_panel(
          title = "Deep7 Catch by Species",
          plotlyOutput("species_plot")
        ),
        
        # Control initial state - can be "first" (default), "all", or "none"
        open = "first"
        )
    
      )
    )
  )
)