# Define UI
ui <- page(
  useShinyjs(),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('catchPlot_update_legendState', function(legendState) {
      Shiny.setInputValue('catchPlot_legendState', legendState);
    });
  ")),
  tags$head(
    tags$style(HTML("
      /* Increase font size for slider values */
      .irs-grid-text, .irs-min, .irs-max, .irs-single, .irs-from, .irs-to {
        font-size: 14px !important; /* Adjust size as needed */
      }
      
      /* Make the active/selected value even larger if desired */
      .irs-single, .irs-from, .irs-to {
        font-size: 16px !important; /* Adjust size as needed */
      }
    "))
  ),
  tags$head(
    tags$style(HTML("
      .compact-input {
        margin-bottom: -10px;
        margin-top: -10px;
      }
      .compact-input .irs {
        margin-top: 0;
        margin-bottom: 0;
      }
      .compact-input .control-label {
        margin-bottom: 0;
      }
    "))
  ),
  
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
              on how to process these data sources and to see how that affects the final non-commercial catch estimates. It
              then compares these estimates to the total catch used in the 2024 assessment."),
            
            h4("Key Decision Points"),
            tags$ul(
              tags$li(strong("Non-commercial fisher count:"), " Adjust for unregistered vessels and inactive vessels"),
              tags$li(strong("FRS data filtering:"), " Select appropriate catch cut-off points to identify non-commercial proxy fishers"),
              tags$li(strong("Commercial catch adjustment:"), " Account for unreported commercial catches")
            ),
            
            h4("How to Use"),
            p("1. Navigate to the 'Analysis' tab"),
            p("2. Adjust the parameters in the sidebar to reflect your assumptions"),
            p("3. Review the analysis results in the main panel"),
            
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
                   
          h4("Number of active non-commercial fishers"),

          sliderInput("percent_active",
            label = tooltip("What percentage of BF-registered 
            fishers are fishing for Deep7?",
            "This is the percentage of fishers registered in the BFVR
            that actually go fishing for Deep7.", 
            placement = "right"), 
            min = 0, max = 100, value = 100, step = 10, post = "%")|> 
          tagAppendAttributes(class = "compact-input"), 
          
          sliderInput("percent_unregistered", 
                        label = tooltip("What percentage of 
                        fishers fishing for Deep7 are BF-registered?",
                        "How often do you see or hear about people fishing for Deep7
                        that are not registered in the BFVR? If you select 100% that means everyone you see/hear about
                        fishing for Deep7 is registered on the BFVR. If you select 50%, that means about half of 
                        the people you see/hear about fishing for Deep7 are not registered.",
                        placement = "right"
                        ),
                     min = 50, max = 100, value = 100, step = 10, post = "%")|> 
          tagAppendAttributes(class = "compact-input"),
   
          # SEPARATOR
          #hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"),
          
          h4("Selecting \"non-commercial fishers\" from the FRS"),
          
          # radioButtons("only_bf_registered", 
          #            "Only include data from fishers on the BF registry?",
          #            choices = c("Yes" = "Y", "No" = "N"), 
          #            selected = "Y"),
          
          # radioButtons("which_filter_taxa_level",
          #             "Should we classify fishers based only on their total Deep7 catch or also their catch by species?",
          #             choices = c("Deep7 only", "All taxa"), 
          #             selected = "All taxa"),

          radioButtons("catch_cutoff", 
                    label = tooltip("What cut-off point should we use for catch?", 
                    "How much catch should we use as a cut-off point to select \"non-commercial\" fishers in the FRS.
                    For example, low cut-off would mean that all fishers reporting a trip with a catch higher than 
                    50 lbs per trip would be classified as commercial and filtered out.",
                    placement = "right"
                    ),
                    choices =  c("Low cut-off (50 lb/trip)" = "low", 
                    "Intermediate cut-off (70 lb/trip)" = "med",
                    "High cut-off (100 lb/trip)" = "high"), 
                    selected = "low") |> 
          tagAppendAttributes(class = "compact-input"),
          # selectInput("which_filter_level",
          #           label = tooltip("At what level should we apply our cut-off points?",
          #           "Should we use MRIP trip-level interviews and/or annual catch
          #           estimates from the Lamson (2007) study.",
          #           placement = "right"
          #           ),
          #           choices = c("Trip", "Annual", "Both"),
          #           selected = "Trip"),
           # SEPARATOR
          #hr(style = "border-top: 2px solid #2c3e50; margin-top: 20px; margin-bottom: 20px;"), 

          h4("Unreported commercial catch"),

          sliderInput("prop_unreported", 
                        label = tooltip("What percentage of the commercial catch is unreported?",
                        "This is the catch caught by CML holders but not reported in the FRS.
                        For example, catch that is sold via social media or unsold, where some 
                        fishers may not feel the need to enter it in the FRS.",
                        position = "right"), 
                      min = 0, max = 100, value = 0, step = 10, post = "%")|> 
          tagAppendAttributes(class = "compact-input")
          # actionButton("run_analysis", "Run Analysis", class = "btn-primary")
        ),
        accordion(
        
        accordion_panel(
          title = "How many active non-commercial Deep7 fishers are there?",
          tabsetPanel(
            id = "active_fishers_plot",
            tabPanel(
              "Honolulu",
              plotlyOutput("honolulu_fishers_plot")
            ),
          tabPanel(
            "Hawaii",
            plotlyOutput("hawaii_fishers_plot")
          ),
          tabPanel(
            "Kauai",
            plotlyOutput("kauai_fishers_plot")
          ),
          tabPanel(
            "Maui",
            plotlyOutput("maui_fishers_plot")
          ),
          tabPanel(
            "All",
            plotlyOutput("total_fishers_plot")
          )
          )
        ),

        accordion_panel(
          title = "What is the Deep7 catch by year?",
          plotlyOutput("combined_plot")
        ),
        
        accordion_panel(
          title = "What does the Annual Catch Limits (ACL) look like?",
          reactable::reactableOutput("acl_table")
        ),
        
        accordion_panel(
          title = "What are the catches by species?",
          tabsetPanel(
            id = "species_plots",
            tabPanel(
              "Opakapaka",
              plotlyOutput("opaka_plot")
            ),
          tabPanel(
            "Onaga",
            plotlyOutput("onaga_plot")
          ),
          tabPanel(
            "Ehu",
            plotlyOutput("ehu_plot")
          ),
          tabPanel(
            "Kalekale",
            plotlyOutput("kale_plot")
          ),
          tabPanel(
            "Gindai",
            plotlyOutput("gindai_plot")
          ),
          tabPanel(
            "Lehi",
            plotlyOutput("lehi_plot")
          ),
          tabPanel(
            "Hapu'upu'u",
            plotlyOutput("hapu_plot")
          )
          )
        ),
        
        # Control initial state - can be "first" (default), "all", or "none"
        open = "first"
        )
    
      )
    )
  )
)