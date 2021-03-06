dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$span(class = "mytitle", "Human Health and Living Near Livestock Production Facilities"), 
    titleWidth = 630
  ),
  ## menu ####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Background", tabName = "start", icon = icon("piggy-bank")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("chalkboard-teacher")),
      menuItem("About the Studies", tabName = "eda", icon = icon("chart-bar")),
      # menuItem("Forest Plot", tabName = "forest", icon = icon("tree")),
      menuItem("Health Outcomes", tabName = "outcome", icon = icon("list"),
               menuItem("Lower Respiratory", tabName = "low_rsp",
                        menuItem("Overview", tabName = "low_rsp_intro"),
                        menuItem("Forest Plot", tabName = "low_rsp_forest"),
                        menuItem("Risk of Bias", tabName = "low_rsp_risk_of_bias"),
                        menuItem("Interpretation", tabName = "low_rsp_conclusion")),
               menuItem("Upper Respiratory", tabName = "up_rsp",
                        menuItem("Overview", tabName = "up_rsp_intro"),
                        menuItem("Forest Plot", tabName = "up_rsp_forest"),
                        menuItem("Risk of Bias", tabName = "up_rsp_risk_of_bias"),
                        menuItem("Interpretation", tabName = "up_rsp_conclusion")),
               menuItem("Antimicrobial resistance", tabName = "ar_rsp",
                        menuItem("Overview", tabName = "ar_rsp_intro"),
                        menuItem("Forest Plot", tabName = "ar_rsp_forest"),
                        menuItem("Risk of Bias", tabName = "ar_rsp_risk_of_bias"),
                        menuItem("Interpretation", tabName = "ar_rsp_conclusion")),
               menuItem("Gastrointestinal diseases", tabName = "gi_rsp",
                        menuItem("Overview", tabName = "gi_rsp_intro"),
                        menuItem("Forest Plot", tabName = "gi_rsp_forest"),
                        menuItem("Risk of Bias", tabName = "gi_rsp_risk_of_bias"),
                        menuItem("Interpretation", tabName = "gi_rsp_conclusion")),
               menuItem("Neurologic", tabName = "Neur_rsp",
                        menuItem("Overview", tabName = "Neur_rsp_intro"),
                        menuItem("Forest Plot", tabName = "Neur_rsp_forest"),
                        menuItem("Risk of Bias", tabName = "Neur_rsp_risk_of_bias"),
                        menuItem("Interpretation", tabName = "Neur_rsp_conclusion"))),
      # ),
      menuItem("References", tabName = "ref", icon = icon("book")),
      # selectInput("class",
      #             "Outcome class",
      #             choices = class_var,
      #             selected = class_var[1]),
      #uiOutput("measure"),
      #uiOutput("expo_var_1"),
      #uiOutput("expo_var_2"),
      id = "sidebar"
    )
  ),
  dashboardBody(
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      ## background ####
      tabItem(tabName = "start",
              fluidRow(
                box(
                  width = 12, solidHeader = TRUE, status = "primary",
                  title = "Living systematic review of effects of animal production on the health of surrounding communities",
                  fluidRow( column(width = 7, h4("What is a Living Systematic Review?"),
                                   br(), 
                                   p("A living systematic reviews is a review that is frequently updated, incorporating relevant new evidence as it becomes available.
                  This term means that rather than being a static publication in a peer reviewed journal, 
                  the review is housed on this website allowing for more timely updates and more accessible information."),
                  #                  p("This process can be visualized in the this animation. Our last static systematic review about this topic was published in 2017 based on  16 relevant studies published before that year. 
                  #   However, during the last 3 years, new studies may have been published and would need to be incorporated in  new systematic reviews. These new reviews are published on this website and updated periodically."),
                  #                  
                  #                  p("Through this website producers, public health officers, community leaders and community members can access
                  # the latest summary of the available studies and a balance interpretation of the findings and their implications
                  # in the wider body of literature will better serve the needs of the community because it"),
                                   p("1. Democratizes access to the information and interpretation, and"),
                                   p("2. Provides for more timely and relevant update"),
                                   
                  ),
                  br(),
                  column(width = 5,div(img(src = "lsr1.gif", height = 320, width = 380), style="text-align: center;")
                  )),
                  hr(),
                
                  fluidRow(column(width = 7, h4("Community Health and Animal Facilities"),
                                  br(),                
                                  p("In recent years there have been a growing concern about the effects that animal facilities could have on nearby communities. 
                  Regarding the swine industry, it has been suggested that facilities that confine animals indoors for feeding might represent a health
                  hazard for surrounding communities due to the exposition to odors, emissions and other harmful agents."),
                                  
                                  
                                  p("The results showed in this website correspond to those published in our last systematic review published in 2017 approaching the association between human health issues and animal production. Given that additional studies have been, and continue to be, conducted to identify the relationship between proximity to livestock facilities and health outcomes in the surrounding communities our plan is create a living literature review of the research reports that study this association. Every time the living review is updated, the results will be displayed on this website so that the public will be able to access the latest summary of the available studies and a balance interpretation of the findings."),
                                  p("Currently, we are developing the protocol that will guide the second update of the systematic review and the implementation of the living systematic review."),
                                  #hr()
                  ),
                  
                  br(),
                  column(width = 5,div(img(src = "cafo.jpg", height = 330, width = 380),align = "center"),
                  )),
                  #p("In this sense we have performed two systematic reviews summarizing the findings of publications investigating this topic.
                  #These previous studies can be consulted by clicking on the following links:"),
                  #p("1.", a("First Systematic Review", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),
                  #p("2.", a("Second Systematic Review", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),
                  hr(),
                  
                  
                  
                  column(width = 12,h4("Our Previous Systematic Reviews"),
                         p("We had performed two systematic review summarizing the findings of publications investigating this topic.
                  These previous systematic review can be consulted by clicking on the following links:"),
                         p("1.", a("First Systematic Review", href = "https://journals.plos.org/plosone/article/authors?id=10.1371/journal.pone.0009530")),
                         p("2.", a("Second Systematic Review (First Update)", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),       
                         p("Our last review (First Update) was published in 2017 and its objective was to update a systematic review of associations
                    between living near an animal feeding operation (AFO) and human health.Our research question was:"),
                         tags$blockquote("What are the associations between animal feeding operations and measures of the health of individuals 
                     living near animal feeding operations, but not actively engaged in livestock production?"),
                         #tags$iframe(width="560", height="315", src="https://www.youtube.com/watch?v=WgJWrHFgh8s", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                         
                  ))
              )),
      ### Tutorial
      tabItem(tabName = "tutorial",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "How to navigate through this website?",
                    fluidRow(
                      column(width = 12,h4("How to navigate the website?"),
                             br(),
                             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/doMYhSZeVvs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                      )
                    ),
                    br(),
                    hr(),
                    fluidRow( column(width = 12, h4("How to use the forest plot graph?"),
                                     br(),
                    p("The reported effect measures were either regression coefficients (βs) or prevalence ORs and prevalence ratios. In the forest plot (left side), 
                      each point represents the reported effect measure (e.g. an odds ratio) for a specific exposure-outcome relationship."),
                    p("The gray columns in the forest plot’s left side group the exposures in two categories. The second column from left to right indicates if the exposure evaluated involved direct contact with the individuals or it is surrogated. The first column from left to right groups the exposures according to type of exposure 
                      (i.e distance, gases, odor and aerosols)."),
                    p(" The table on the right side shows the risk of bias assessment for the specific point selected previously on the forest plot. For further details about risk of bias assessment displayed in the table on the right side", a("click here", href = "https://www.bristol.ac.uk/population-health-sciences/centres/cresyda/barr/riskofbias/"), 
                      "Once you click on one a row, a table is going to pop-up to provide more details about the judgment made by the authors for that particular exposure-outcome relationship.  "),
                    p("The video below  provides more details about the usage of the forest plot and its annexes."),
                    column(width = 12,
                           br(),
                           HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/BPUWWaVx0k0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                    )
                    )),
                    br(),
                    hr()
                    ))),
      
      ## about the studies ####
      tabItem(tabName = "eda",
              #fluidRow(
              # box(width = 12,
              #    h4("Brief Summary"),
              #   p("The literture about human health impacts of living near production animals is quite limited. After conducting an exhaustive search, our team identified 16 studies consisting of 10 study populations to include in the analysis.
              #    Those 16 studies were conducted in only three countries. The health outcomes were lower and upper respiratory tracts, antibiotic resistance, other infectious disease, neurological, 
              #        psychological, dermatological, otologic, ocular, gastrointestinal, stress and mood, and other non-infectious health outcomes."),
              #    #column(width = 12,
              #    checkboxInput("chk", label = "Brief studies: ", value = T),
              #    useShinyjs(),
              #    box(width = 12, DT::dataTableOutput("mytable1234"))
              
              #    )),
              #  hr(),
              fluidRow(
                box(width = 12,
                    h4("Introduction"),
                    p("The literture about human health impacts of living near production animals is quite limited. After conducting an exhaustive search, our team identified 16 studies consisting of 10 study populations to include in the analysis.
                    Those 16 studies were conducted in only three countries. The health outcomes were lower and upper respiratory tracts, MRSA, other infectious disease, neurological, 
                    psychological, dermatological, otologic, ocular, gastrointestinal, stress and mood, and other non-infectious health outcomes.
                      The health outcomes displayed in the side bar were selected because these were analyzed in more than two papers (see Timeline)."),
                    p("Please click the following check box to visualize the titles, year of publication and origin of the those studies. "),
                    
                    #column(width = 12,
                    #checkboxInput("chk", label = "Hide table ", value = T),
                    #useShinyjs(),
                    #box(width = 12, DT::dataTableOutput("mytable1234")),
                    #DT::dataTableOutput("mytable1234"),
                    hr(),
                    #h4("Description "),
                    #p("By clicking the following tabs, you can see the geographical distribution, a publication timeline and the health outcomes approached in each of the 16 studies included."),
                    radioGroupButtons(
                      inputId = "eda_btn", justified = T, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Location of all studies` = "sp", 
                                  `<i class='fa fa-calendar-alt'></i> Timeline of all studies` = "ts", 
                                  `<i class='fa fa-poll'></i> Health Outcome of all studies` = "coef",
                                  `<i class='fa fa-file-text'></i> All Studies  ` = "tabl"),
                    ),
                    
                    uiOutput("eda_text"),
                    uiOutput("eda_plot")
                    
                )
              )
      ),
      ## low respiratory ####
      ## * introduction ####
      tabItem(tabName = "low_rsp_intro",
              fluidRow(
                box(width = 12,
                    p("Out 532 outcome and exposure relationships extracted from the 16 studies, relationships that include outcomes categorized as lower respiratory tract were the most approached."),
                    p("There was no consistent evidence of an association between exposure (or higher levels of exposure) to animal
facilities and higher odds of lower respiratory tract outcomes for the prevalence OR effect measure, except
when the level of odor annoyance was used as the measure of exposure."),
                    radioGroupButtons(
                      inputId = "low_res_btn", justified = F, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Distribution of Lower Respiratory Outcomes` = "sp",
                                  `<i class='fa fa-calendar-alt'></i> Timeline of Lower Respiratory Outcomes` = "ts",
                                  `<i class='fa fa-poll'></i> Outcomes Categorized as Lower Respiratory` = "coef")
                    ),
                    uiOutput("low_res_intro_text"),
                    uiOutput("low_res_intro_plot")
                )
              )
      ),
      
      tabItem(tabName = "up_rsp_intro",
              fluidRow(
                box(width = 12,
                    p("Measures of upper respiratory tract health were commonly reported outcome variables. The measures of association reported were regression coefficients and
prevalence ORs."),
                    p("In some cases, the regression coefficients indicated increased disease at higher levels of exposure, which suggested that exposure was associated with increased
disease or symptoms. In other cases, the regression coefficients for exposure indicated the presence of protective
effects."),
                    radioGroupButtons(
                      inputId = "up_res_btn", justified = F, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Distribution of Upper Respiratory Outcomes` = "sp",
                                  `<i class='fa fa-calendar-alt'></i> Timeline of Upper Respiratory Outcomes` = "ts",
                                  `<i class='fa fa-poll'></i> Outcomes Categorized as Upper Respiratory` = "coef")
                    ),
                    uiOutput("up_res_intro_text"),
                    uiOutput("up_res_intro_plot")
                )
              )
      ),
      
      tabItem(tabName = "ar_rsp_intro",
              fluidRow(
                box(width = 12,
                    p("The association between MRSA colonization and proximity is unclear, mainly due to a lack of replication."),
                    p("The association between human carriage of Methicillin-resistant Staphylococcus aureus (MRSA) and proximity to animal feeding operations, was a new outcome for our last update."),
                    radioGroupButtons(
                      inputId = "ar_res_btn", justified = T, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Distribution of AMR Outcomes` = "sp",
                                  `<i class='fa fa-calendar-alt'></i> Timeline of AMR Outcomes` = "ts",
                                  `<i class='fa fa-poll'></i> Outcomes Categorized as AMR` = "coef")
                    ),
                    uiOutput("ar_res_intro_text"),
                    uiOutput("ar_res_intro_plot")
                )
              )
      ),
      
      tabItem(tabName = "gi_rsp_intro",
              fluidRow(
                box(width = 12,
                    p("Gastrointestinal outcomes were assessed in only two publication"),
                    
                    radioGroupButtons(
                      inputId = "gi_res_btn", justified = F, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Distribution of Gastrointestinal Outcomes` = "sp",
                                  `<i class='fa fa-calendar-alt'></i> Timeline of Gastrointestinal Outcomes` = "ts",
                                  `<i class='fa fa-poll'></i> Outcomes Categorized as Gastrointestinal diseases` = "coef")
                    ),
                    uiOutput("gi_res_intro_text"),
                    uiOutput("gi_res_intro_plot")
                )
              )
      ),
      
      tabItem(tabName = "Neur_rsp_intro",
              fluidRow(
                box(width = 12,
                    p("Neurological diseases were addressed in three publications where headache and dizziness were the most analyzed responses."),
                    
                    radioGroupButtons(
                      inputId = "Neur_res_btn", justified = F, label = "",
                      choices = c(`<i class='fa fa-globe'></i> Geographic Distribution of Neurologic Outcomes` = "sp",
                                  `<i class='fa fa-calendar-alt'></i> Timeline of Neurologic Outcomes` = "ts",
                                  `<i class='fa fa-poll'></i> Outcomes Categorized as Neurological diseases` = "coef")
                    ),
                    uiOutput("Neur_res_intro_text"),
                    uiOutput("Neur_res_intro_plot")
                )
              )
      ),
      
      ## * forest plot ####
      
      
      tabItem(tabName = "low_rsp_forest",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "Forest Plot",
                    p("Please select an effect size and a type of exposure from the drop-down menus below."),
                    br(),
                    fluidRow(
                      #column(width = 4,
                      #      wellPanel(
                      #       uiOutput("measure")
                      #     )
                      #),
                      # column(width = 4,
                      #       wellPanel(
                      #         uiOutput("expo_var_1")
                      #       ) 
                      
                      #),
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_1")
                             )),
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_2")
                             )),
                      infoBoxOutput("out1")
                      #br(),
                      
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot_low",height="auto"),
                             #plotOutput("gg_forestp"),
                             #plotOutput("myplot"),
                             #DT::dataTableOutput("low_rsp_dt")#,
                             #DT::dataTableOutput('mytable1234')
                      ),
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )
                      ),
                      column(width = 7,
                             #selectInput("Measure", "Mesure:", 
                             #               choices=(forest$mm)),
                             h4("Selected outcome-exposure"),
                             #uiOutput("expo_var_1"),
                             #div(dataTableOutput("my_table"),style
                             #     = "overflow-y: auto")#,
                             div(dataTableOutput("my_table"),style = "font-size: 65%; width: 40%;text-align: center",
                             )#,
                             
                             #DT::dataTableOutput("my_table")
                      )
                    )                    
                )
                
                
                
              )    
              
      ),   
      
      
      tabItem(tabName = "up_rsp_forest",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "Forest Plot",
                    p("Please select an effect size and a type of exposure from the drop-down menus below."),
                    br(),
                    fluidRow(
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_1_up")
                             )),
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_2_up")
                             )),
                      infoBoxOutput("out12")
                      
                      
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot_up",height="auto"),
                             
                      ),
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )
                      ),
                      column(width = 7,
                             
                             h4("Selected outcome-exposure"),
                             
                             div(dataTableOutput("my_table_up"),style = "font-size: 65%; width: 40%;text-align: center",
                             )
                             
                             
                      )
                    )                    
                )
                
                
                
              )    
              
      ),  
      
      
      tabItem(tabName = "ar_rsp_forest",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "Forest Plot",
                    p("Please select an effect size and a type of exposure from the drop-down menus below."),
                    br(),
                    fluidRow(
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_1_ar")
                             )),
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_2_ar")
                             )),
                      infoBoxOutput("out12_ar")
                      
                      
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot_ar",height="auto"),
                             
                      ),
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )
                      ),
                      column(width = 7,
                             
                             h4("Selected outcome-exposure"),
                             
                             div(dataTableOutput("my_table_ar"),style = "font-size: 65%; width: 40%;text-align: center",
                             )
                             
                             
                      )
                    )                    
                )
                
                
                
              )    
              
      ),  
      
      tabItem(tabName = "gi_rsp_forest",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "Forest Plot",
                    p("Please select an effect size and a type of exposure from the drop-down menus below."),
                    br(),
                    fluidRow(
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_1_gi")
                             )),
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_2_gi")
                             )),
                      infoBoxOutput("out12_gi")
                      
                      
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot_gi",height="auto"),
                             
                      ),
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )
                      ),
                      column(width = 7,
                             
                             h4("Selected outcome-exposure"),
                             
                             div(dataTableOutput("my_table_gi"),style = "font-size: 65%; width: 40%;text-align: center",
                             )
                             
                             
                      )
                    )                    
                )
                
                
                
              )    
              
      ),  
      
      
      tabItem(tabName = "Neur_rsp_forest",
              fluidRow(
                box(width = 12, solidHeader = T, status = "primary",
                    title = "Forest Plot",
                    p("Please select an effect size and a type of exposure from the drop-down menus below."),
                    br(),
                    fluidRow(
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_1_Neur")
                             )),
                      
                      column(width = 4,
                             wellPanel(
                               uiOutput("expo_var_2_Neur")
                             )),
                      infoBoxOutput("out12_Neur")
                      
                      
                    ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot_Neur",height="auto"),
                             
                      ),
                      tags$head(
                        tags$style(type = "text/css",
                                   HTML("th { text-align: center; }")
                        )
                      ),
                      column(width = 7,
                             
                             h4("Selected outcome-exposure"),
                             
                             div(dataTableOutput("my_table_Neur"),style = "font-size: 65%; width: 40%;text-align: center",
                             )
                             
                             
                      )
                    )                    
                )
                
                
                
              )    
              
      ),
 
    
     
      
      ## * risk of bias ####
      tabItem(tabName = "low_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Lower Respiratory Diseases",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias") %>% withSpinner(),
                    br(),
                    
                    tags$li("For further details about risk of bias assessment 
                    displayed in the table on the right side", a("click here", href = "https://www.bristol.ac.uk/population-health-sciences/centres/cresyda/barr/riskofbias/")), 
                    
                    tags$li("The overall
risk of bias was considered to be serious or critical for
the studies that provided outcomes measured as regression coefficients."),
                
                tags$li("The overall risk of bias was serious for all of the studies
that reported prevalence ORs as measures of association")
              ))), 
      ## * risk of bias upper R
      tabItem(tabName = "up_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Upper Respiratory Diseases",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias_up") %>% withSpinner()
                )
              )), 
      
      ## * risk of bias AR
      tabItem(tabName = "ar_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Antimicrobial resistance outcomes",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias_ar") %>% withSpinner(),
                    br(),
                    tags$li("The overall risk of bias was serious for all antimicrobial resistance outcomes reported by Schinasi et al. and moderate for
the outcome reported by Feingold et al.")
                ))), 
      ## * risk of bias upper GI
      tabItem(tabName = "gi_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Gastrointestinal Diseases",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias_gi") %>% withSpinner(),
                    br(),
                    tags$li("There was an overall serious risk of bias associated with these objectively measured exposures. The associations with the subjectively measured exposures (hog odor) had an overall critical risk of bias due to the concerns previously discussed (relating to the CHEIHO study).")
                ))), 
      ## * risk of bias Neurologic
      tabItem(tabName = "Neur_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Neurologic conditions",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias_Neur") %>% withSpinner(),
                    br(),
                    tags$li("The overall risk of bias for this exposure-outcome association was critical.")
                ))), 
      
      
      
      ## * conclusion ####
      tabItem(tabName = "low_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation Lower Respiratory Outcomes",
                    tags$li("Many studies reported outcomes associated with the lower respiratory tract. The reported effect measures were either regression coefficients (βs) or prevalence ORs  and prevalence ratios. "),
                    tags$li("Most of the regression coefficients reported for lower respiratory tract outcomes included a 95% confidence interval (CI), which included effect sizes associated with protective effects, risk effects, and no effect (i.e., the 95% CI included the null value). Three regression coefficient values had negative value beta estimates. The overall risk of bias was considered serious or critical for the studies that provided outcomes measured as regression coefficients."),
                    tags$li("There was no consistent evidence of an association between exposure (or higher levels of exposure) to animal facilities and higher odds of lower respiratory tract outcomes for the prevalence OR effect measure, except when the level of odor annoyance was used as the measure of exposure. The precision of the effect size estimates was low (i.e., the intervals were wide), and the 95% CIs extended across a range that included a protective effect, no effect, and a risk effect. . Mirabelli et al. reported 89 prevalence ratios (PR) and these are reported in Additional file 1, and the same inference applies. Most prevalence ratio intervals included one, and no consistent dose-response effect was observed."),
                    tags$li("Many authors studied ordered levels of exposure (increasing or decreasing) to document a dose-response, which is important for investigation of causation. When the metric for goat exposure was a density indicator (i.e., number of goats within 5 km of the subject’s residence) and the outcome metric was pneumonia, there was evidence of an association between higher goat density and lower respiratory disease. The prevalence OR for the highest goat density (17,191–20,960) was 1.68, which indicated an increased prevalence of disease. Although the precision was moderate, all of the values within the 95% CI were associated with increased prevalence. These apparently inconsistent findings were reported by the same authors in the same study population. One explanation is that different mechanisms lead to the development of pneumonia versus asthma."),
                    tags$li("The study was performed during a Q fever outbreak, and the finding suggested that exposure to goats was strongly associated with Q fever risk. The authors used pneumonia as a potential Q fever-related outcome, because pneumonia was the most frequent diagnosis among the notified Q fever patients in the Netherlands epidemic. The authors also noted that exposure to poultry was associated with increased prevalence odds of pneumonia. This association between goats and pneumonia was likely due to Q fever, rather than particulate or gaseous emissions. The overall risk of bias was serious for all of the studies that reported prevalence ORs as measures of association."),    
                    tags$li("The overall risk of bias was serious for all of the studies
that reported prevalence ORs as measures of association.")
                    )
              )),
      
      tabItem(tabName = "up_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation Upper Respiratory Outcomes",
                    tags$li("In Schinasi et al. there were inconsistent associations between the objective measures of exposure and the subjective measures of upper respiratory tract outcomes. For objectively measured exposure metrics and subjectively measured outcomes, most effect sizes had levels of precision that were associated with protective effects, no effect, or risk effects. There were consistent associations between subjectively measured exposures (average 12-h odor levels and twice-daily odor levels) and outcomes (i.e., increased odor was associated with increased disease). Large positive effect sizes indicated higher values for measures of upper respiratory tract outcomes in participants who indicated that they were exposed to higher odor levels. For this subgroup, all of the values in the 95% CI were associated with increased prevalence, except for the sore throat outcome."),
                    tags$li("Four studies used prevalence ORs to measure associations between exposure to animal facilities and upper respiratory tract outcomes. The associations were not consistent for the subgroups objective exposure/objective outcome, objective exposure/ subjective outcome, and subjective exposure/objective outcome. The findings across dose gradient were also inconsistent. Some results indicated that compared with high-level exposures, lower- or medium-level exposures were associated with higher odds of disease."),
                    tags$li("When the measure of exposure was the level of odor annoyance, a dose-response was present if the outcome was measured subjectively (self-reported symptoms). The association was inconsistent when an objective measure of disease occurrence was assessed (specific IgE to common allergens). Moderate annoyance at odor was associated with the highest specific IgE response relative to “not at all annoyed” by odor. The precision of the result suggested the direction was consistently positive (OR = 1.71, 95% CI 1.02, 2.87). However, compared with “not at all annoyed,” individuals who responded “somewhat annoyed” (OR = 1.11, 95% CI 0.79, 1.57) and “strongly annoyed” (OR = 1.02, 95% CI 0.51, 2.03) had effect sizes more consistent with an interpretation of no effect (close to OR = 1).")
                    )
              )),
      
      tabItem(tabName = "ar_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation Antimicrobial resistance Outcomes",
                    tags$li("There was no consistent finding for this outcome. One study used a subjective measure of exposure (odor), and the patients were aware of their Methicillin-resistant Staphylococcus aureus (MRSA) status at the time of exposure assessment. Schinasi et al.  found that the odds of being MRSA-positive were greater if the subject had ever smelled farm odors when at home (OR = 1.51). The precision of this estimate was moderate, but most of the effect sizes in the 95% CI were consistent with no effect (95% CI 0.8, 2.86). However, this association was not consistently observed for the same study participants when the exposure was an objectively measured metric, and the observed results did not document a consistent dose-response."),
                    tags$li("Schinasi et al. evaluated another exposure metric in the same population (the number of farrowing swine permitted within a 1- square-mile block of the participant’s residence) and explored whether a dose-response was present. However, when compared with the lowest exposure level, the middle exposure level had a higher prevalence of MRSA carriage than higher levels of exposure. The authors reported moderate exposure using three metrics (farrowing swine, non-farrowing swine, and swine); the effect sizes and precision estimates were 1.99 (95% CI 0.99, 4.06), 2.04 (0.61, 6.85), and 4.76 (1.36, 16.69), respectively. These estimates contrasted with higher levels of exposure (>149), which were associated with a prevalence OR that suggested lower odds of nasal carriage of Methicillin-resistant Staphylococcus aureus (MRSA). "),
                    tags$li("Feingold et al. did report an association between MRSA carriage and swine (1.37, 95% CI 1.01, 1.67), cattle (2.28, 95% CI 1.17, 4.15), and veal (1.37, 95% CI 1.08, 1.72) density. They did not report any inconsistencies in association because they only reported one exposure metric (the log of municipal density). It is unclear whether other exposure metrics were evaluated but not reported. "),
                    tags$li("The overall risk of bias was serious for all antimicrobial resistance outcomes reported by Schinasi et al.  and moderate for the outcome reported by Feingold et al.")
                    )
              )),
      
      tabItem(tabName = "gi_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation Gastrointestinal Outcomes",
                    tags$li("The associations with the exposures were reported as regression coefficients. The outcomes were self-assessed every 12 hours by each participant and represented the presence or absence of nausea, diarrhea, and poor appetite over the past 12 hours. Overall, there was no consistent direction of association between outcome and exposure. Only two effect sizes had intervals that did not include zero: poor appetite was positively associated with 12 h community-level average PM10 per 10 g/m3 (β = 0.51, 95% CI 0.12, 0.90), and nausea was negatively associated with 12 h community-level PM2.5-10 per 10 µg/m3 (β = -1.43, 95% CI -2.82, -0.04). "),
                    tags$li("There was an overall serious risk of bias associated with these objectively measured exposures.")
                )
              )),
      
      tabItem(tabName = "Neur_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation Neurologic Outcomes",
                    tags$li("Horton et al. and Schinasi et al. both publications described the results for the participants of the CHEIHO Study, who were asked to self-report numerous outcomes. Horton et al.reported the associations as odds ratios. Schinasi et al. reported the regression coefficients (betas) from a logistic regression model. The direction of association was not consistent; sometimes the effect size indicated a protective effect or a risk factor. For the odor metrics, the effect measure was always in the direction that indicated increased risk of health outcomes in exposed individuals. The intervals of three (hydrogen sulfide, PM10, semivolatile PM10) of the four effect sizes reported by Horton et al. were associated with estimates that indicated risk, protection, or no association."),
                    tags$li("The overall risk of bias for this exposure-outcome association was serious or critical. Horton et al. reported one effect measure for which the precision measure (95% confidence interval (CI)) only included estimates associated with harm: for the exposure - twice-daily odor rating - and the outcome - confused or unable to concentrate - (95% CI = 1.16, 1.50). The overall risk of bias for this exposure-outcome association was critical.")
                    )
              )),
      
      ## references ####  
      tabItem(tabName = "ref",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "References",
                    div(HTML(mybib)))
              ))
    )
  )
)