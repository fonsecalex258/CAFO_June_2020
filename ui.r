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
                        menuItem("Conclusion", tabName = "up_rsp_conclusion")),
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
                                   p("Systematic reviews that are continually updated, incorporating relevant new evidence as it becomes available.
                  This term means that rather than being a static publication in a peer reviewed journal, 
                  the review is housed on this website allowing for more timely updates and more accessible information."),
                                   p("This process can be visualized in the below figure . Our last static systematic review about this topic was published in 2017 based on  16 relevant studies published before that year. 
                    However, during the las 3 years new studies may have been published and would need to be incorporated in  new systematic reviews. These new reviews are published on this website and updated periodically."),
                                   
                                   p("Through this website producers, public health officers, community leaders and community members can access
                  the latest summary of the available studies and a balance interpretation of the findings and their implications
                  in the wider body of literature will better serve the needs of the community because it"),
                                   p("1. Democratizes access to the information and interpretation, and"),
                                   p("2. Provides for more timely and relevant update"),
                                   
                  ),
                  br(),
                  column(width = 5,div(img(src = "lsr.PNG", height = 320, width = 400), style="text-align: center;")
                  )),
                  hr(),
                  
                  fluidRow(column(width = 7, 
                                  br(),                
                  p("In recent years there have been a growing concern about the harmful effects that animal facilities could have on nearby communities. 
                  Regarding the swine industry, it has been suggested that facilities that confine animals indoors for feeding might represent a health
                  hazard for surrounding communities due to the exposition to odors, emissions and other harmful agents."),
                  
                  
                  p("The results showed in this website correspond to those published in our last systematic review published in 2017 approaching the association between human health issues and animal production. Given that additional studies have been , and continue to be, conducted to identify the relationship between proximity to livestock facilities and health outcomes in the surrounding communities our plan is create a living literature review of the research reports that study this association. Every time the living review is updated, the results will be displayed on this website so that the public will be able to access the latest summary of the available studies and a balance interpretation of the findings."),
                  p("Currently, we are developing the protocol that will guide the second update of the systematic review and the implementation of the living systematic review."),
                  #hr()
                  ),
                  
                  br(),
                  column(width = 5,div(img(src = "cafo.jpg", height = 330, width = 400),align = "center"),
                         )),
                  #p("In this sense we had performed two systematic review summarizing the findings of publications approaching this matter.
                  #These previous studies can be consulted by clicking on the following links:"),
                  #p("1.", a("First Systematic Review", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),
                  #p("2.", a("Second Systematic Review", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),
                  hr(),
                  
                  
                  
                  column(width = 12,h4("Our Previous Systematic Reviews"),
                         p("We had performed two systematic review summarizing the findings of publications approaching this matter.
                  These previous studies can be consulted by clicking on the following links:"),
                         p("1.", a("First Systematic Review", href = "https://journals.plos.org/plosone/article/authors?id=10.1371/journal.pone.0009530")),
                         p("2.", a("Second Systematic Review (First Update)", href = "https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-017-0465-z")),       
                  p("Our last review (First Update) was published in 2017 and its objective was to update a systematic review of associations
                    between living near an animal feeding operation (AFO) and human health.Our research question was:"),
                  tags$blockquote("What are the associations between animal feeding operations and measures of the health of individuals 
                     living near animal feeding operations, but not actively engaged in livestock production?"),
                  #tags$iframe(width="560", height="315", src="https://www.youtube.com/watch?v=WgJWrHFgh8s", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                  
                ))
              )),
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
when the level of odor annoyance was used as the measure of exposure”."),
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
                    p("In
some cases, the regression coefficients indicated increased disease at higher levels of exposure, which suggested that exposure was associated with increased
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
                    p("Out 532 outcome and exposure relationships extracted from the 16 studies, relationships that include outcomes categorized as lower respiratory tract were the most approached."),
                    p("There was no consistent evidence of an association between exposure (or higher levels of exposure) to animal
facilities and higher odds of lower respiratory tract outcomes for the prevalence OR effect measure, except
when the level of odor annoyance was used as the measure of exposure”."),
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
      
      ## * forest plot ####

      
      tabItem(tabName = "low_rsp_forest",
              fluidRow(
                box(width = 12, title = "Concentrated Animal Feeding Operations (CAFOs) Data", solidHeader = T, status = "primary",
                    p("The reported effect measures were either regression coefficients (βs) or prevalence ORs and prevalence ratios. In the forest plot (left side), 
                      each point represents the reported effect measure (e.g. an odds ratio) for a specific exposure-outcome relationship."),
                    p("The gray columns in the forest plot’s left side group the exposures in two categories. The second column from left to right indicates if the exposure evaluated involved direct contact with the individuals or it is surrogated. The first column from left to right groups the exposures according to type of exposure 
                      (i.e distance, gases, odor and aerosols)."),
                    p(" The table on the right side shows the risk of bias assessment for the specific point selected previously on the forest plot. For further details about risk of bias assessment displayed in the table on the right side", a("click here", href = "https://www.bristol.ac.uk/population-health-sciences/centres/cresyda/barr/riskofbias/"), 
                      "Once you click on one a row, a table is going to pop-up to provide more details about the judgment made by the authors for that particular exposure-outcome relationship.  "),
                    p("The video below  provides more details about the usage of the forest plot and its annexes."),
                    br(),
                    
                    hr(),
                    
                    fluidRow(
                      column(width = 8,h4("How to use the forest plot??"),
                             HTML('<iframe width="450" height="230" src="https://www.youtube.com/embed/T1-k7VYwsHg" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                      )  
                      
                    ),
                    
                    br(),
                    
                    hr(),
                    
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
                             ))
                             #br(),
                            
                      ),
                    
                    hr(),
                    fluidRow(
                      column(width = 5,
                             h4("Select a point: "),
                             #actionButton("reset", label = "Reset selection"),
                             ggiraph::girafeOutput("plot"),
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
                             div(dataTableOutput("my_table"),style = "font-size: 70%; width: 60%;text-align: center",
                             )#,
                             
                             #DT::dataTableOutput("my_table")
                      )
                    )
                )
              )
              
              
              
      ),    
      
      ## * risk of bias ####
      tabItem(tabName = "low_rsp_risk_of_bias",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Risk of Bias for Lower Respiratory Disease",
                    p("Risk of Bias plot"),
                    plotlyOutput("bias") %>% withSpinner()
                )
              )), 
   
      
      
      
      ## * conclusion ####
      tabItem(tabName = "low_rsp_conclusion",
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary", title = "Interpretation about Lower Respiratory Disease",
                    p("Many studies reported outcomes associated with the lower respiratory tract. The reported effect measures were either regression coefficients (βs) or prevalence ORs  and prevalence ratios. "),
                    p("Most of the regression coefficients reported for lower respiratory tract outcomes included a 95% confidence interval (CI), which included effect sizes associated with protective effects, risk effects, and no effect (i.e., the 95% CI included the null value). Three regression coefficient values had negative value beta estimates. The overall risk of bias was considered serious or critical for the studies that provided outcomes measured as regression coefficients."),
                    p("There was no consistent evidence of an association between exposure (or higher levels of exposure) to animal facilities and higher odds of lower respiratory tract outcomes for the prevalence OR effect measure, except when the level of odor annoyance was used as the measure of exposure. The precision of the effect size estimates was low (i.e., the intervals were wide), and the 95% CIs extended across a range that included a protective effect, no effect, and a risk effect. . Mirabelli et al. reported 89 prevalence ratios (PR) and these are reported in Additional file 1, and the same inference applies. Most prevalence ratio intervals included one, and no consistent dose-response effect was observed."),
                p("Many authors studied ordered levels of exposure (increasing or decreasing) to document a dose-response, which is important for investigation of causation. When the metric for goat exposure was a density indicator (i.e., number of goats within 5 km of the subject’s residence) and the outcome metric was pneumonia, there was evidence of an association between higher goat density and lower respiratory disease. The prevalence OR for the highest goat density (17,191–20,960) was 1.68, which indicated an increased prevalence of disease. Although the precision was moderate, all of the values within the 95% CI were associated with increased prevalence. These apparently inconsistent findings were reported by the same authors in the same study population. One explanation is that different mechanisms lead to the development of pneumonia versus asthma."),
                p("The study was performed during a Q fever outbreak, and the finding suggested that exposure to goats was strongly associated with Q fever risk. The authors used pneumonia as a potential Q fever-related outcome, because pneumonia was the most frequent diagnosis among the notified Q fever patients in the Netherlands epidemic. The authors also noted that exposure to poultry was associated with increased prevalence odds of pneumonia. This association between goats and pneumonia was likely due to Q fever, rather than particulate or gaseous emissions. The overall risk of bias was serious for all of the studies that reported prevalence ORs as measures of association."),    
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
