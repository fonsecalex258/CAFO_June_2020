library(ggplot2)
library(ggiraph)
library(DT)
library(dplyr)
library(data.table)
library(formattable)
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(dplyr)

dat <- data.frame(
  country = c('us', 'nl', 'de')
  #flag = c(
  #   '<img src = "colombia.jpg" height="52" ></img>',
  #       '<img src = "united.jpg" height="52" ></img>'
)

#dat$flag1 <- sprintf ('<img src = "http://flagpedia.net/data/flags/mini/%s.png" height="52" ></img>', dat$country)
dat$flag1 <- sprintf ('<img src = "https://flagpedia.net/data/flags/w2560/%s.png" height="52" ></img>', dat$country)

#My APA-format theme
apatheme=theme_bw(base_size = 23)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        strip.text.y.left = element_text(size = 27,angle = 90),
        axis.text.y = element_text(hjust = 23),
        axis.title.y = element_text(face = "bold", size=25),
        text=element_text(family='Times', size=29),
        legend.position='none')

bg.picker <- function(z){
  if(is.na(z)){return("black")}
  else if(z == "Low"){return("forestgreen")}
  else if(z == "Moderate"){return("lightgreen")}
  else if(z == "High"){return("pink")}
  else if(z == "Serious"){return("salmon")}
  else if(z == "Critical"){return("red")}
  else if(z == "Uncertain"){return("wheat")}
  #else if( z > 20 & z <= 80){return("yellow")}
  #else {return("pink")}
}

#forest <- forest %>% mutate(inter = ifelse(is.na(lowerci), yi, paste(forest$yi,"[",forest$lowerci, ",", forest$upperci,"]")), Reference = paste(forest$id, ".", forest$study))
#forest123 <- forest123 %>% mutate( Reference = paste(forest123$id, ".", forest123$study))
#forest123 <- forest123 %>% mutate( Reference = paste(forest123$study, "(", forest123$id,")"))
shinyServer(function(input, output){ 
  
  #source("reactive.R", local = TRUE)
  #source("forestROBPlot.R",  local = TRUE)
  #source("forestROBPlotExperiment.R", local = TRUE)
  #  observeEvent(input$chk, {
  #    if (input$chk) hide('mytable1234') else show('mytable1234')
  #  })
  
  
  ## descriptive plots ####
  ## * switch text ####
  output$eda_text <- renderUI({
    switch(
      input$eda_btn,
      "sp" = p("Most articles were published in North America and some in Europe."),
      "ts" = p("This plot shows the date of publication of studies included in the review"),
      "coef" = p("The following table shows the number of reported outcomes grouped in broad categories (i.e lower and upper respiratory tracts, MRSA etc) 
                    for the relevant studies included in the last review. We observe that nearly 500 outcomes were extracted from the 16 publications and 10 study populations. For several study populations, numerous correlated outcomes were compared with
numerous correlated exposures. This approach increases
the potential to discover important associations and also
increases the potential for identification of false associations due to random error (increased type 1 error)."),
      "tabl" = p("The following list contains the titles of the articles included in this review.")
    )
  })
  ## * switch plot ####
  output$eda_plot <- renderUI({
    switch(
      input$eda_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map") %>% withSpinner()),
        #column(width = 6, plotlyOutput("geobar"),dataTableOutput('mytable1234') %>% withSpinner())#,
        column(width = 6, plotlyOutput("geobar") %>% withSpinner())),
      "ts" = timevisOutput("timeline"),
      "coef" = plotOutput("measure_all", hover = "plot_hover") %>% withSpinner(),
      "tabl" = DT::dataTableOutput("mytable1234")
    )
  })
  
  ##
  
  
  
  ## * geographic distribution ####
  output$map <- renderLeaflet({
    
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/23111006/'>Wing et al. 2013</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/7620910/'>Schiffman et al. 1995</a></b>","<br>",
                                                                      "<b><a href='https://link.springer.com/article/10.1007/s10745-005-1653-3'>Bullers 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/19890165/'>Horton et al. 2009</a></b>","<br>",
                                                                      "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4517575/'>Mirabelli et al. 2006</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16075904/'>Avery et al. 2004</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/24958086/'>Schinasi et al. 2014</a></b>","<br>"
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16379061/'>Radon et al. 2005</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       "<b><a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0038843'>Smit et al. 2012</a></b>","<br>",
                       "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3810935/'>Feingold et al. 2012</a></b>","<br>",
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
    
    
    
    
  })
  
  #####
  output$geobar <- renderPlotly({
    gg <- cafo2 %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>% 
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) + 
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  ## * timeline ####
  output$timeline <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% select(paperInfo, paperYear) %>% distinct() %>% 
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period 
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ## Insert into a dataframe 
    datt1 <- data.frame(
      ## make it reactive
      id = 1:nrow(testtimeline),   
      content = testtimeline$weblink,
      start = testtimeline$year,
      end = NA
    )
    timevis(datt1, showZoom = F)
  })
  
  
  ## * outcome ####
  output$measure_all <- renderPlot({
    colnames(dataset)[which(names(dataset) == "Categorized.class")] <- "Broad Outcome Category"
    dataset %>% ggplot(aes(x = paperInfo ) ) +
      geom_bar(aes(fill = `Broad Outcome Category`)) + coord_flip() + 
      scale_fill_brewer(palette = "Set3") +
      #labs(x = "", fill = "Health Outcome Group") +
      labs(x = "", fill = "Broad Outcome Category") +
      xlab("Paper") +
      ylab("Number of Reported Outcomes") + 
      theme(plot.background = element_rect(fill = "#BFD5E3"),
            panel.background = element_rect(fill = "white"),
            axis.line.x = element_line(color = "grey"))
    #ggplotly(gg, tooltip = c('Broad Outcome Category', 'count')) %>% config(displayModeBar = T)
    
    
  })
  
  ## forest fitlers ####
  selected_class <- reactive({
    case_when(
      grepl("low_rsp", input$sidebar) ~ "Lower Respiratory",
      grepl("up_rsp", input$sidebar) ~ "Upper Respiratory",
      grepl("ar_rsp", input$sidebar) ~ "Antimicrobial resistance",
      grepl("gi_rsp", input$sidebar) ~ "Gastrointestinal diseases",
      grepl("Neur_rsp", input$sidebar) ~ "Neurologic",
      TRUE ~ "Other"
    )
  }) 
  selected_id <- reactive({
    dataset %>% filter(Categorized.class==selected_class()) %>% 
      pull(Refid) %>% unique()
  })
  
  
  ##### only measure of association
  output$expo_var_1 <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### up
  
  output$expo_var_1_up <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_up",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### AR
  
  output$expo_var_1_ar <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_ar",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### GI
  
  output$expo_var_1_gi <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_gi",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  ### Neuro
  
  output$expo_var_1_Neur <- renderUI({
    choices1 <- forest$effect_z
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b_Neur",
                "Effect size",
                choices = unique(choices1), 
                selected = choices1[1])
    
  })
  
  output$expo_var_2 <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Upper R
  
  output$expo_var_2_up <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_up",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### AR
  
  output$expo_var_2_ar <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_ar",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Gastro
  
  output$expo_var_2_gi <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_gi",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  #### Neuro
  
  output$expo_var_2_Neur <- renderUI({
    choices2 <- forest$t_expo
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_c_Neur",
                "Type of Exposure",
                choices = unique(choices2),
                
                selected = choices2[1])
  })
  
  
  
  
  ## lower respiratory ####
  ## * intro #####
  output$low_res_intro_text <- renderUI({
    switch(
      input$low_res_btn,
      "sp" = p("Articles related to lower respiratory disease were published in Germany, United States and Netherlands."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to lower respiratory tract were studied. This category was the most analyzed with 9 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as lower respiratory tract. Asthma and wheeze related conditions were the most common outcomes grouped in this category.")
    )
  })
  
  #####copying for upper
  
  output$up_res_intro_text <- renderUI({
    switch(
      input$up_res_btn,
      "sp" = p("Most articles related to upper respiratory disease were published in United States, Netherlands and Germany."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to upper respiratory tract were studied. This category was analyzed by 6 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as upper respiratory tract. Allergic rhinitis and nasal irritation were the most common variables grouped in this category.")
    )
  })
  
  #####copying for AMR
  output$ar_res_intro_text <- renderUI({
    switch(
      input$ar_res_btn,
      "sp" = p("Most articles related to Antimicrobial resistance were published in United States and Netherlands"),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to Antimicrobial resistance were studied. This category was analyzed by 2 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as Antimicrobial resistance outcomes.")
    )
  })
  
  #####copying for GI
  
  output$gi_res_intro_text <- renderUI({
    switch(
      input$gi_res_btn,
      "sp" = p("Articles related to Gastrointestinal diseases were published only in United States."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to gastrointestinal tract were studied. This category was analyzed by 2 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as gastrointestinal tract. Diarhea, nausea and poor appetite were the most common outcomes grouped in this category.")
    )
  })
  
  output$Neur_res_intro_text <- renderUI({
    switch(
      input$Neur_res_btn,
      "sp" = p("Articles related to Neurologic diseases were published only in United States."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to Neurologic diseases were studied. This category was analyzed by 3 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as Neurologic diseases. Headache and dizziness related conditions were the most common outcomes grouped in this category.")
    )
  })
  
  output$low_res_intro_plot <- renderUI({
    switch(
      input$low_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_low_res") %>% withSpinner()),
        column(width = 6, plotlyOutput("geobar_low_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_low_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_low_res") %>% withSpinner()
    )
  })
  
  #######copying for uppper
  
  output$up_res_intro_plot <- renderUI({
    switch(
      input$up_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_up_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_up_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_up_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_up_res") %>% withSpinner()
    )
  })
  
  #######copying for AMR
  
  output$ar_res_intro_plot <- renderUI({
    switch(
      input$ar_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_ar_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_ar_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_ar_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_ar_res") %>% withSpinner()
    )
  })
  
  #######copying for GI
  
  output$gi_res_intro_plot <- renderUI({
    switch(
      input$gi_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_gi_res") %>% withSpinner()) ,
        column(width = 6, plotlyOutput("geobar_gi_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_gi_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_gi_res") %>% withSpinner()
    )
  })
  
  output$Neur_res_intro_plot <- renderUI({
    switch(
      input$Neur_res_btn,
      "sp" = fluidRow(
        column(width = 6, leafletOutput("map_Neur_res") %>% withSpinner()),
        column(width = 6, plotlyOutput("geobar_Neur_res") %>% withSpinner())),
      "ts" = timevisOutput("timeline_Neur_res"),
      #"coef" = plotlyOutput("measure_all_low_res") %>% withSpinner()
      "coef" = dataTableOutput("measure_all_Neur_res") %>% withSpinner()
    )
  })
  
  ## ** geographic distribution ####
  output$map_low_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      
                                                                      "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4517575/'>Mirabelli et al. 2006</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>"
                                                                      
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16379061/'>Radon et al. 2005</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       "<b><a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0038843'>Smit et al. 2012</a></b>","<br>",
                       
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
  })
  
  #####copying for upper
  
  output$map_up_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>",
                                                                      
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>"
                                                                      
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
                                                           "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "<b><a href='https://oem.bmj.com/content/71/2/134.long'>Smit et al. 2014</a></b>","<br>"
                 ) )))
  })
  
  #####copying for AMR
  
  output$map_ar_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/24958086/'>Schinasi et al. 2014</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                                                                            ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "<b><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3810935/'>Feingold et al. 2012</a></b>","<br>"
                 ) )))
  })
  
  ####copying for GI
  
  output$map_gi_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "No Records"
                 ) )))
  })
  
  ####copying for Neurologic
  
  output$map_Neur_res <- renderLeaflet({
    leaflet(cafoo) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%  
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map 
      addMarkers(lng = cafoo$long, lat = cafoo$lat,
                 #radius = log(cafoo$`Number of Studies`)*8,
                 popup = ifelse(cafoo$Country=="United States", paste("Country:",cafoo$Country,"<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/19890165/'>Horton et al. 2009</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/15866765/'>Schiffman et al. 2005</a></b>","<br>",
                                                                      "<b><a href='https://pubmed.ncbi.nlm.nih.gov/21228696/'>Schinasi et al. 2011</a></b>","<br>"
                                                                      
                 ), ifelse(cafoo$Country=="Germany", paste("Country:",cafoo$Country,"<br>",
                                                           "No Records"
                 ),
                 paste("Country:",cafoo$Country,"<br>",
                       
                       
                       "No Records"
                 ) )))
  })
  
  output$geobar_low_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for up
  
  output$geobar_up_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for AMR
  
  output$geobar_ar_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for GI
  
  output$geobar_gi_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  
  ########copying for Neurologic
  
  output$geobar_Neur_res <- renderPlotly({
    gg <- cafo2 %>% filter(Refid %in% selected_id()) %>% distinct() %>%
      group_by(Country) %>% summarise(Count = n()) %>%
      mutate(Country = forcats::fct_reorder(factor(Country), Count)) %>%
      ggplot(aes(x = Country, y = Count)) +
      geom_bar(aes(fill = Country), stat = "identity") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(gg, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)
  })
  ## ** timeline ####
  output$timeline_low_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for up
  
  output$timeline_up_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for AMR
  
  output$timeline_ar_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  ##### copying for GI
  
  output$timeline_gi_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  #######copying for Neurologic
  
  output$timeline_Neur_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(Refid, paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ###
    timedata2 <- testtimeline %>% filter (ids %in% timedata$Refid)%>% 
      select(weblink, year, ids) %>% distinct() 
    ## Insert into a dataframe 
    datt2 <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),   
      content = timedata2$weblink,
      start = timedata2$year,
      end = NA
    )
    timevis(datt2, showZoom = F)
  })
  
  
  ##### table outcomes Lower R
  
  output$measure_all_low_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for up
  
  output$measure_all_up_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for AMR
  
  output$measure_all_ar_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for GI
  
  output$measure_all_gi_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for Neurologic
  
  output$measure_all_Neur_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    
    names(sat)[1] <- "Outcome category"
    names(sat)[2] <- "Outcome variable analyzed"
    names(sat)[3] <- "Number of times the variable was analyzed"
    
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  ## ** outcome ####
  # output$measure_all_low_res <- renderPlotly({
  # browser()
  #   gg_low_res <- dataset %>%
  #    filter(`Categorized.class` == selected_class()) %>% 
  #   ggplot(aes(x = paperInfo)) +
  #     geom_bar(aes(fill = Outcome.variable)) + coord_flip() +
  #     scale_fill_brewer(palette = "Set3") +
  #     labs(x = "", fill = "Health Outcome Group") +
  #    xlab("Study") +
  #     ylab("Number of Reported Outcomes") + 
  #     theme(plot.background = element_rect(fill = "#BFD5E3"),
  #          panel.background = element_rect(fill = "white"),
  #          axis.line.x = element_line(color = "grey"))
  #  ggplotly(gg_low_res) %>% layout(showlegend = FALSE)
  # })
  
  
  #####################
  ## * forest plot ####
  #####################
  selected_state <- reactive({
    input$plot_low_selected
  })
  
  ##### For Upper
  
  selected_state_up <- reactive({
    input$plot_up_selected
  })
  
  ##### For AR
  
  selected_state_ar <- reactive({
    input$plot_ar_selected
  })
  
  ##### For Gastro
  
  selected_state_gi <- reactive({
    input$plot_gi_selected
  })
  
  ##### For Neuro
  
  selected_state_Neur <- reactive({
    input$plot_Neur_selected
  })
  
  #####
  
  forest_data <- reactive({
   
    forest_data <- forest %>% filter(
      effect_z == input$expo_b &  t_expo == input$expo_c)
  })
  
  
  #output$low_rsp_dt <- DT::renderDataTable({
  #  forest_data()
  #newdatas <- forest_data()[c(1,2)]
  #})
  
  output$out1 <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  }) 
 #### for Upper R 
  output$out12 <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for AR 
  output$out12_ar <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for GI 
  output$out12_gi <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  #### for Neuro 
  output$out12_Neur <- renderInfoBox({
    infoBox("How to use a forest plot",  
            a("Click here to go to the tutorial", onclick = "openTab('tutorial')", href="#", style = "font-size: 90%;"),
            icon = icon("chalkboard-teacher"), color = "blue"
    )
  })
  
  
  output$plot_low <- renderGirafe({
    low_forest <- forest_data() %>% filter(Categorized.class==selected_class())
    
    
       validate(
      need(low_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    x <- girafe(code = print(
      
      
      ggplot(low_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(low_forest$yi),max(low_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b == "Odds Ratio (OR)"|input$expo_b == "Prevalence Ratio (PR)"|input$expo_b == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b == "beta coefficient of the variable"){28} 
    else if (input$expo_b == "Odds Ratio (OR)") {25}
    else if (input$expo_b == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })
  
  
  ##### For Upper R
  forest_data_up <- reactive({
    
    forest_data_up <- forest %>% filter(
      effect_z == input$expo_b_up &  t_expo == input$expo_c_up)
  })
  
  output$plot_up <- renderGirafe({
    up_forest <- forest_data_up() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(up_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(up_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(up_forest$yi),max(up_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_up == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_up == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_up == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_up == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_up == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_up == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_up == "Odds Ratio (OR)"|input$expo_b_up == "Prevalence Ratio (PR)"|input$expo_b_up == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_up == "beta coefficient of the variable"){28} 
    else if (input$expo_b_up == "Odds Ratio (OR)") {25}
    else if (input$expo_b_up == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })   
  
  #### For AR
  
  forest_data_ar <- reactive({
    
    forest_data_ar <- forest %>% filter(
      effect_z == input$expo_b_ar &  t_expo == input$expo_c_ar)
  })
  
  output$plot_ar <- renderGirafe({
    ar_forest <- forest_data_ar() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(ar_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(ar_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(ar_forest$yi),max(ar_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_ar == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_ar == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_ar == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_ar == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_ar == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_ar == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_ar == "Odds Ratio (OR)"|input$expo_b_ar == "Prevalence Ratio (PR)"|input$expo_b_ar == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_ar == "beta coefficient of the variable"){28} 
    else if (input$expo_b_ar == "Odds Ratio (OR)") {15}
    else if (input$expo_b_ar == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  #### For Gastro
  
  forest_data_gi <- reactive({
    
    forest_data_gi <- forest %>% filter(
      effect_z == input$expo_b_gi &  t_expo == input$expo_c_gi)
  })
  
  output$plot_gi <- renderGirafe({
    gi_forest <- forest_data_gi() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(gi_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(gi_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(gi_forest$yi),max(gi_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_gi == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_gi == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_gi == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_gi == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_gi == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_gi == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_gi == "Odds Ratio (OR)"|input$expo_b_gi == "Prevalence Ratio (PR)"|input$expo_b_gi == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_gi == "beta coefficient of the variable"){28} 
    else if (input$expo_b_gi == "Odds Ratio (OR)") {15}
    else if (input$expo_b_gi == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  #### For Neuro
  
  forest_data_Neur <- reactive({
    
    forest_data_Neur <- forest %>% filter(
      effect_z == input$expo_b_Neur &  t_expo == input$expo_c_Neur)
  })
  
  output$plot_Neur <- renderGirafe({
    Neur_forest <- forest_data_Neur() %>% filter(Categorized.class==selected_class())
    
    
    validate(
      need(Neur_forest$id != "", "There are no records for the selected combination of effect size (ES) and Type of exposure, please make another selection.")
    )
    
    j <- girafe(code = print(
      
      
      ggplot(Neur_forest, aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(Neur_forest$yi),max(Neur_forest$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b_Neur == "Odds Ratio (OR)") {name = "Odds Ratio (95% confidence interval)"}
                            else if (input$expo_b_Neur == "beta coefficient of the variable"){name = "beta coefficient (95% CI)"}
                            else if (input$expo_b_Neur == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b_Neur == "Prevalence Ratio (PR)"){name = "Prevalence ratio (95% CI)"}
                            else if (input$expo_b_Neur == "p value of the Odds Ratio"){name = "p value of the Odds Ratio"}
                            else if (input$expo_b_Neur == "p value of the beta coefficient of the variable"){name = "p value of the beta coefficient"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b_Neur == "Odds Ratio (OR)"|input$expo_b_Neur == "Prevalence Ratio (PR)"|input$expo_b_Neur == "p value of the odds ratio") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow ~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    
    width_svg = 11, height_svg = if(input$expo_b_Neur == "beta coefficient of the variable"){28} 
    else if (input$expo_b_Neur == "Odds Ratio (OR)") {15}
    else if (input$expo_b_Neur == "Prevalence Ratio (PR)") {25} else {10}
    ,
    #width_svg = 12, height_svg = 12,
    options = list(
      opts_selection(
        type = "multiple", css = "fill:#0c0000;stroke:black;"),
      opts_hover(css = "fill:#0c0000;stroke:black;cursor:pointer;")
    ))
    #x
  })
  
  
  
  
  #
  #forest1 <- forest[c(-1,-2,-3, -4, -5, -6, -7, -8, -9, -10, -11, -12)]
  #The dt output code
  output$my_table <- renderDataTable({
    
    out <- forest[forest$Reference %in% selected_state(), ]
    if( nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata <- out[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac <- as.datatable(formattable(newdata,align = c("l", rep("c", NCOL(newdata) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = x ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(x,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = x ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(x,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = x ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(x,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = x ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(x,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = x ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(x,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = x ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(x,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = x ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(x,bg.picker))),
      
      "Overall" = formatter("span",
                            style = x ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(x,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For upper R
  
  output$my_table_up <- renderDataTable({
    
    out23 <- forest[forest$Reference %in% selected_state_up(), ]
    if( nrow(out23) < 1 ) return(NULL)
    row.names(out23) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23 <- out23[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23 <- as.datatable(formattable(newdata23,align = c("l", rep("c", NCOL(newdata23) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  
  #### For AR
  
  output$my_table_ar <- renderDataTable({
    
    out23ar <- forest[forest$Reference %in% selected_state_ar(), ]
    if( nrow(out23ar) < 1 ) return(NULL)
    row.names(out23ar) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23ar <- out23ar[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23ar <- as.datatable(formattable(newdata23ar,align = c("l", rep("c", NCOL(newdata23ar) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For GI
  
  output$my_table_gi <- renderDataTable({
    
    out23gi <- forest[forest$Reference %in% selected_state_gi(), ]
    if( nrow(out23gi) < 1 ) return(NULL)
    row.names(out23gi) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23gi <- out23gi[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23gi <- as.datatable(formattable(newdata23gi,align = c("l", rep("c", NCOL(newdata23gi) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
  #### For Neuro
  
  output$my_table_Neur <- renderDataTable({
    
    out23Neur <- forest[forest$Reference %in% selected_state_Neur(), ]
    if( nrow(out23Neur) < 1 ) return(NULL)
    row.names(out23Neur) <- NULL
    #datatable(out ,selection='single')
    #out
    
    #newdata <- out[c(-1,-3, -4, -5,-6,-7,-15, -16, -17, -18)]
    newdata23Neur <- out23Neur[c(1, 10, 11, 12, 13, 14,15, 16, 17)]
    tablereac23Neur <- as.datatable(formattable(newdata23Neur,align = c("l", rep("c", NCOL(newdata23Neur) - 1)), list(
      #CPOE = color_tile("white", "green"),
      #VERBAL = color_tile("white", "red"),
      #WRITTEN = color_tile("white", "red"),
      Confounding = formatter("span",
                              style = j ~ style(display  = "block",
                                                "border-radius" = "50%",
                                                height="30px",
                                                margin="auto",
                                                width="30px",
                                                "font-size"="9px", 
                                                "line-height"="30px",
                                                "text-align"="center",
                                                "padding-right" = "4px",
                                                color = "black",
                                                "background-color" = sapply(j,bg.picker))
                              
      ),
      "Selection of participants" = formatter("span",
                                              style = j ~ style(display  = "block",
                                                                "border-radius" = "50%",
                                                                height="30px",
                                                                margin="auto",
                                                                width="30px",
                                                                "font-size"="9px", 
                                                                "line-height"="30px",
                                                                "text-align"="center",
                                                                "padding-right" = "4px",
                                                                color = "black",
                                                                "background-color" = sapply(j,bg.picker))),
      
      "Measurement of exposure" = formatter("span",
                                            style = j ~ style(display  = "block",
                                                              "border-radius" = "50%",
                                                              height="30px",
                                                              margin="auto",
                                                              width="30px",
                                                              "font-size"="9px", 
                                                              "line-height"="30px",
                                                              "text-align"="center",
                                                              "padding-right" = "4px",
                                                              color = "black",
                                                              "background-color" = sapply(j,bg.picker))),
      "Missing Data" = formatter("span",
                                 style = j ~ style(display  = "block",
                                                   "border-radius" = "50%",
                                                   height="30px",
                                                   margin="auto",
                                                   width="30px",
                                                   "font-size"="9px", 
                                                   "line-height"="30px",
                                                   "text-align"="center",
                                                   "padding-right" = "4px",
                                                   color = "black",
                                                   "background-color" = sapply(j,bg.picker))),
      "Measurement of outcome" = formatter("span",
                                           style = j ~ style(display  = "block",
                                                             "border-radius" = "50%",
                                                             height="30px",
                                                             margin="auto",
                                                             width="30px",
                                                             "font-size"="10px", 
                                                             "line-height"="30px",
                                                             "text-align"="center",
                                                             "padding-right" = "4px",
                                                             color = "black",
                                                             "background-color" = sapply(j,bg.picker))),
      "Selection of reported results" = formatter("span",
                                                  style = j ~ style(display  = "block",
                                                                    "border-radius" = "50%",
                                                                    height="30px",
                                                                    margin="auto",
                                                                    width="30px",
                                                                    "font-size"="9px", 
                                                                    "line-height"="30px",
                                                                    "text-align"="center",
                                                                    "padding-right" = "4px",
                                                                    color = "black",
                                                                    "background-color" = sapply(j,bg.picker))),
      "Measurement of interventions" = formatter("span",
                                                 style = j ~ style(display  = "block",
                                                                   "border-radius" = "50%",
                                                                   height="30px",
                                                                   margin="auto",
                                                                   width="30px",
                                                                   "font-size"="9px", 
                                                                   "line-height"="30px",
                                                                   "text-align"="center",
                                                                   "padding-right" = "4px",
                                                                   color = "black",
                                                                   "background-color" = sapply(j,bg.picker))),
      
      "Overall" = formatter("span",
                            style = j ~ style(display  = "block",
                                              "border-radius" = "50%",
                                              height="30px",
                                              margin="auto",
                                              width="30px",
                                              "font-size"="9px", 
                                              "line-height"="30px",
                                              "text-align"="center",
                                              "padding-right" = "4px",
                                              color = "black",
                                              "background-color" = sapply(j,bg.picker)))
      
      
    )), rownames = FALSE, escape = F, selection = c("single"),class="compact",options = list(ordering=F, bFilter=F))
    
    
  })
  
 
  
  #reactive table based on the selected row 
  tbl_reactive <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew <- forest123[forest123$Reference %in% selected_state(), ]
    gew<- gew[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew[as.numeric(input$my_table_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
  })
  ##### For upper R
  
  tbl_reactive_up <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1 <- forest123[forest123$Reference %in% selected_state_up(), ]
    gew1<- gew1[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1[as.numeric(input$my_table_up_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  
  ##### For AR
  
  tbl_reactive_ar <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1ar <- forest123[forest123$Reference %in% selected_state_ar(), ]
    gew1ar<- gew1ar[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1ar[as.numeric(input$my_table_ar_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  
  ##### For GI
  
  tbl_reactive_gi <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1gi <- forest123[forest123$Reference %in% selected_state_gi(), ]
    gew1gi<- gew1gi[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1gi[as.numeric(input$my_table_gi_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  
  ##### For Neuro
  
  tbl_reactive_Neur <- reactive({
    #t(input$filter(forest123, study == selected_state()))
    gew1Neur <- forest123[forest123$Reference %in% selected_state_Neur(), ]
    gew1Neur<- gew1Neur[c(12, 4, 5, 6, 7, 8, 9, 10, 11)]
    t(gew1Neur[as.numeric(input$my_table_Neur_rows_selected[1]),])
    #t(forest123[as.character(input$my_table_rows_selected[1]),])
    #t(forest123[input$my_table_rows_selected %in% selected_state())
    #t(forest123[selected_state(), ])
    
  })
  
  #here's the table displayed in our modal
  output$modal_table <- DT::renderDataTable({
    tbl_reactive()
    
  })
  
  ### for Upper R
  
  output$modal_table_up <- DT::renderDataTable({
    tbl_reactive_up()
  })
  
  ### for AR
  
  output$modal_table_ar <- DT::renderDataTable({
    tbl_reactive_ar()
  })
  
  ### for GI
  
  output$modal_table_gi <- DT::renderDataTable({
    tbl_reactive_gi()
  })
  
  ### for Neuro
  
  output$modal_table_Neur <- DT::renderDataTable({
    tbl_reactive_Neur()
  })
  
  #our modal dialog box
  myModal <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for Upper R
  
  myModal_up <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_up'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for AR
  
  myModal_ar <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_ar'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for GI
  
  myModal_gi <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_gi'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #### for Neuro
  
  myModal_Neur <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table_Neur'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #event to trigger the modal box to appear
  observeEvent(input$my_table_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal())
    
  }) 
  
  ### For upper R
  observeEvent(input$my_table_up_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_up())
    
  }) 
  
  ### For AR
  observeEvent(input$my_table_ar_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_ar())
    
  }) 
  
  ### For GI
  observeEvent(input$my_table_gi_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_gi())
    
  }) 
  
  ### For Neuro
  observeEvent(input$my_table_Neur_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal_Neur())
    
  })
  
  
  output$mytable1234 <- DT::renderDataTable({
    #datatable(df())
    DT::datatable(cafo3, escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 20))
  })
  
  
  
  ## * risk of bias ####
  output$bias <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_up <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_ar <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_gi <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
  output$bias_Neur <- renderPlotly({
    #gg <- r22 %>% filter(Refid %in% selected_id()) %>% 
    gg <- r22 %>% filter(Categorized.class==selected_class()) %>% 
      ggplot(aes(x = `Type of Bias`, fill = Bias)) + 
      geom_bar(position = "fill") + coord_flip() + 
      scale_fill_manual(values = color_table$Color) + 
      scale_x_discrete(labels = rev(c("Overall", "Confounding", "Measurement of Exposure", 
                                      "Measurement of Outcome", "Measurement of Interventions","Missing Data", "Selection of Participants",
                                      "Selection of Reported Result"))) +
      ylab("Proportion")
    ggplotly(gg)
  })
  
})


