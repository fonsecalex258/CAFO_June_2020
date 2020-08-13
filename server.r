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
        axis.text.y = element_text(hjust = 15),
        axis.title.y = element_text(face = "bold", size=30),
        text=element_text(family='Times'),
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
      "tabl" = p("Perro")
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
    choices <- forest$mm
    #forest %>%
    #filter(mm == selected_state()) %>%
    #pull(mm) %>% unique() %>% sort() 
    selectInput("expo_b",
                "Effect size (ES) measure method",
                choices = choices,
                multiple = FALSE,
                selected = choices[1])
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
      "coef" = p("The following table shows all the outcomes that were categorized as upper respiratory tract. Allergic rhinitis and nasal irritation were the most common outcomes grouped in this category.")
    )
  })
  
  #####copying for AMR
  output$ar_res_intro_text <- renderUI({
    switch(
      input$ar_res_btn,
      "sp" = p("Most articles related to upper respiratory disease were published in United States, Netherlands and Germany."),
      "ts" = p("This timeline shows the date of publication of references in which outcome related to upper respiratory tract were studied. This category was analyzed by 6 out 16 relevant references included."),
      "coef" = p("The following table shows all the outcomes that were categorized as upper respiratory tract. Allergic rhinitis and nasal irritation were the most common outcomes grouped in this category.")
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
  
  ## ** geographic distribution ####
  output$map_low_res <- renderLeaflet({
    leaflet(cafoo) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map
      addCircleMarkers(lng = cafoo$long, lat = cafoo$lat,
                       radius = log(cafoo$`Number of Studies`)*8,
                       popup = ~paste("Country:", cafoo$Country, "<br>",
                                      "Number of Studies:", cafoo$`Number of Studies`))
  })
  
  #####copying for upper
  
  output$map_up_res <- renderLeaflet({
    leaflet(cafoo) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map
      addCircleMarkers(lng = cafoo$long, lat = cafoo$lat,
                       radius = log(cafoo$`Number of Studies`)*8,
                       popup = ~paste("Country:", cafoo$Country, "<br>",
                                      "Number of Studies:", cafoo$`Number of Studies`))
  })
  
  #####copying for AMR
  
  output$map_ar_res <- renderLeaflet({
    leaflet(cafoo) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-40.679728, 34.738366, zoom = 2) %>%  ## Set/fix the view of the map
      addCircleMarkers(lng = cafoo$long, lat = cafoo$lat,
                       radius = log(cafoo$`Number of Studies`)*8,
                       popup = ~paste("Country:", cafoo$Country, "<br>",
                                      "Number of Studies:", cafoo$`Number of Studies`))
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
  ## ** timeline ####
  output$timeline_low_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ## Insert into a dataframe
    datt <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),
      content = timedata2$Author,
      start = timedata2$paperYear,
      end = NA
    )
    timevis(datt)
  })
  
  #######copying for up
  
  output$timeline_up_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ## Insert into a dataframe
    datt <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),
      content = timedata2$Author,
      start = timedata2$paperYear,
      end = NA
    )
    timevis(datt)
  })
  
  #######copying for AMR
  
  output$timeline_ar_res <- renderTimevis({
    ## Only select authors and year information columns
    timedata <- dataset %>% 
      filter(Categorized.class == selected_class()) %>% 
      select(paperInfo, paperYear) %>% distinct() %>%
      ## Extract only author names from paperInfo column
      ## Extract string comes before the period
      mutate(Author = sub("\\..*", "", paperInfo))
    # timedata$paperYear[8] <- 2006  Fixed missing data on the original dataset
    timedata2 <- timedata %>% select(paperYear, Author)
    ## Insert into a dataframe
    datt <- data.frame(
      ## make it reactive
      id = 1:nrow(timedata2),
      content = timedata2$Author,
      start = timedata2$paperYear,
      end = NA
    )
    timevis(datt)
  })
  
  output$measure_all_low_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=T, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for up
  
  output$measure_all_up_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 50))
    #subset(cafoo, cafoo$`Country` == "Germany")
    #broadfilter <- subset(dataset, Categorized.class == selected_class())
  })
  
  #######copying for AMR
  
  output$measure_all_ar_res <- DT::renderDataTable({
    sat <- dataset %>% filter(Categorized.class==selected_class())%>%
      group_by(Categorized.class, Outcome.variable) %>%summarise(Frequency = length(Outcome.variable))
    DT::datatable(sat, rownames = FALSE,escape = FALSE, options = list(ordering=F, bFilter=F, pageLength = 50))
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
  ## * forest plot ####
  
  selected_state <- reactive({
    input$plot_selected
  })
  
  forest_data <- reactive({
    forest_data <- forest %>% filter(
      mm == input$expo_b & Categorized.class==selected_class())
  })
  
  
  #output$low_rsp_dt <- DT::renderDataTable({
  #  forest_data()
    #newdatas <- forest_data()[c(1,2)]
  #})
  
  
  
  
  output$plot <- renderGirafe({
    x <- girafe(code = print(
      
      
      ggplot(forest_data(), aes(y=Reference, x=yi, col=study))+
        #Add data points and color them black
        geom_point_interactive(
          aes( data_id = Reference, tooltip = inter), size = 6, position=position_dodge(5)) +
        #Add 'special' points for the summary estimates, by making them diamond shaped
        #geom_point(data=subset(forest, h=='Summary'), color='black', shape=18, size=4)+
        #add the CI error bars
        geom_errorbarh(aes(xmin= lowerci, xmax=upperci),height=.4, position=position_dodge(5) )+
        coord_cartesian(xlim= c(min(forest_data()$yi),max(forest_data()$yi)))+
        #coord_cartesian(xlim= c(lowerci,upperci))+
        #geom_jitter(position=position_dodge(3))+
        #coord_cartesian(xlim= if (input$expo_b == "OR") {c(min(forest$lowerci),max(forest$yi))} else {c(-5,5)})+
        #Specify the limits of the x-axis and relabel it to something more meaningful
        scale_x_continuous( if (input$expo_b == "OR") {name = "Odds ratio (95% confidence interval)"}
                            else if (input$expo_b == "beta"){name = "Beta (95% confidence interval)"}
                            else if (input$expo_b == "Mean difference"){name = "Mean difference"}
                            else if (input$expo_b == "PR"){name = "Prevalence ratio"}
                            else if (input$expo_b == "OR p value"){name = "Odds ratio p value"}
                            else if (input$expo_b == "beta p value"){name = "Beta p value"}
        )+
        scale_y_discrete('References',position="right")+
        #Give y-axis a meaningful label
        #ylab('Reference')+ 
        #Add a vertical dashed line indicating an effect size of zero, for reference
        geom_vline(xintercept= if (input$expo_b == "OR"|input$expo_b == "PR"|input$expo_b == "OR p value") {1} else {0}, color='black', linetype='dashed')+
        #Create sub-plots (i.e., facets) based on levels of setting
        #And allow them to have their own unique axes (so authors don't redundantly repeat)
        facet_grid(narrow + expoboard~., scales= 'free', space='free', switch="y")+
        #annotate("segment", x = UCL_l, xend = if (input$expo_b == "OR") {3.3} else {5.4}, y = study, yend = study, alpha=0.6, arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = if(input$expo_b == "OR" & is.na(LCL_l)){lowerci} else {LCL_l-yi}, xend = if (input$expo_b == "OR") {-3.3} else {-5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        #geom_segment(aes(x = UCL_l , xend = if (input$expo_b == "OR") {3.3} else {5.4}  , y = study , yend = study) ,arrow = arrow(length = unit(0.2, "inches"),type = "closed"))+
        apatheme
    ),
    width_svg = 11, height_svg = if(input$expo_b == "beta"){28} 
    else if (input$expo_b == "OR") {25}
    else if (input$expo_b == "PR") {25} else {10}
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
    newdata <- out[c(9, 10, 11, 12, 13, 14,15, 16)]
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
  
  #here's the table displayed in our modal
  output$modal_table <- DT::renderDataTable({
    tbl_reactive()
  })
  
  #our modal dialog box
  myModal <- function(failed=FALSE){
    modalDialog(
      div(dataTableOutput('modal_table'), style = "font-size:85%"),
      width= "fit-content",
      easyClose = TRUE
      
    )
  }
  
  #event to trigger the modal box to appear
  observeEvent(input$my_table_rows_selected,{
    #observeEvent(input$selected_state,{
    showModal(myModal())
    
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
})
