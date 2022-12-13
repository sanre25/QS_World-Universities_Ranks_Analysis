
#=================================
#    ALL LIBRARIES
#=================================

library(shiny)
library(fmsb)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(rworldmap)
library(geosphere)
library(gpclib)
library(countrycode)
library(RColorBrewer)
library(CGPfunctions)
library(ggthemes)
library(viridis)

#==================================
#          UI
#==================================

ui <- navbarPage( theme = shinytheme("sandstone"),
                  title = "Data Visualization of the QS ranking Universities",
         
                  tabPanel("Piechart",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("dataset1", "Select an Year:",
                                            c("2020" , "2021" , "2022" , "2023"),
                                            selected = "2020"),
                               sliderInput("temp",
                                           "Number of Observations",
                                           min = 0,
                                           max = 1422,
                                           value = 400,
                                           animate = animationOptions(
                                             interval = 100,
                                           )),
                               ),
                             
                             mainPanel(
                               plotOutput(outputId = "piechart")
                             )
                           ),
                  ),
                  tabPanel("Radarchart",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "variable",
                                           label = "Choose a university:",
                                           choices = as.array(unique_uni[[1]]) ,
                                           selected = "Indian Institute of Technology Kanpur (IITK)"
                               )),
                             
                             mainPanel(
                               plotOutput(outputId = "radarplot")
                             )
                           ),
                  ),
                  
                  tabPanel("Barplot",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("dataset3", "Select an Year:",
                                            c("2020" , "2021" , "2022" , "2023"),
                                            selected = "2020")),
                             
                             mainPanel(
                               plotOutput(outputId = "barplot")
                             )
                           ),
                  ),
                  
                  tabPanel("Scatterplot",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("dataset4", "Select an Year:",
                                            c("2020" , "2021" , "2022" , "2023"),
                                            selected = "2020"),
                               checkboxGroupInput(inputId = "var",
                                                  label = "Choose a dataset:",
                                                  choices = c("Score" = "Score",
                                                              "Academic.Reputation" = "Academic.Reputation",
                                                              "International.Faculty.Ratio" = "International.Faculty.Ratio",
                                                              "Employer.Reputation" = "Employer.Reputation" ,
                                                              "Faculty.Student.Ratio" = "Faculty.Student.Ratio" ,
                                                              "Citations.per.Faculty" = "Citations.per.Faculty" , 
                                                              "International.Students.Ratio" = "International.Students.Ratio") ,
                                                  selected = c("Score", "Academic.Reputation")
                               )),
                               
                             mainPanel(
                               plotOutput(outputId = "scatterplot")
                             )
                           ),
                  ),
                  
                  tabPanel("Globe",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("dataset5", "Select an Year:",
                                            c("2020" , "2021" , "2022"),
                                            selected = "2020"),
                               sliderInput("rot1",
                                           "Select upside rotation",
                                           min = 0,
                                           max = 360,
                                           value = 10),
                               
                               sliderInput("rot2",
                                           "Select sideways rotation",
                                           min = -90,
                                           max = 90,
                                           value = 75)),
                             
                             mainPanel(
                               plotOutput(outputId = "globe")
                             )
                           ),
                  ),
                  
                  tabPanel("Top Universities",
      
                             
                             mainPanel(
                               plotOutput(outputId = "graph1"),
                               plotOutput(outputId = "graph2")
                             )
                           ),
                  
                  tabPanel("Dataset",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("dataset6", "Select an Year:",
                                            c("2020" , "2021" , "2022" , "2023"),
                                            selected = "2020"),
                               numericInput(inputId = "obs",
                                            label = "Number of observations to view:",
                                            value = 10)),
                             
                             mainPanel(
                               tableOutput("view")
                             )
                           ),
                  )
)

#================================
#         SERVER 
#================================

server <- function(input, output) {
  
  
  #========================================
  #       USING REACTIVE TO CHANGE OUTPUT
  #========================================
  
  datasetInput1 <- reactive({
    switch(input$dataset1,
           "2020" = QS_Rank_2020,
           "2021" = QS_Rank_2021,
           "2022" = QS_Rank_2022,
           "2023" = QS_Rank_2023)
  })
  
  datasetInput3 <- reactive({
    switch(input$dataset3,
           "2020" = QS_Rank_2020,
           "2021" = QS_Rank_2021,
           "2022" = QS_Rank_2022,
           "2023" = QS_Rank_2023)
  })
  datasetInput4 <- reactive({
    switch(input$dataset4,
           "2020" = QS_Rank_2020[1:499 , ],
           "2021" = QS_Rank_2021[1:499 , ],
           "2022" = QS_Rank_2022[1:499 , ],
           "2023" = QS_Rank_2023[1:499 , ])
  })

  datasetInput6 <- reactive({
    switch(input$dataset6,
           "2020" = QS_Rank_2020,
           "2021" = QS_Rank_2021,
           "2022" = QS_Rank_2022,
           "2023" = QS_Rank_2023)
  })
  
  
  gdpInput <- reactive({
    switch(input$dataset5,
           "2020" = lat_long_cont_gdp_2020,
           "2021" = lat_long_cont_gdp_2021,
           "2022" = lat_long_cont_gdp_2022)
  })
  
  #===============================================================
  # DONUT CHART SHOWING DISTRIBUTION OF COLLEGES IN DIFF CONTINENTS
  #=============================================================== 
  
  output$piechart <- renderPlot({
    
    Continent_2020 <- datasetInput1()[1:input$temp,]$Continent
    cdf <- sort(table(Continent_2020), F) %>% as.data.frame()
    
    cdf <- cdf %>%
      arrange(desc(Continent_2020)) %>%
      mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
    
    ggplot(cdf, aes(x = 2, y = Freq, fill = Continent_2020)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = lab.ypos, label = Freq), color = "white") +
      guides(fill = guide_legend("Continents")) +
      ggtitle(paste("Donut chart showing the respective number \n of universities for different continents in" , input$dataset)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(0.5, 2.5)
    
  })
  
  
  #=============================================
  # SCATTERPLOT SHOWING SCORE DISTRIBUTION
  #=============================================
  
  output$scatterplot <- renderPlot({
    
    attach(datasetInput4())
    matplot(x = 1:499 , cbind(datasetInput4()[, c(input$var)]) , pch = 1 , lwd = 2 , xlab = "Ranking" , ylab = "Score", 
            main = "Scatter plot of the first 500 universities \nagainst there different scores")
    legend("topright" , legend = c(input$var) , col = 1:length(input$var) , pch = 1 , lwd = 2)
    
  })
  
  #=============================================================
  # BARPLOT SHOWING DISTRIBUTION OF COLLEGES IN DIFF COUNTRIES
  #=============================================================
  
  
  output$barplot <- renderPlot({
    
    feq <- table(datasetInput3()$Country)
    country.data <- data.frame(feq)
    country.data = country.data%>%arrange(desc(Freq))
    
    # Draw bar plot
    theme_set(theme_bw())
    ggplot(country.data[1:10,], aes(x=reorder(Var1, Freq), y=Freq), xlab = "Countries", ylab = "No of Universities") + 
      geom_bar(stat="identity", width=.5, fill="orange") +  xlab("Universities") + ylab("No. of universities") +
      labs(title ="Ordered Bar Chart", 
           subtitle = "This chart shows the frequencies of the Universities for respective countries", 
           caption = "") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      coord_flip()
    
  })
  
  
  #===========================================
  #   RADARCHART OF VARIABLES IN THE DATASET
  #===========================================            
  
  
  output$radarplot <- renderPlot({
    
    mat = matrix(c(rep(100 , 1 , 7) , rep(0 , 1 , 7)) , ncol = 7 , nrow = 2 , byrow = T)
    mat = rbind(mat ,as.numeric(QS_Rank_2020[which(QS_Rank_2020$University==input$variable),c(3,7,8,9,10,11,12)]),as.numeric(QS_Rank_2021[which(QS_Rank_2021$University==input$variable),c(3,7,8,9,10,11,12)]) , as.numeric(QS_Rank_2022[which(QS_Rank_2022$University==input$variable),c(3,7,8,9,10,11,12)]) , as.numeric(QS_Rank_2023[which(QS_Rank_2023$University==input$variable),c(3,7,8,9,10,11,12)]))
    colnames(mat) = c("Overall score" , "International Student Ratio" , "International Factulty Ratio" , 
                      "Faculty Student Ratio" , "Citations per Faculty" , "Academic Reputation" , "Employee Reputation")
    create_beautiful_radarchart <- function(data, color = c("red" , "blue" , "green" , "black"), 
                                            vlabels = colnames(data), vlcex = 0.7,
                                            caxislabels = NULL, title = NULL, ...){
      radarchart(
        data, axistype = 1,
        # Customize the polygon
        pcol = color, plwd = 2, plty = 1,
        # Customize the grid
        cglcol = "grey", cglty = 1, cglwd = 0.8,
        # Customize the axis
        axislabcol = "grey", 
        # Variable labels
        vlcex = vlcex, vlabels = vlabels,
        caxislabels = caxislabels, title = title, ...
        
      )
    }
    
    
    
    op <- par(mar = c(1, 2, 2, 1))
    create_beautiful_radarchart(as.data.frame(mat), caxislabels = c(0, 25, 50, 75, 100) , title = input$variable)
    par(op)
    
    legend("bottomleft" , col = c("red" , "blue" , "green" , "black") , legend = c("2020" , "2021" , "2022" , "2023") , pch = 1 , cex = 1 , lwd = 2)
    
    
  })
  
  
  #=====================================================
  #     PLOT SLOPE BAR GRAPH FOR TOP 10 UNIVERSITIES 
  #=====================================================
  
  
  output$graph1 <- renderPlot({
    
    ##   ***Top 10 universities of Year 2020***
    my.data <- data.frame(
      Rank20 = QS_Rank_2020$Rank[1:10],
      University    = QS_Rank_2020$University[1:10]
    )
    
    
    ## ***res.my.data is result DF after merging with sunsequent DF's***
    ## *** Merge with 2021 ***
    res.my.data <- merge(my.data, QS_Rank_2021,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank21'
    res.my.data <- res.my.data[ ,-c(4:13)]
    
    
    ## ***College ETH zurich have different name in 2022 or 2023 Ranking***
    QS_Rank_2022[8,2] <- "ETH Zurich - Swiss Federal Institute of Technology"
    QS_Rank_2023[9,2] <- "ETH Zurich - Swiss Federal Institute of Technology"
    
    
    ## *** Merge with 2022 ***
    res.my.data <- merge(res.my.data, QS_Rank_2022,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank22'
    res.my.data <- res.my.data[ ,-c(5:14)]
    
    
    ## *** Merge with 2023 ***
    res.my.data <- merge(res.my.data, QS_Rank_2023,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- 'Rank23'
    res.my.data <- res.my.data[ ,-c(6:15)]
    
    
    ## *** Converting Ranks into numeric ***
    res.my.data$Rank20 = as.numeric(res.my.data$Rank20)
    res.my.data$Rank21 = as.numeric(res.my.data$Rank21)
    res.my.data$Rank22 = as.numeric(res.my.data$Rank22)
    res.my.data$Rank23 = as.numeric(res.my.data$Rank23)
    
    
    res.my.data <-  res.my.data %>% arrange(Rank20)
    
    df <- res.my.data
    
    colnames(df) <- c("University","2020", "2021", "2022" ,"2023")
    
    df[c(1, 5, 6), 1] <- c("MIT", "CalTech", "ETH Zurich")
    
    Year <- as.character(c(rep(2020, 10), rep(2021, 10),rep(2022, 10), rep(2023, 10)))
    University <- rep(df$University, 4)
    Rank <- c(df$`2020`, df$`2021`, df$`2022`, df$`2023`)
    
    ## sg_df is required DF for ploting graph
    sg_df <- data.frame(Year, University, Rank)
    head(sg_df)
    
    ### ** code 
    
    newggslopegraph(dataframe = sg_df, 
                    Times = Year,
                    Measurement = Rank,
                    Title = "QS ranking of the top 10 universities over last 4 years",
                    SubTitle = "2020 - 2023",
                    Grouping = University, 
                    ReverseYAxis = T, 
                    YTextSize = 5,
                    WiderLabels = T,ThemeChoice = "bw",
                    DataTextSize = 5,
                    Caption = "")
    
  })
  
  #==================================================
  #   PLOT SLOPE BAR GRAPH FOR TOP 7 IITS of India
  #==================================================
  
  output$graph2 <- renderPlot({
    
    IIT <- c(
      "Indian Institute of Technology Bombay (IITB)" ,
      "Indian Institute of Technology Delhi (IITD)"  ,
      "Indian Institute of Technology Madras (IITM)" ,
      "Indian Institute of Technology Kharagpur (IIT-KGP)" ,
      "Indian Institute of Technology Kanpur (IITK)" ,
      "Indian Institute of Technology Roorkee (IITR)" ,
      "Indian Institute of Technology Guwahati (IITG)"
    )
    
    IIT_df <- data.frame(University = IIT)
    
    res.my.data <- merge(IIT_df, QS_Rank_2020,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- '2020'
    res.my.data <- res.my.data[ ,-c(3:12)]
    
    res.my.data <- merge(res.my.data, QS_Rank_2021,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- '2021'
    res.my.data <- res.my.data[ ,-c(4:13)]
    
    res.my.data <- merge(res.my.data, QS_Rank_2022,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- '2022'
    res.my.data <- res.my.data[ ,-c(5:14)]
    
    res.my.data <- merge(res.my.data, QS_Rank_2023,by = "University")
    names(res.my.data)[names(res.my.data) == 'Rank'] <- '2023'
    res.my.data <- res.my.data[ ,-c(6:15)]
    
    res.my.data <-  res.my.data %>% arrange(2020)
    
    df <- res.my.data
    df[, 1] <- c("IITB", "IITD", "IITG", "IITK", "IIT-KGP", "IITM", "IITR") 
    Year <- as.character(c(rep(2020, 7), rep(2021, 7),rep(2022, 7), rep(2023, 7)))
    University <- rep(df$University, 4)
    Rank <- c(df$`2020`, df$`2021`, df$`2022`, df$`2023`)
    
    sg_df <- data.frame(Year, University, Rank)
    
    sg_df$Rank <- as.numeric(sg_df$Rank)
    
    newggslopegraph(dataframe = sg_df, 
                    Times = Year,
                    Measurement = Rank,
                    Title = "QS ranking of the top 7 IIT over last 4 years",
                    SubTitle = "2020 - 2023",
                    Grouping = University, 
                    ReverseYAxis = T, 
                    YTextSize = 5,
                    WiderLabels = T,ThemeChoice = "bw",
                    DataTextSize = 5,
                    Caption = "")
    
    
  })
  
  
  #============================================
  #  GLOBE COMPARING GDPPC AND NO OF COLLEGES
  #============================================
  
  
  output$globe <- renderPlot({
    worldMap <- getMap()
    world.points <- fortify(worldMap)
    world.points$region <- world.points$id
    
    world.df <- world.points[,c("long","lat","group", "region")]
    
    new_df_1 <- world.df %>% 
      mutate(Iso3_1 = countrycode::countrycode(
        sourcevar = region,
        origin = "country.name",
        destination = "iso3c"
      )) 
    
    new_df_2 <- gdpInput() %>% 
      mutate(Iso3_2 = countrycode::countrycode(
        sourcevar = Country,
        origin = "country.name",
        destination = "iso3c"
      )) 
    
    
    new_map_df <- new_df_1 %>% 
      select(long, lat, group,Iso3_1) %>% 
      left_join(new_df_2, by = c("Iso3_1" = "Iso3_2")) 
    
    
    ggplot(new_map_df, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = GDPPC)) +
      scale_fill_viridis(direction = -1) +
      geom_path(aes(x = long, y = lat, group = group)) +
      scale_color_brewer(palette="Set1") +
      geom_point(aes(x = Longitude, y = Latitude, color = Continent)) +
      coord_map("ortho", orientation = c(input$rot1, input$rot2, 0)) +
      xlab("Longitude")  +
      ylab("Latitude") +
      labs(title = "GDP per capita of countries overlayed with location of the Universties") +
      theme_minimal()
  })
  
  #================================
  #       TABLE OF THE DATA
  #================================ 
  
  output$view <- renderTable({
    head(datasetInput6(), n = input$obs)
    
  })
  
  
}

#====================================
#  CONNECTING THE UI AND THE SERVER
#====================================

shinyApp(ui = ui, server = server)
