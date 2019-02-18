
#
library(tidyverse)
library(shiny)
library(lmerTest)
library(googlesheets)
library(ggthemes)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Archery Metrics - Carl Pearson"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      p("This app gives a live update on my archery accuracy progress. My targets have possible arrow scores of 0-5. Each arrow score is part of an end (usually 3-6 arrows). Each end is part of a round. "),
      selectInput("select", h3("Select plot"), 
                  choices = list("Ends" = 1, 
                                 "Rounds" = 2,
                                 "Rounds (Official)" = 3,
                                 "Accuracy over grouping"=4), 
                  selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot60"),
      br(),
      checkboxInput(inputId = "dataviewswitch60",
                    "View data",
                    value=F),
      conditionalPanel(
        condition = "output.dataview60",
        dataTableOutput("data60")
      ),
      hr(),
      
      br(),
      checkboxInput(inputId = "dataviewswitch",
                    "View old plot and data",
                    value=F),
      conditionalPanel(
        condition = "output.dataview",
        plotOutput("plot"),
        dataTableOutput("data")
      )
      
      
      
    ) #end main panel
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #dataview
  output$dataview <- reactive({
    input$dataviewswitch == TRUE
  })
  outputOptions(output, "dataview", suspendWhenHidden = FALSE)
  
  #dataview60
  output$dataview60 <- reactive({
    input$dataviewswitch60 == TRUE
  })
  outputOptions(output, "dataview60", suspendWhenHidden = FALSE)
  
  data <- isolate({
    
    key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/17qRcQmidYnkvlPqK7uTLuLpyzVyU0D7498WZnRXsPUI/edit?usp=sharing")
    
    key %>%
      gs_key() %>%
      gs_read('tracking') -> df
    
  })
  
  
  
  data_sum <- isolate({
    
    data %>%
      group_by(round=as.numeric(Round),end=as.factor(End_unordered)) %>%
      summarise(score=mean(real_score),
                sd=sd(real_score,na.rm=TRUE),
                range_max=max(real_score,na.rm=TRUE),
                range_min=min(real_score,na.rm=TRUE),
                distance=mean(distance_yards)
      ) -> df_sum
    
  })
  
  
  
  data_round <- isolate({
    data %>% 
      group_by(round=Round) %>%
      summarise(score=mean(real_score),
                sd=sd(real_score,na.rm=TRUE),
                range_max=max(real_score,na.rm=TRUE),
                range_min=min(real_score,na.rm=TRUE),
                distance=mean(distance_yards)
      ) -> df_round
    
  })
  output$data <- renderDataTable({
    data_sum
  })
  output$data_sum <- renderDataTable({
    data_sum
  })
  output$data_round <- renderDataTable({
    data
  })
  
  end_plot_object <- reactive({
    
    data=df
    df %>%
      mutate(round=as.factor(Round)) %>%
      mutate(End=as.factor(End_unordered)) %>%
      mutate(full_lab=paste0(round,", ",distance_yards," yd")) %>%
      ggplot(aes(x=full_lab,y=real_score)) +
      stat_summary(aes(fill=End),fun.y = "mean", geom = "bar",position="dodge")+
      geom_point(aes(fill=End),
                 position=position_jitterdodge(jitter.width = .1,seed=5))+
      theme_tufte(base_family="sans"
      ) +
      #theme(text = element_text(size=20)) +
      coord_cartesian(ylim=c(0,5)) +
      scale_fill_brewer(palette="BrBG") +
      labs(title="Accuracy per arrow over ends and rounds",
           subtitle = "20cm target",
           x="Rounds",
           y="Score") 
    
  })
  
  
  
  round_plot_object <- reactive({
    
  
    df_sum %>%
      mutate(distance=as.factor(distance)) %>%
      ggplot(aes(x=round,y=score)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      geom_point(aes(color=distance),position=position_jitter(height=0,width = .2),size=2) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      theme_tufte(base_family="sans") +
      coord_cartesian(ylim=c(0,5)) +
      scale_color_brewer(palette="Set1") +
      labs(title="Average round accuracy",
           subtitle = "20cm target, dots are end accuracy",
           x="Rounds",
           y="Score")
    
  })
  
  
  
  official_plot_object <- reactive({
    
    df<-data
    df %>%
      group_by(round=as.factor(Round),end=as.factor(End_unordered)) %>%
      summarise(score=sum(official_score),n=n(),Distance=as.factor(mean(distance_yards))) %>%
      mutate(perfect=n*10) %>%
      mutate(percentage_score = score/perfect)%>%
      ggplot(aes(x=round,y=percentage_score)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      geom_point(aes(color=Distance),position=position_jitter(height=0,width = .2),size=2) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      coord_cartesian(ylim = c(0,1)) +
      scale_color_brewer(palette="Set1") +
      theme_tufte(base_family="sans") +
      labs(title="Offical score sum percentage compared to possible perfect score",
           subtitle = "20cm target, dots are end score",
           x="Rounds",
           y="Score") 
    
  })
  
  
  output$plot <- renderPlot({
    
    if(input$select==1){
      
      end_plot_object()
      
    } else if(input$select==2) {
      
      round_plot_object()
    } else if(input$select==3){
      official_plot_object()
    } else{
      NULL
    }
    
    })
  
  
  data60 <- isolate({
    
    key <- extract_key_from_url("https://docs.google.com/spreadsheets/d/17qRcQmidYnkvlPqK7uTLuLpyzVyU0D7498WZnRXsPUI/edit?usp=sharing")
    
    key %>%
      gs_key() %>%
      gs_read('tracking_60cm') -> df60
    
  })
  
  data_sum60 <- isolate({
    
    data60 %>%
      group_by(round=as.numeric(Round),end=as.factor(End_unordered)) %>%
      summarise(score=mean(real_score),
                sd=sd(real_score,na.rm=TRUE),
                range_max=max(real_score,na.rm=TRUE),
                range_min=min(real_score,na.rm=TRUE),
                distance=mean(distance_yards)
      ) -> df_sum60
    
    key %>%
      gs_key() %>%
      gs_read('grouping') -> df_group
    
    df_group %>% 
      na.omit(date )%>%
      select(round=Round,end=End,Distance,Distance_actual) %>%
      inner_join(.,df_sum60) -> df_sum60
    
  })
  
  

  
  output$data60 <- renderDataTable({
    data_sum60
  })
  
  end_plot_object60 <- reactive({
    
    data60=df60
    df60 %>%
      mutate(round=as.factor(Round)) %>%
      mutate(End=as.factor(End_unordered)) %>%
      mutate(full_lab=paste0(round,", ",distance_yards," yd")) %>%
      ggplot(aes(x=full_lab,y=real_score)) +
      stat_summary(aes(fill=End),fun.y = "mean", geom = "bar",position="dodge")+
      geom_point(aes(fill=End),
                 position=position_jitterdodge(jitter.width = .1,seed=5))+
      theme_tufte(base_family="sans"
      ) +
      #theme(text = element_text(size=20)) +
      coord_cartesian(ylim=c(0,5)) +
      scale_fill_brewer(palette="BrBG") +
      labs(title="Accuracy per arrow over ends and rounds",
           subtitle = "60cm target",
           x="Rounds",
           y="Score") 
    
  })
  
  round_plot_object60 <- reactive({
    
    data_sum60 -> df_sum
    
    df_sum %>%
      mutate(distance=as.factor(distance)) %>%
      ggplot(aes(x=round,y=score)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      geom_point(aes(color=distance),position=position_jitter(height=0,width = .2),size=2) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      theme_tufte(base_family="sans") +
      coord_cartesian(ylim=c(0,5)) +
      scale_color_brewer(palette="Set1") +
      labs(title="Average round accuracy",
           subtitle = "60cm target, dots are end accuracy",
           x="Rounds",
           y="Score")
    
  })
  
  official_plot_object60 <- reactive({
    
    df<-data60
    df %>%
      group_by(round=as.factor(Round),end=as.factor(End_unordered)) %>%
      summarise(score=sum(official_score),n=n(),Distance=as.factor(mean(distance_yards))) %>%
      mutate(perfect=n*10) %>%
      mutate(percentage_score = score/perfect)%>%
      ggplot(aes(x=round,y=percentage_score)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      geom_point(aes(color=Distance),position=position_jitter(height=0,width = .2),size=2) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      coord_cartesian(ylim = c(0,1)) +
      scale_color_brewer(palette="Set1") +
      theme_tufte(base_family="sans") +
      labs(title="Offical score sum percentage compared to possible perfect score",
           subtitle = "60cm target, dots are end score",
           x="Rounds",
           y="Score") 
    
  })
  
  
  grouping_plot <- reactive({
    
    df <- data_sum60
    
    df %>%
      ggplot(aes(x=Distance_actual,score)) +
      geom_point() +
      geom_smooth(method='lm',formula=y~x) +
      labs(
        title="Score plotted against grouping",
        x="Perimeter sum, cm",
        y="Point score"
        
      ) +
      theme_tufte(base_family="sans") 
    
  })
  
  
  
  
  output$plot60 <- renderPlot({
    
    if(input$select==1){
      
      end_plot_object60()
      
    } else if(input$select==2) {
      
      round_plot_object60()
    } else if(input$select==3) {
      official_plot_object60()
    } else {
      grouping_plot()
      
    }
    
  })
  
  
  
  
  
} 


# Run the application 
shinyApp(ui = ui, server = server)