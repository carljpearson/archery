
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
                                 "Rounds (Official)" = 3), 
                  selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      br(),
      checkboxInput(inputId = "dataviewswitch",
                    "View data",
                    value=F),
      conditionalPanel(
        condition = "output.dataview",
        dataTableOutput("data")
      )
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #dataview
  output$dataview <- reactive({
    input$dataviewswitch == TRUE
  })
  outputOptions(output, "dataview", suspendWhenHidden = FALSE)
  
  
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
      ggplot(aes(x=round,y=real_score)) +
      geom_boxplot(aes(fill=End)) +
      geom_point(aes(fill=End),
                 position=position_jitterdodge(jitter.width = .3,seed=5))+
      theme_tufte(base_family="sans"
                  ) +
      theme(text = element_text(size=20)) +
      coord_cartesian(ylim=c(0,5)) +
      scale_fill_brewer(palette="BrBG") +
      labs(title="Accuracy per arrow over ends and rounds",
           x="Rounds",
           y="Score")
    
  })
  
  round_plot_object <- reactive({
    
  
  df_sum %>%
  ggplot(aes(x=as.factor(round),y=score)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    geom_point(position=position_jitter(height=0,width = .2)) +
    stat_summary(geom="line", fun.y="mean",group=1) +
    theme_tufte(base_family="sans") +
    coord_cartesian(ylim=c(0,5)) +
    scale_fill_brewer(palette="BrBG") +
    theme(text = element_text(size=20)) +
    labs(title="Average round accuracy",
         subtitle = "Dots are end accuracy",
         x="Rounds",
         y="Score")
    
  })
  
  official_plot_object <- reactive({
    
    df<-data
    df %>%
      group_by(round=as.factor(Round),end=as.factor(End_unordered)) %>%
      summarise(score=sum(official_score),n=n()) %>%
      mutate(perfect=n*10) %>%
      mutate(percentage_score = score/perfect)%>%
      ggplot(aes(x=round,y=percentage_score)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      geom_point(position=position_jitter(height=0,width = .2)) +
      stat_summary(geom="line", fun.y="mean",group=1) +
      coord_cartesian(ylim = c(0,1)) +
      theme_tufte(base_family="sans") +
      theme(text = element_text(size=20)) +
      labs(title="Offical score sum percentage compared to possible perfect score",
           subtitle = "Dots are end score",
           x="Rounds",
           y="Score") 
    
  })
  
  output$plot <- renderPlot({
    
    if(input$select==1){
      
      end_plot_object()
      
    } else if(input$select==2) {
      
      round_plot_object()
    } else {
      official_plot_object()
    }
    
    })
  
  
  
  
  
  
} 


# Run the application 
shinyApp(ui = ui, server = server)