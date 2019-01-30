
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
      
      
      p("This app gives a live update on my archery accuracy progress.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("round_plot"),
      dataTableOutput("data_round")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    data_round
  })
  
  round_plot_object <- reactive({
    
    df_sum <- df_sum
    
    df_sum %>%
      ggplot(aes(x=round,
                 y=score,
                 fill=end)) +
      geom_bar(stat="identity",
               position="dodge") +
      geom_errorbar(aes(ymin=ifelse(sd>0,0,score-sd),
                        ymax=ifelse(sd>5,5,score+sd)),
                    color="black",
                    position="dodge",
                    alpha=.3) +
      theme_tufte(base_family="sans") +
      coord_cartesian(ylim=c(0,5)) +
      scale_fill_brewer(palette="BrBG") +
      labs(title="Score per end over rounds")
    
  })
  
  output$round_plot <- renderPlot({round_plot_object()})
  
  
  
  
  
  
} 


# Run the application 
shinyApp(ui = ui, server = server)