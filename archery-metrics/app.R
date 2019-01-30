
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
    data_round
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
      coord_cartesian(ylim=c(0,5)) +
      scale_fill_brewer(palette="BrBG") +
      labs(title="Accuracy per arrow over ends and rounds",
           x="Rounds",
           y="Score")
    
  })
  
  output$plot <- renderPlot({
    end_plot_object()
    
    
    })
  
  
  
  
  
  
} 


# Run the application 
shinyApp(ui = ui, server = server)