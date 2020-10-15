library(shiny)
library(tidyverse)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(selectInput(inputId = "state",
                            label = "State",
                            choices = unique(covid19$state),
                            multiple = TRUE),
                submitButton(text = "Submit"),
                plotOutput(outputId = "covid19date"))

server <- function(input, output) {
  output$covid19date <- renderPlot({
    covid19 %>% 
      filter(cases >=20,
             state %in% input$state) %>%
      group_by(state) %>%
      mutate(daysSince = row_number()) %>%
    
    ggplot(aes(x = daysSince, 
               y = cases, 
               group = state,
               color = state)) +
      scale_y_log10(labels = scales::comma) +
      geom_path(size = 1) +
      labs(title = "Rate of COVID-19 Cases in the US",
           x = "Total Cases >20 the Past Week",
           y = "Total Cases") +
      theme(legend.position = "") 
  })
}
shinyApp(ui = ui, server = server)