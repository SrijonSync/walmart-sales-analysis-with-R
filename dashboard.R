walmart<-read.csv(file = "C:/srijon410/walmart.csv")
store_data <- walmart[walmart$Store == 5,]
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

# Replace this with your actual data loading mechanism
for.out <- data.frame(
  original = c(343048.3, 325345.4, 313358.2, 319550.8),
  forecast = c(334994.7, 323719.7, 320072.3, 319959.1),
  week = c(712, 713, 714, 715) # Assuming you have a 'week' column as well
)

# Define the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Create a tabset
    tabsetPanel(
      tabPanel("Table View", DTOutput("salesTable")),
      tabPanel("Bar Chart", plotlyOutput("salesBarChart"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Output for the interactive table
  output$salesTable <- renderDT({
    datatable(for.out, options = list(
      pageLength = 5, 
      searchHighlight = TRUE, 
      autoWidth = TRUE
    ))
  }, server = FALSE)
  
  # Output for the interactive bar chart
  output$salesBarChart <- renderPlotly({
    p <- ggplot(for.out, aes(x = factor(week))) +
      geom_bar(aes(y = original, text = paste("Original: ", original)), stat = "identity", fill = "red") +
      geom_bar(aes(y = forecast, text = paste("Forecast: ", forecast)), stat = "identity", fill = "darkgreen", position = position_dodge()) +
      labs(x = 'Week', y = 'Sales', title = 'Weekly Sales vs Forecast') +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
    
    # Make it interactive with ggplotly
    ggplotly(p, tooltip = c("text", "y")) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Sales"),
             hovermode = "closest")
  })
}

# Run the application
shinyApp(ui, server)


