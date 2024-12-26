# an example of app.R

library(shiny)

source("WHO_data.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("Global Health Trends"),
  
  # Sidebar with a slider input widget
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country_id",
        label = "Select country",
        choices = unique(who_data_clean$country),
        selected = "Ghana",
        multiple = FALSE
      ),
      sliderInput("input_1", min = 1,value = 30, max = 50, label = "life expectancy"),
      
    ),
    
    # Show a plot 
    mainPanel(
      plotOutput("my_plot"),
      
    )
  )
)









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- who_data
  
  filter_data <- reactive({data %>% filter(country == input$country_id)})
  
  
  
  plot_1 <- reactive({
    ggplot(filter_data(), aes(x = year)) +
      geom_line(aes(y = life_expect)) +
      theme_bw() +
      labs(title = "Life expectancy per country over time")
  })
  
  output$my_plot <- renderPlot({
    plot_1()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)