#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Old Faithful Geyser Data visualization"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title:", "Old Faithful Geyser Data"),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      ),
      selectInput("color", "Color:", colors(), "coral3"),
      checkboxInput("showScatter", label = "Show scatter plot", FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distplot"),
      downloadButton('downloadDistPlot', 'Download Plot'),
      conditionalPanel(
        "input.showScatter",
        plotOutput("scatter"),
        downloadButton('downloadScatPlot', 'Download Plot')
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  plotInputDist = function() {
    ggplot(faithful, aes(x = waiting)) +
      geom_histogram(color = "white",
                     fill = input$color,
                     bins = input$bins) +
      ggtitle(input$title) +
      xlab("Waiting [m]") +
      ylab("Frequency")
  }
  
  plotInputScat = function() {
    ggplot(faithful, aes(x = waiting, y = eruptions)) +
      geom_point(colour = input$color) +
      ggtitle(input$title) +
      xlab("Waiting [m]") +
      ylab("Eruption [m]")
  }
  
  output$distplot <- renderPlot({
    print(plotInputDist())
  })
  
  output$scatter <- renderPlot({
    print(plotInputScat())
  })
  
  
  output$downloadDistPlot <- downloadHandler(
    filename =  function() {
      paste('distplot.png', sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plotInputDist(), device = "png")
    }
  )
  output$downloadScatPlot <- downloadHandler(
    filename =  function() {
      paste('scatterplot.png', sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plotInputScat(), device = "png")
    }
  )
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
