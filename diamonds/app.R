library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(reshape2)

ui <- fluidPage(

  theme = bs_theme(version = 5),
  titlePanel("Exploration des Diamants"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("price",
                        "Prix maximum :",
                        min = 300,
                        max = 20000,
                        value = 5000)
        ),

      
        mainPanel(
          plotOutput("diamondsplot")
        )
    )
)


server <- function(input, output) {

  thematic::thematic_shiny(font = "auto")
  
  output$diamondsplot <- renderPlot({
    diamonds %>%
      filter(color == input$Color_Input) %>%    
      
      ggplot(aes(x = carat, y = price, color = color)) +
      geom_point(alpha = 0.6) +                
      labs(
        x = "Carat",
        y = "Price",
        title = paste("Diamants de couleur :", input$Color_Input)
      ) +
      theme_minimal()})
}


shinyApp(ui = ui, server = server)
