library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(reshape2)

ui <- fluidPage(

  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  
  titlePanel("Exploration des Diamants"),

  sidebarLayout(
    sidebarPanel(
      
      radioButtons("boutton1", label = ("Colorier les points en rose ?"),
                   choices = list("Oui" = 1, "Non" = 2 ), 
                   selected = 1),
      
      
      selectInput("Color_Input", "Choisir une couleur à filtrer :",
                  choices = unique(diamonds$color)),
      
      sliderInput("price",
                  "Prix maximum :",
                  min = 300,
                  max = 20000,
                  value = 5000),
      
      
      actionButton(inputId = "boutton2", label = "Visualiser le graph"),
    ),

    mainPanel(
      plotOutput("diamondsplot"), 
      DTOutput ("tablo")
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
      theme_minimal()
  })
  
  output$value <- renderPrint({ input$boutton1 })
}


shinyApp(ui = ui, server = server)
