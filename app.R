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
                   choices = sort(unique(diamonds$color))),
      
      sliderInput("price",
                  "Prix maximum :",
                  min = 300,
                  max = 20000,
                  value = 5000),
      
      
      actionButton(inputId = "boutton2", label = "Visualiser le graph"),
    ),

    mainPanel(
      plotly::plotlyOutput("plot"),
      DTOutput ("tablo")
      
    )
    )
)


server <- function(input, output) {

  thematic::thematic_shiny(font = "auto")
  
  rv <- reactiveValues(df = NULL,
                       dfc = NULL,
                       choix = NULL
                       )
  

  observeEvent(input$boutton2,
               
               {showNotification(
                 paste("prix :", input$price, "& color :", input$Color_Input), 
                 type = "message")
                 
                 rv$df <- diamonds %>%
                   filter(
                     color == input$Color_Input,
                     price <= input$price
                   )
                 
                 rv$dfc <- rv$df
                 rv$choix <- ifelse(input$boutton1 == 1, "pink", "black")
               })
  
  output$value <- renderPrint({ 
    req(rv$choix)
    rv$choix })
  
  
output$plot <- plotly::renderPlotly({
  
  req(rv$dfc)
    
    mygraph <- ggplot(data = rv$dfc %>%
                        filter(color == input$Color_Input, price <= input$price)) +
      aes(x = carat, y = price) +
      geom_point(alpha = 0.6, color = rv$choix )+ 
                   labs(
                     x = "Carat",
                     y = "Price",
                     title = paste("prix :",max(rv$dfc$price), "& color :", unique(rv$dfc$color))
                   ) +
                   theme_minimal() +
                   theme(legend.position = "none") 
    
    plotly::ggplotly(mygraph)
    
  })
  
  
  output$tablo <- renderDT({
   
     req(rv$df)
    
    rv$df %>%
      filter(color == input$Color_Input) %>% 
      select(carat, cut, color, clarity, depth, table, price)  
  }, rownames = FALSE)

}




shinyApp(ui = ui, server = server)
