#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <- fluidPage(theme = shinytheme("superhero"),
   
   
   titlePanel("Know the price of your Diamond"),
   setBackgroundImage(src = "img.jpg"),
   img(src='logo.png', align = "right"),
   sliderInput("carat", "What carat are you looking for?",
               min = 0,
               max = 20.45,
               value = 5),
   selectInput("cut", "Cut",
               choices = c("Astor Ideal" = "Astor Ideal",
                           "Good" = "Good",
                           "Ideal" = "Ideal",
                           "Very Good" = "Very Good"
               )),
   selectInput("clarity","Clarity",
               choices = c("Flawless"= "FL",
                           "Internally Flawless" = "IF",
                           "Very Very Slightly Included-VVS1" = "VVS1",
                           "Very Very Slightly Included-VVS2" = "VVS2",
                           "Very Slightly Included-VS1" = "VS1",
                           "Very Slightly Included-VS2" = "VS2",
                           "Slightly Included-SI1" = "SI1",
                           "Slightly Included-SI2" = "SI2"
               )),
   selectInput("color", "Color",
               choices = c("D" = "D",
                           "E" = "E",
                           "F" = "F",
                           "G" = "G",
                           "H" = "H",
                           "I" = "I",
                           "J" = "J")),
   actionButton("price", "Get Price"),
   #dataTableOutput("rate"),
   h3(textOutput("string")),
   textOutput("pred_price")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data1 <- read.csv("clean_diamond_data.csv")
  #mod4<-lm(log(price)~ cut + clarity + color + I(carat^(1/3)) + carat, data = data1)
  mod <- lm(log(price)~ log(carat) + clarity + color + cut, data = data1)
  observeEvent(input$price,{
    new_data = data.frame(carat = log(input$carat), clarity = input$clarity, color = input$color, cut = input$cut)
    p = predict(mod, newdata = new_data)
    #output$rate <- renderDataTable(new_data)
    string <- "Price of the diamond you are looking for is: "
    output$string <- renderText(string)
    output$pred_price <- renderText(exp(p))
    
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)



















