
library(shiny)
library(sp)

source("predictNextWord.R")


# UI application
ui <- shinyUI(fluidPage(
    titlePanel("Data Science Capstone Project"), h4("Next Word Prediction"),
    h4("Author: Caroline Lisevski"), h4("Date: October 9, 2020"), br(),
    sidebarLayout(sidebarPanel(
            sliderInput("num.predictions", label = h5("Number of Predictions"),
                        min = 1, max = 10, step = 1, value = 1),
            textInput("words", label = h3("Text input"), value = ""),
            textOutput("echo"), br(), textOutput("text")),
        mainPanel(h3("Help Text"), br(),
            p("The slider input is used to set ",
              span("how many predictions", style = "color:red"),"to output."),
            br(), p("Input a phrase to see the possibilities for the next word.")
        ))))

# Server logic required

server <- shinyServer(
    function(input, output){
        getWords <- reactive({
            trimws(input$words)
        })
        output$echo <- renderText({
            if (nchar(getWords()) > 0) {
                paste0("Your text: ", getWords())
            } else {
                "No text entered! Try again!"
            }
        })
        result <- reactive({
            predictWord(getWords(), input$num.predictions)
        })
        output$text <- renderText({
            paste("Predicted word(s):", result())
        })
    }
)

# Run the application 
shinyApp(ui = ui, server = server)
