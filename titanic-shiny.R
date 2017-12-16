library(shiny)
library(rpart)
library(vcdExtra)

titanic <- vcdExtra::Titanicp

ui <- fluidPage(
  titlePanel("Titanic Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pclass", label = "Passenger Class",
                  choices = list("1st" = "1st", "2nd" = "2nd", "3rd" = "3rd"),
                  selected = "1st"),
      radioButtons("sex", label = "Sex",
                   choices = list("Female" = "female", "Male" = "male"), 
                   selected = "female"),
      sliderInput("age",
                  "Age:",
                  min = 0.1667,
                  max = 80,
                  step = 0.5,
                  value = 25),
      sliderInput("sibsp",
                  "Number of Siblings or Spouses Aboard:",
                  min = 0,
                  max = 8,
                  step = 1,
                  value = 2),
      sliderInput("parch",
                  "Number of Parents or Children Aboard:",
                  min = 0,
                  max = 9,
                  step = 1,
                  value = 4),
      sliderInput("dataprop",
                  "Proportion of Data Used:",
                  min = 0.1,
                  max = 1,
                  step = 0.1,
                  value = 0.8)
    ),
    mainPanel(
      textOutput("prediction"),  # labels are arguments 
      plotOutput("tree")
    )
  ))


server <- function(input, output) {         # both input, output are lists; what goes in to ui (input) and what comes out of ui (output)
  titanic.fit <- reactive({    # way of compartmentalizing the work going on in the server; where work happens when ticker is changed 
    index = sample(nrow(titanic), round(nrow(titanic) * input$dataprop), replace = F)  # reactive creates a new symbol that acts like an input slider
    fit.titanic = rpart(survived ~., data = titanic[index, ])
  })
  output$prediction <- renderText({
    print(titanic.fit())
    new_data = data.frame(
      pclass = input$pclass,
      sex = input$sex,
      age = input$age,
      sibsp = input$sibsp,
      parch = input$parch
    )
    titanic.predict <- predict(titanic.fit(), new_data, type = "class")
    paste("You ", titanic.predict, ".", sep = "")   # assigned to output$prediction
  })
  output$tree <- renderPlot({
    plot(titanic.fit())
    text(titanic.fit())
  })
}

shinyApp(ui = ui, server = server)

