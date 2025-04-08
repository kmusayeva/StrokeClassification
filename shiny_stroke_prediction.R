library(shiny)
library(tidymodels)

# Load the saved logistic regression model
model <- readRDS("stroke_model.rds")

ui <- fluidPage(
  titlePanel("Stroke Risk Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = c("Male", "Female", "Other")),
      numericInput("age", "Age", value = 50, min = 0, max = 120),
      selectInput("hypertension", "Hypertension", choices = c("0" = 0, "1" = 1)),
      selectInput("heart_disease", "Heart Disease", choices = c("0" = 0, "1" = 1)),
      selectInput("ever_married", "Ever Married", choices = c("Yes", "No")),
      selectInput("work_type", "Work Type", choices = c("Private", "Self-employed", "Govt_job", "children", "Never_worked")),
      selectInput("Residence_type", "Residence Type", choices = c("Urban", "Rural")),
      numericInput("avg_glucose_level", "Avg Glucose Level", value = 100, min = 0),
      numericInput("bmi", "BMI", value = 25, min = 10, max = 60),
      selectInput("smoking_status", "Smoking Status", choices = c("formerly smoked", "never smoked", "smokes", "Unknown")),
      actionButton("predict", "Predict Stroke Risk")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    new_data <- tibble(
      gender = input$gender,
      age = input$age,
      hypertension = as.factor(input$hypertension),
      heart_disease = as.factor(input$heart_disease),
      ever_married = input$ever_married,
      work_type = input$work_type,
      Residence_type = input$Residence_type,
      avg_glucose_level = input$avg_glucose_level,
      bmi = input$bmi,
      smoking_status = input$smoking_status
    )
    
    prediction <- predict(model, new_data, type = "prob")
    
    output$result <- renderPrint({
      cat(paste0("Predicted Stroke Probability: ", round(prediction$.pred_1, 3)))
    })
  })
}

shinyApp(ui = ui, server = server)
