library(shiny)
library(ggplot2)

insurance <- read.csv("insurance.csv")
insurance$smoker <- factor(insurance$smoker, levels = c("no","yes"), labels = c(0,1))
insurance$charges <- insurance$charges^0.15
set.seed(111)
indices <- sample(1:nrow(insurance), 0.75 * nrow(insurance))
insurance_train <- insurance[indices, ]
str(insurance_train)
# and 'lambda' is the Box-Cox transformation parameter
insurance_lm3 <- lm(charges ~ age + bmi + smoker, data = insurance_train)
lambda <- 0.15

# Define UI
ui <- fluidPage(
  titlePanel("Insurance Charges Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", min = 18, max = 100, value = 25),
      numericInput("bmi", "BMI:", min = 15, max = 50, value = 25),
      selectInput("smoker", "Smoker:", c("No" = 0, "Yes" = 1), selected = 0),
      numericInput("children", "Number of Children:", min = 0, max = 10, value = 0),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      plotOutput("modelPlot"),
      verbatimTextOutput("predictedValue")
    )
  )
)

# Define Server
server <- function(input, output) {
  # Function to retransform predictions
  retransform_predictions <- function(predictions, lambda) {
    return(predictions^(1/lambda))
  }
  
  # Function to make predictions with confidence intervals
  make_predictions <- function(new_data) {
    # Apply Box-Cox transformation to 'charges' variable
    new_data$charges <- new_data$charges^lambda
    
    # Make predictions using the linear regression model
    predictions <- predict.lm(insurance_lm3, newdata = new_data, interval = "predict")
    
    # Retransform predictions
    retransformed_predictions <- retransform_predictions(predictions, lambda)
    
#    if (!is.data.frame(retransformed_predictions)) {
#      retransformed_predictions <- as.data.frame(retransformed_predictions)
#    }
    
    rtf_predictions <- retransformed_predictions[,1]
    rtf_lower <- retransformed_predictions[,2]
    rtf_upper <- retransformed_predictions[,3]
    #retransformed_predictions <- retransform_predictions(predictions[, 1], lambda)
    #retransformed_intervals <- retransform_predictions(predictions[, 2:3], lambda)
    
    return(list(predictions = rtf_predictions, lower = rtf_lower, upper = rtf_upper))
  }
  

  # Reactive expression to handle predictions
  calculate_predictions <- reactive({
    # Create a data frame from user inputs
    new_data <- data.frame(
      age = input$age,
      bmi = input$bmi,
      smoker = input$smoker,
      children = input$children,
      charges = 1  # Placeholder, as the actual 'charges' value will be predicted
    )
    
    # Make predictions
    result <- make_predictions(new_data)
    
    reactiveValues(
      tpredictions = result$predictions,
      tlower = result$lower,
      tupper = result$upper)
    
#    predictions$tpredictions <- result$predictions
#    predictions$tlower <- result$lower
#    predictions$tupper <- result$upper
    
#    return(result)
    })
  
  # Observe button click and update the plot and text output
  observeEvent(input$predictButton, {
    output$modelPlot <- renderPlot({
      # Plot the linear regression model with intervals
      ggplot(insurance_train, aes(x = age, y = charges)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, col = "pink") +
        labs(title = "Linear Regression Model",
             x = "Age",
             y = "Charges")
    })
    
    output$predictedValue <- renderText({
      # Display the retransformed predicted value, lower, and upper boundaries
#      paste("Predicted Value:", predictions())
      
      paste("Predicted Value:", round(calculate_predictions()$tpredictions, 2),
            "\nLower Boundary:", round(calculate_predictions()$tlower, 2),
            "\nUpper Boundary:", round(calculate_predictions()$tupper, 2))
      
#      paste("Predicted Value:", round(tpredictions, 2),
#            "\nLower Boundary:", round(tlower, 2),
#            "\nUpper Boundary:", round(tupper, 2))
    })
  })
}

# Run the Shiny App
shinyApp(ui, server)
