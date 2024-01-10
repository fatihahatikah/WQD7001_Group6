# Load necessary libraries for model training
library(caret)
library(randomForest)
library(e1071)
library(Metrics)
library(shiny)

# Read the dataset
df <- read.csv("/Users/fatihahatikah/Documents/Principles of Data Science/Assignments/rentkl_clean.csv")

# Set seed for reproducibility
set.seed(123)

# Ensure 'City', 'PropertyName', and 'Postcode' are factors
df$City <- as.factor(df$City)
df$PropertyName <- as.factor(df$PropertyName)
df$Postcode <- as.factor(df$Postcode)

# Define the user interface
ui <- fluidPage(
  titlePanel("RentEstimate Pro: Rental Price Prediction Tool"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "City:", choices = levels(df$City)),
      selectInput("postcode", "Postcode:", choices = 1),  # Will be updated based on city
      selectInput("propertyName", "Property Name:", choices = 1),  # Will be updated based on city and postcode
      selectInput("furnishing", "Furnishing:", choices = c("Fully Furnished", "Unfurnished")),
      numericInput("bedrooms", "Number of Bedrooms:", min = 1, max = 5, value = 1),
      numericInput("bathrooms", "Number of Bathrooms:", min = 1, max = 5, value = 1),
      numericInput("size", "Size (sqft):", min = 500, max = 3115, value = 500),
      numericInput("parking", "Number of Parkings:", min = 0, max = 4, value = 1),
      actionButton("predict", "Predict Rental Price")
    ),
    mainPanel(
      textOutput("price")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  #Update postcode choices based on selected city
  observeEvent(input$city, {
    postcodes <- unique(df$Postcode[df$City == input$city])
    updateSelectInput(session, "postcode", choices = postcodes)
  })
  
  # Update property name choices based on selected city and postcode
  observeEvent(input$postcode, {
    propertyNames <- unique(df$PropertyName[df$Postcode == input$postcode])
    updateSelectInput(session, "propertyName", choices = propertyNames)
  })
  
  # Load the trained model
  model <- readRDS("/Users/fatihahatikah/Documents/Principles of Data Science/Assignments/randomforest.RDS")
  
  observeEvent(input$predict, {
    # Create a data frame from the input values
    input_data <- data.frame(
      BuiltUpSize = input$size,
      City = factor(input$city, levels = levels(df$City)),
      Furnishing = input$furnishing, #factor(input$furnishing, levels = c("Fully Furnished", "Unfurnished")),
      NoOfBathroom = input$bathrooms,
      NoOfBedroom = input$bedrooms,
      NoOfParking = input$parking,
      Postcode = factor(input$postcode, levels = levels(df$Postcode)),
      PropertyName = factor(input$propertyName, levels = levels(df$PropertyName))
    )
    
    # Predict the rental price
    # Handling factors not present in the training data
    valid_levels <- sapply(input_data, function(x) all(levels(x) %in% levels(df[[names(x)]])))
       if(all(valid_levels)) {
      predicted_price <- predict(model, newdata = input_data)
      
      # Output the predicted price
      predicted_price <- predict(model, newdata = input_data)
      output$price <- renderText({
        paste("Estimated Rental Price: RM", round(predicted_price, 2))
      })
      } else {
        output$price <- renderText("Selected factor levels not present in the training data")
      }
  })
}

# Run the app
shinyApp(ui, server)