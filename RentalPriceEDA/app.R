library(shiny)
library(dplyr)
library(ggplot2)

df <- read.csv("rentkl_clean.csv")

set.seed(123)

df$City <- as.factor(df$City)

shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("cosmo"),
      "Rental Price Exploratory Data Analysis Dashboard",
      tabPanel("Home",
               sidebarPanel(
                 h3("Calculate Return on Investment (ROI)"),
                 h5("While what constitutes a 'good' rate can vary depending on an individual's investment strategy, location, and market conditions,
                    generally, a return between 6% and 8% is considered decent,
                    while a return of 10% or more is viewed as excellent."),
                 numericInput("propertyPrice", "Property Price (RM)", 0),
                 numericInput("incomePerMonth", "Rental Income per Month (RM)", 0),
                 numericInput("totalExpense", "Total Expense per Year (RM)", 0),
                h4("Rental Yield Per Annum"),
                h4(textOutput("rentalYieldAnnum"))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Property",
                            selectInput("city", "Select City", choices = levels(df$City)),
                            h3("Average Built Up Size (sqft)"),
                            h4(textOutput("averageBuiltUpsZie")),
                            h3(textOutput("HeaderCity")),
                            plotOutput("barplotPropertyName"),
                            h3("Preferred Furnishing Type"),
                            plotOutput("barplotPropertyFurnishing"),
                            h3("Preferred Number of Bedroom"),
                            plotOutput("barplotPropertyBedroom")
                   )
                   #tabPanel("Tab 2", "This panel is intentionally left blank"),
                   #tabPanel("Tab 3", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("About",
               fluidPage(
                 fluidRow(
                   mainPanel(
                     includeHTML('about.html') 
                   )
                 )
               )
               )
      )
    ),
  server = function(input, output) {
    
    output$HeaderCity <- renderText({ 
      paste("Top 5 Property Rent Listings in ", input$city)
    })
    
    output$averageBuiltUpsZie <- renderPrint({
      # Filter data based on selected city
      filtered_data <- df %>%
        filter(df$City == input$city)
      
      # Calculate property counts
      ave_built_up_size <- round(mean(filtered_data$BuiltUpSize),digits=0)
      cat(toString(ave_built_up_size))
      
    })
    
    output$barplotPropertyName <- renderPlot({
      # Filter data based on selected city
      filtered_data <- df %>%
        filter(df$City == input$city)
      
      # Calculate property counts
      property_counts <- filtered_data %>%
        group_by(PropertyName) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      
      # Select top N counts
      top_n_counts <- head(property_counts, n = 5)
      
      # Plot barplot
      ggplot(top_n_counts, aes(x = PropertyName, y = count, fill = PropertyName)) +
        geom_bar(stat = "identity") +
        labs(x = "Property Name", y = "Frequency") +
        theme_minimal()
    })
    
    output$barplotPropertyFurnishing <- renderPlot({
      # Filter data based on selected city
      filtered_data <- df %>%
        filter(df$City == input$city)
      
      # Calculate property counts
      furnishing_counts <- filtered_data %>%
        group_by(Furnishing) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      
      # Plot barplot
      ggplot(furnishing_counts, aes(x = Furnishing, y = count, fill = Furnishing)) +
        geom_bar(stat = "identity") +
        labs(x = "Furnishing Type", y = "Frequency") +
        theme_minimal()
    })
    output$barplotPropertyBedroom <- renderPlot({
      # Filter data based on selected city
      filtered_data <- df %>%
        filter(df$City == input$city)
      
      # Calculate property counts
      bedroom_counts <- filtered_data %>%
        group_by(NoOfBedroom) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      
      # Plot barplot
      ggplot(bedroom_counts, aes(x = NoOfBedroom, y = count, fill = NoOfBedroom)) +
        geom_bar(stat = "identity") +
        labs(x = "No of Bedroom(s)", y = "Frequency") +
        theme_minimal()
    })
    output$rentalYieldAnnum <- renderPrint({
             perAnnumRentalInc <- as.numeric(input$incomePerMonth)*12
             grossYield = round((perAnnumRentalInc/as.numeric(input$propertyPrice))*100, digits = 2)
             netYield = round(((perAnnumRentalInc - as.numeric(input$totalExpense))/as.numeric(input$propertyPrice))*100, digits = 2)
             cat("Gross Rental Yield (%): ",toString(grossYield),"\n","Net Rental Yield (%): ",toString(netYield))
    })
  }
)