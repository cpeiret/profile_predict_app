# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
library(scales)
library(tibble)
library(ggradar)  # Ensure you have the ggradar package installed
library(brms)
library(marginaleffects)
library(tidyr)
library(ggdist)


# Load data and model
multi_df <- read.csv('multi_df.csv', sep = ',')
m_balanced <- readRDS('m_balanced.RData')

# Define the desired order for the income levels and ages
ordered_income_levels <- c("First 10% group", "Second 10% group", "Third 10% group", "Fourth 10% group",
                           "Fifth 10% group", "Sixth 10% group", "Seventh 10% group", "Eighth 10% group",
                           "Ninth 10% group", "Tenth 10% group", "Income unknown")
ordered_ages <- 6:99
multi_df$Urbanity <- as.numeric(multi_df$Urbanity)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Model Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("wopc", "WoPC:", choices = unique(multi_df$WoPC), options = list(create = FALSE)),
      selectInput("age", "Age:", choices = ordered_ages),
      selectInput("gender", "Gender:", choices = unique(multi_df$gender)),
      selectInput("ethnicity", "Ethnicity:", choices = unique(multi_df$ethnicity)),
      selectInput("occupation", "Occupation:", choices = unique(multi_df$occupation)),
      selectInput("education", "Education:", choices = unique(multi_df$education)),
      selectInput("income", "Income:", choices = ordered_income_levels),
      selectInput("hh_composition", "Household Composition:", choices = unique(multi_df$hh_composition))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs", # Adds the tab system
        tabPanel("Prediction Plot", plotOutput("barplot")),  # First tab with the prediction plot
        tabPanel("Cluster Radar Plot", imageOutput("radar_image"))  # Second tab with the radar plot image
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store Urbanity, entropy_car, and entropy_pt
  urbanity <- reactiveVal()
  entropy_car <- reactiveVal()
  entropy_pt <- reactiveVal()
  Corop <- reactiveVal()
  index <- reactiveVal()
  
  observeEvent(input$wopc, {
    selected_wopc <- multi_df[multi_df$WoPC == input$wopc, ]
    urbanity(selected_wopc$Urbanity[1])
    entropy_car(selected_wopc$entropy_car[1])
    entropy_pt(selected_wopc$entropy_pt[1])
    Corop(selected_wopc$Corop[1])
    index(selected_wopc$index[1])
  })
  
  # Render the bar plot (prediction plot)
  output$barplot <- renderPlot({
    new_data <- data.frame(
      age = as.numeric(input$age),
      gender = input$gender,
      ethnicity = input$ethnicity,
      occupation = input$occupation,
      education = input$education,
      income = input$income,
      hh_composition = input$hh_composition,
      Urbanity = urbanity(),
      entropy_car = entropy_car(),
      entropy_pt = entropy_pt(),
      Corop = Corop(),
      index = index()
    )
    
    pred <- predictions(m_balanced, newdata = new_data) %>%
      as_tibble() %>%
      posterior_draws() %>%
      arrange(estimate) %>%
      mutate(group = as.character(group)) %>%  # Convert factor to character
      mutate(group = replace_na(group, "5")) %>%  # Replace NA with "5"
      mutate(group = factor(group))  # Convert back to factor
    
    ggplot(pred, aes(x = draw, y = group, fill = group)) +
      stat_halfeye(normalize = 'xy') +
      labs(x = 'Predicted probability of belonging', y = element_blank()) +
      theme_minimal(base_family = "Clear Sans", base_size = 16) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(family = "Clear Sans", face = "bold"),
            axis.title.x = element_text(face = 'bold'),
            axis.title.y = element_text(face = 'bold'),
            strip.text = element_text(family = "Clear Sans", face = "bold",
                                      size = rel(0.75), hjust = 0),
            strip.background = element_rect(fill = "grey90", color = NA),
            axis.text.y = element_blank()) +
      scale_fill_manual(
        values = c("#264653","#2a9d8f","#e9c46a","#f4a261","#e76f51"),  # Customize colors
        labels = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5"),  # Customize labels
        name = 'Cluster'
      )
  })
  
  # Render the radar plot image
  output$radar_image <- renderImage({
    # Return a list with the image path, width, and height
    list(src = "cluster_radars.png",  # Path to your radar plot image
         contentType = "image/png",
         width = 900,  # Adjust width
         height = 600,  # Adjust height
         alt = "Cluster Radar Plot")
  }, deleteFile = FALSE)  # Set deleteFile to FALSE so the file is not removed
}

# Run the application 
shinyApp(ui = ui, server = server)
