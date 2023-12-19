
library(shiny)
library(dplyr)
library(ggplot2)
library(countrycode)
library(maps)
library(ggplot2)
library(maps)
library(dplyr)
library(RColorBrewer)
library(sf)
library(tidyr)

BaseTable <- read_excel("C:/Users/vijay/Desktop/France 3/R/Group/MarketMetrics.xlsx")

# Create the filtered table
Vizualisation_1_Prefered_Product <- BaseTable %>%
  filter(Preferred_Product != "None") %>%
  select(Gender, Preferred_Product, Language, Country)

#Getting Coordinates from the Maps and CountryCode Package
world_map_data <- map_data("world")

# Calculate central coordinates for each country
coordinates_ <- world_map_data %>%
  group_by(region) %>%
  summarise(
    lon = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  )
#Renaming for join
coordinates_ <- coordinates_ %>% 
  rename(Country = region)

column_names <- names(coordinates_)

Vizualisation_1_Prefered_Product <- Vizualisation_1_Prefered_Product %>%
  left_join(coordinates_, by = "Country")

#Manually Filling the coordinates for UK and Holland
Vizualisation_1_Prefered_Product <- Vizualisation_1_Prefered_Product %>%
  mutate(
    lon = ifelse(Country == "United Kingdom", -3.4360, lon),
    lat = ifelse(Country == "United Kingdom", 55.3781, lat)
  )

Vizualisation_1_Prefered_Product <- Vizualisation_1_Prefered_Product %>%
  mutate(
    lon = ifelse(Country == "Holland" | Country == "Netherlands", 5.2913, lon),
    lat = ifelse(Country == "Holland" | Country == "Netherlands", 52.1326, lat)
  )

#Removing missing lon and lat values since the data for the country is not defined properly

Vizualisation_1_Prefered_Product <- Vizualisation_1_Prefered_Product[!is.na(Vizualisation_1_Prefered_Product$lon), ]

#Check for missing values
#rows_with_missing_lon <- Vizualisation_1_Prefered_Product[is.na(Vizualisation_1_Prefered_Product$lon), ]
# Displaying the rows, including the country column
#print(rows_with_missing_lon, n = 33)


Vizualisation_4 <- BaseTable %>%
  group_by(Application, Country) %>%
  summarise(Usage_Count = n(), .groups = 'drop')

Vizualisation_4 <- Vizualisation_4 %>%
  left_join(coordinates_, by = "Country")

#Manually Filling the coordinates for UK and Holland
Vizualisation_4 <- Vizualisation_4 %>%
  mutate(
    lon = ifelse(Country == "United Kingdom", -3.4360, lon),
    lat = ifelse(Country == "United Kingdom", 55.3781, lat)
  )

Vizualisation_4 <- Vizualisation_4 %>%
  mutate(
    lon = ifelse(Country == "Holland" | Country == "Netherlands", 5.2913, lon),
    lat = ifelse(Country == "Holland" | Country == "Netherlands", 52.1326, lat)
  )

#Removing missing lon and lat values since the data for the country is not defined properly

Vizualisation_4 <- Vizualisation_4[!is.na(Vizualisation_4$lon), ]


ui <- fluidPage(
  titlePanel("Marekting Metrics"),
  tabsetPanel(
    tabPanel("Mapping Preferred Products",
             selectInput("selectedVis1", "Select Product:", 
                         choices = list("Sports book fixed - odd" = "1_1", 
                                        "Sports book live - action" = "1_2",
                                        "Poker BossMedia" = "1_3", 
                                        "Casino BossMedia" = "1_4",
                                        "Supertoto" = "1_5", 
                                        "Games Vs" = "1_6", 
                                        "Games bwin" = "1_7",                           
                                        "Casino Chartwell" = "1_8")),
             selectInput("genderFilter", "Select Gender:",
                         choices = c("All", "Male", "Female"),
                         selected = "All"),
             uiOutput("visOutput1")
    ),
    tabPanel("Understanding Application Usage",
             selectInput("selectedVis2", "Select Metric:", 
                         choices = list("Average Transaction Amount" = "2",
                                        "Total Count of Games" = "2_1",
                                        "Average Games Played "  = "2_2")),
             uiOutput("visOutput2")
    ),
    tabPanel("Product Preference Understanding",
             selectInput("selectedVis3", "Select Metric :", 
                         choices = list("Product Preference - Gender" = "3", 
                                        "Product Preference - Application" = "3_1",
                                        "Count of Application - Gender" = "3_2")),
             selectInput("genderFilter3", "Select Gender:",
                         choices = list("All", "Male", "Female"),
                         selected = "All"),
             uiOutput("visOutput3")
    ),
    tabPanel("Mapping Application Usage Count", plotOutput("plot4")),
    tabPanel("User Metrics",
             selectInput("selectedVis5", "Select Metric:", 
                         choices = list("Average ROI" = "5",
                                        "Average Winning" = "5_1",
                                        "Average Stake" = "5_1_1")),
             uiOutput("visOutput5")
    ),
    tabPanel("Profit Loss Analysis", plotOutput("plot6"))
  )
)

server <- function(input, output) {
  
  
  output$visOutput1 <- renderUI({
    switch(input$selectedVis1,
           "1_1" = plotOutput("plot1_1"),
           "1_2" = plotOutput("plot1_2"),
           "1_3" = plotOutput("plot1_3"),
           "1_4" = plotOutput("plot1_4"),
           "1_5" = plotOutput("plot1_5"),
           "1_6" = plotOutput("plot1_6"),
           "1_7" = plotOutput("plot1_7"),
           "1_8" = plotOutput("plot1_8"))
  })
  
  
  output$visOutput2 <- renderUI({
    switch(input$selectedVis2,
           "2" = plotOutput("plot2"),
           "2_2" = plotOutput("plot2_2"),
           "2_1" = plotOutput("plot2_1"))
  })
  
  output$visOutput3 <- renderUI({
    switch(input$selectedVis3,
           "3" = plotOutput("plot3"),
           "3_1" = plotOutput("plot3_1"),
           "3_2" = plotOutput("plot3_2"))
    
  })
  
  
  output$visOutput5 <- renderUI({
    switch(input$selectedVis5,
           "5" = plotOutput("plot5"),
           "5_1" = plotOutput("plot5_1"),
           "5_1_1" = plotOutput("plot5_1_1"))
  })
  
  
  
  # Visualization 1.1
  output$plot1_1 <- renderPlot({
    
    V1_1 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Sports book fixed-odd",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_1) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Sports book fixed-odd' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.2
  output$plot1_2 <- renderPlot({
    V1_2 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Sports book live-action",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_2) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Sports book live-action' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.3
  output$plot1_3 <- renderPlot({
    V1_3 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Poker BossMedia",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_3) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Poker BossMedia' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.4
  output$plot1_4 <- renderPlot({
    V1_4 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Casino BossMedia",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_4) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Casino BossMedia' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.5
  output$plot1_5 <- renderPlot({
    V1_5 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Supertoto",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_5) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Supertoto' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.6
  output$plot1_6 <- renderPlot({
    V1_6 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Games VS",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_6) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Games VS' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  
  # Visualization 1.7
  output$plot1_7 <- renderPlot({
    V1_7 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Games bwin",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_7) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Games bwin' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  # Visualization 1.8
  output$plot1_8 <- renderPlot({
    V1_8 <- Vizualisation_1_Prefered_Product %>%
      filter(Preferred_Product == "Casino Chartwell",
             if (input$genderFilter != "All") Gender == input$genderFilter else TRUE)
    
    
    
    ggplot(data = V1_8) +
      borders("world", colour = "gray50", fill = "gray50") +  # Draw the world map
      geom_point(aes(x = lon, y = lat, color = Gender), size = 2, alpha = 0.6) +
      scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
      theme_minimal() +
      labs(title = "Distribution of Preferred Product : 'Casino Chartwell' by Gender",
           x = "Longitude",
           y = "Latitude",
           color = "Gender")
  })
  
  
  # Visualization 2
  output$plot2 <- renderPlot({
    Vizualisation_2 <- BaseTable %>%
      select(Application, TransAmountPerBet, Total_games_played)
    
    
    ggplot(Vizualisation_2, aes(x = Application, y = TransAmountPerBet)) +
      geom_bar(stat = "summary", fun = "mean", fill = "red") +
      theme_minimal() +
      labs(title = "Average Transaction Amount per Bet by Application",
           x = "Application", y = "Average Transaction Amount per Bet") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  
  # Visualization 2.1
  output$plot2_1 <- renderPlot({
    Vizualisation_2 <- BaseTable %>%
      select(Application, TransAmountPerBet, Total_games_played)
    
    
    ggplot(Vizualisation_2, aes(x = Application, y = Total_games_played)) +
      geom_bar(stat = "summary", fun = "sum", fill = "lightblue") +
      theme_minimal() +
      labs(title = "Total Count of Games Played by Application",
           x = "Application", y = "Total Games Played") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Visualization 2.2
  output$plot2_2 <- renderPlot({
    Vizualisation_2 <- BaseTable %>%
      select(Application, TransAmountPerBet, Total_games_played)
    
    
    ggplot(Vizualisation_2, aes(x = Application, y = Total_games_played)) +
      geom_bar(stat = "summary", fun = "mean", fill = "lightgreen") +
      theme_minimal() +
      labs(title = "Average Number of Games Played by Application",
           x = "Application", y = "Average Games Played") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Visualization 3
  output$plot3 <- renderPlot({
    Vizualisation_3 <- BaseTable %>%
      filter(Preferred_Product != "None", if (input$genderFilter3 != "All") Gender == input$genderFilter3 else TRUE) %>%
      group_by(Preferred_Product, Gender) %>%
      summarise(Count = n(), .groups = 'drop')
    
    
    
    ggplot(Vizualisation_3, aes(x = Preferred_Product, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      labs(title = "Product Preferences by Gender",
           x = "Preferred Product",
           y = "Count",
           fill = "Gender") +
      scale_fill_brewer(palette = "Set1")
    
  }) 
  
  # Visualization 3.1
  output$plot3_1 <- renderPlot({
    Vizualisation_3.1 <- BaseTable %>%
      filter(Preferred_Product != "None") %>%
      group_by(Preferred_Product, Application) %>%
      summarise(Count = n(), .groups = 'drop')
    
    color_palette <- rainbow(length(unique(Vizualisation_3.1$Application)))
    
    # Creating the vizualisation
    ggplot(Vizualisation_3.1, aes(x = Preferred_Product, y = Count, fill = Application)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = color_palette) +
      theme_minimal() +
      labs(title = "Product Preferences by Application",
           x = "Preferred Product",
           y = "Count",
           fill = "Application") 
    
  })
  
  # Visualization 3.2
  output$plot3_2 <- renderPlot({
    Vizualisation_3.2 <- BaseTable %>%
      filter(if (input$genderFilter3 != "All") Gender == input$genderFilter3 else TRUE)%>%
      group_by(Application,Gender) %>%
      summarise(Count = n(), .groups = 'drop')
    
    
    ggplot(Vizualisation_3.2, aes(x = Count, y = Application, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(title = "Count by Application used by Gender",
           x = "Application",
           y = "Count",
           fill = "Gender") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # Visualization 4
  output$plot4 <- renderPlot({
    ggplot(Vizualisation_4, aes(x = lon, y = lat, size = Usage_Count, color = Application)) +
      geom_point(alpha = 0.7) +
      borders("world", colour = "gray50", fill = "gray50") +
      theme_minimal() +
      labs(title = "Application Usage Count by Country",
           x = "Longitude",
           y = "Latitude",
           size = "Usage Count",
           color = "Application") +
      scale_size(range = c(2, 10)) 
  })
  
  
  
  # Visualization 5
  output$plot5 <- renderPlot({
    Vizualisation_5 <- BaseTable %>%
      filter(Preferred_Product != "None") %>%
      group_by(Application, Preferred_Product) %>%
      summarise(AverageROI = mean(UserROI, na.rm = TRUE), .groups = 'drop')
    
    ggplot(Vizualisation_5, aes(x = Preferred_Product, y = AverageROI, fill = Preferred_Product)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Application) +
      theme_minimal() +
      labs(title = " Average ROI Across Products Within Each Application",
           x = "", y = "Average ROI") +  
      theme(axis.text.x = element_blank(),  
            axis.ticks.x = element_blank())
  })
  
  # Visualization 5.1
  output$plot5_1 <- renderPlot({
    Vizualisation_5.1 <- BaseTable %>%
      filter(Preferred_Product != "None") %>%
      select(Application, Preferred_Product,AverageWinning,AverageStake)
    
    
    # Plot For Avg Winning
    
    ggplot(Vizualisation_5.1, aes(x = Preferred_Product, y = AverageWinning, fill = Preferred_Product)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Application) +
      theme_minimal() +
      labs(title = "Average Winning Across Products Within Each Application",
           x = "", y = "Average ROI") +  
      theme(axis.text.x = element_blank(),  
            axis.ticks.x = element_blank())
    
  })
  
  # Visualization 5.1.1
  output$plot5_1_1 <- renderPlot({
    Vizualisation_5.1 <- BaseTable %>%
      filter(Preferred_Product != "None") %>%
      select(Application, Preferred_Product,AverageWinning,AverageStake)
    
    
    # Plot For Avg Winning
    
    ggplot(Vizualisation_5.1, aes(x = Preferred_Product, y = AverageStake, fill = Preferred_Product)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Application) +
      theme_minimal() +
      labs(title = "Average Stake Across Products Within Each Application",
           x = "", y = "Average ROI") +  
      theme(axis.text.x = element_blank(),  
            axis.ticks.x = element_blank())
    
  })
  
  
  
  # Visualization 6
  output$plot6 <- renderPlot({
    Vizualisation_6 <- BaseTable %>%
      filter(User_Status_Profit_or_Loss != "Neither") %>%
      group_by(Application, User_Status_Profit_or_Loss) %>%
      summarise(UserCount = n(), .groups = 'drop')
    
    ggplot(Vizualisation_6, aes(x = Application, y = UserCount, fill = User_Status_Profit_or_Loss)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(title = "Profit vs Loss User Count by Application",
           x = "Application", y = "User Count", fill = "User Status") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
