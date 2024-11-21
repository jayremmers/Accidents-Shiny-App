# This is a test
#
# This is another test
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
# code uploaded
accidents <- read.csv("accident.csv")
weather <- read.csv("weather.csv")

# megre and clean up data
report <- left_join(accidents, weather, by = "ST_CASE")
cities <- report %>%
  filter(CITYNAME == "NEW YORK CITY" | CITYNAME == "LOS ANGELES" | 
           CITYNAME == "CHICAGO")
cities <- cities %>%
  mutate(month = factor(MONTHNAME, levels = month.name, ordered = TRUE))
cities$day <- factor(cities$DAY_WEEKNAME, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                     "Friday", "Saturday", "Sunday"), ordered = TRUE)

cities$CITYNAME <- as.factor(cities$CITYNAME)
cities$WEATHERNAME.x <- as.factor(cities$WEATHERNAME.x)
cities$HOUR <- as.factor(cities$HOUR)
cities$HARM_EVNAME <- as.factor(cities$HARM_EVNAME)
cities$MAN_COLLNAME <- as.factor(cities$MAN_COLLNAME)
cities$ROUTENAME <- as.factor(cities$ROUTENAME)
cities$RELJCT2NAME <- as.factor(cities$RELJCT2NAME)
cities$LGT_CONDNAME <- as.factor(cities$LGT_CONDNAME)

# create less categories
cities <- cities %>%
  mutate(event = case_when(
    HARM_EVNAME %in% c("Motor Vehicle In-Transport") ~ "Vehicle",
    HARM_EVNAME %in% c("Pedestrian", "Pedalcyclist") ~ "Pedestrian",
    TRUE ~ "Object"  # This can be used as a catch-all for any remaining categories
  ))

# create less categories
cities <- cities %>%
  mutate(junction = case_when(
    RELJCT2NAME %in% c("Non-Junction") ~ "Non-Junction",
    RELJCT2NAME %in% c("Intersection-Related", "Intersection") ~ "Intersection",
    RELJCT2NAME %in% c("Entrance/Exit Ramp Related", "Entrance/Exit Ramp") ~ "Entrance/Exit Ramp",
    RELJCT2NAME %in% c("Driveway Access Related", "Driveway Access") ~ "Driveway Access",
    TRUE ~ "Other"  # This can be used as a catch-all for any remaining categories
  ))

names <- c("City" = "CITYNAME",
           "Weather" = "WEATHERNAME.x",
           "Collision" = "event",
           "Street Type" = "ROUTENAME",
           "Junction Type" = "junction",
           "Light Conditions" = "LGT_CONDNAME")

times <- c("Month" = "month",
           "Day" = "day",
           "Hour" = "HOUR")

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinythemes::shinytheme("sandstone"),
    
    # Application title
    titlePanel(" "),

    # Sidebar with selection options
    sidebarLayout(
        sidebarPanel(
            selectInput("plot",
                        "Bar Chart Type:",
                        choices = c("Count", "Percent"),
                        selected = "Count"
                        ),
            selectInput("variable",
                        "X Variable:",
                        choices = names,
                        selected = "STATENAME.x"
                        ),
                    
            selectInput("fill",
                        "Fill Variable:",
                        choices = names,
                        selected = "WEATHER"
                        ),
            
            selectInput("time",
                        "Unit of Time:",
                        choices = times,
                        selected = "month"
                        )
        ),

        # Show plots
        mainPanel(
           plotOutput("barPlot"),
           plotOutput("timePlot")
        )
    )

)


server <- function(input, output) {

    # Bar Chart with fill variable
    output$barPlot <- renderPlot({
       
      #display chart by selected plot type
      switch(input$plot,
             
            # bar chart by count
            "Count" = ggplot(cities, aes_string(x = input$variable, fill = input$fill)) +
                      geom_bar() +
                      coord_flip() +
                      labs(x=NULL, y="Count") +
                      theme_minimal() +
                      theme(legend.position = "bottom"),
            
            # bar chart by percent of total
            "Percent" = ggplot(cities, aes_string(x = input$variable, fill = input$fill)) +
                        geom_bar(position = "fill") +
                        scale_y_continuous(labels = scales::percent_format()) +
                        coord_flip() +
                        labs(x=NULL, y="Percent of Total") +
                        theme_minimal() +
                        theme(legend.position = "bottom")
      )
    })
    
    # count by time period line chart
    output$timePlot <- renderPlot({
      
      # get count
      counts <- cities %>%
        group_by(.data[[input$time]], CITYNAME) %>%
        summarise(n = n(), .groups = 'drop')
      
      # display chart based on time selected
      switch(input$time,
             "HOUR" = counts %>%
               filter(HOUR != 99) %>%
               ggplot(aes_string(x="HOUR", y="n", group="CITYNAME", color="CITYNAME")) +
               geom_line(size = 2) +
               theme_minimal() +
               labs(x=" ",y="Count"),
             
             "day" = ggplot(counts, aes_string(x="day", y="n", group="CITYNAME", color="CITYNAME")) +
               geom_line(size = 2) +
               theme_minimal() +
               labs(x=" ",y="Count"),
             
             "month" = ggplot(counts, aes_string(x="month", y="n", group="CITYNAME", color="CITYNAME")) +
               geom_line(size = 2) +
               theme_minimal() +
               labs(x=" ",y="Count")
      )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
