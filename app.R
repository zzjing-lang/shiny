library(shiny)
library(tidyverse)
library(DT)
library(stringr)

prizes <- readr::read_csv("cleaned_prizes.csv")

ui <- navbarPage(
  "Prize Explorer",
  
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               selectInput("genre", "Select Prize Genre:",
                           choices = c("All", sort(unique(prizes$prize_genre)))),
               
               sliderInput("year_range", "Select Prize Year Range:",
                           min = min(prizes$prize_year, na.rm = TRUE),
                           max = max(prizes$prize_year, na.rm = TRUE),
                           value = c(min(prizes$prize_year, na.rm = TRUE),
                                     max(prizes$prize_year, na.rm = TRUE)))
             ),
             
             mainPanel(
               h3("Prizes Over Time"),
               plotOutput("yearPlot"),
               
               h3("Prize Genre Distribution"),
               plotOutput("genrePlot")
             )
           )),
  
  tabPanel("Institutions",
           sidebarLayout(
             sidebarPanel(
               textInput("inst_search", "Search Institution:", ""),
               
               selectInput("institution", "Select Institution:",
                           choices = c("All", sort(unique(prizes$degree_institution))))
             ),
             
             mainPanel(
               h3("Prize Genres for Selected Institution"),
               plotOutput("instPlot"),
               
               h3("Institution Prize Records"),
               DTOutput("instTable")
             )
           )),
  
  tabPanel("Raw Data",
           h3("Full Prize Dataset"),
           DTOutput("rawTable"))
)

server <- function(input, output) {
  
  filtered_overview <- reactive({
    data <- prizes
    
    if (input$genre != "All") {
      data <- data |> filter(prize_genre == input$genre)
    }
    
    data |> filter(
      prize_year >= input$year_range[1],
      prize_year <= input$year_range[2]
    )
  })
  
  output$yearPlot <- renderPlot({
    filtered_overview() |>
      count(prize_year) |>
      ggplot(aes(prize_year, n)) +
      geom_line(linewidth = 1.2, color = "#0072B2") +
      geom_point(size = 2, color = "#0072B2") +
      labs(
        x = "Prize Year",
        y = "Number of Prizes",
        title = "Prizes Awarded by Year"
      ) +
      theme_minimal()
  })
  
  output$genrePlot <- renderPlot({
    filtered_overview() |>
      count(prize_genre) |>
      ggplot(aes(reorder(prize_genre, n), n)) +
      geom_col(fill = "#D55E00") +
      coord_flip() +
      labs(
        x = "Prize Genre",
        y = "Count",
        title = "Distribution of Prize Genres"
      ) +
      theme_minimal()
  })
  
  filtered_inst <- reactive({
    data <- prizes
    
    if (input$institution != "All") {
      data <- data |> filter(degree_institution == input$institution)
    }
    
    if (input$inst_search != "") {
      data <- data |> filter(
        str_detect(degree_institution, regex(input$inst_search, ignore_case = TRUE))
      )
    }
    
    data
  })
  
  output$instPlot <- renderPlot({
    filtered_inst() |>
      count(prize_genre) |>
      ggplot(aes(reorder(prize_genre, n), n)) +
      geom_col(fill = "#009E73") +
      coord_flip() +
      labs(
        x = "Prize Genre",
        y = "Count",
        title = "Prize Genres for Selected Institution"
      ) +
      theme_minimal()
  })
  
  output$instTable <- renderDT({
    filtered_inst()
  })
  
  output$rawTable <- renderDT({
    prizes
  })
}

shinyApp(ui, server)
