# Title: Correspondence Analysis (CA) graphics
# Description: Perform a correspondence analysis on one ore
# more variables observed on various players.
# Author: Gaston Sanchez
# Date: Spring 2023


# ---------------------------------------------------------
# Packages
# ---------------------------------------------------------
library(shiny)
library(tidyverse)
library(FactoMineR)


# ---------------------------------------------------------
# Data
# ---------------------------------------------------------
raw_dat = read_csv(
  file = "WBB-Data.csv", 
  col_types = "cdcdddddd")

# remove NAs
tidy_dat = raw_dat %>%
  select(Date:Confidence) %>%
  drop_na()

# fixing awkward sleep values
tidy_dat$Sleep[which(tidy_dat$Sleep > 5)] = 5

# add year to values in column Date
years = c(rep(2022, 872), rep(2023, 121))
tidy_dat$aux_date = paste0(tidy_dat$Date, "/", years)

tidy_dat = tidy_dat %>% 
  mutate(Date = as.Date(aux_date, "%m/%d/%Y"))


# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Correspondence Analysis"),
  
  # Sidebar with input widgets
  sidebarLayout(
    sidebarPanel(
      # which variables to analyze
      checkboxGroupInput(
        inputId = "checked",
        label = "Check variables",
        choices = c("Soreness",
                    "Fatigue",
                    "Confidence",
                    "Mood",
                    "Sleep",
                    "Nutrition"),
        selected = "Soreness"),
      
      # do we trust player N?
      radioButtons(
        inputId = "player_n", 
        label = "remove player N?", 
        choices = c("yes", "no"), 
        selected = "yes")
      
    ), # closes sidebarPanel
    
    # CA graphic
    mainPanel(
      plotOutput(outputId = "graphic")
    )
    
  ) # closes sidebarLayout
) # closes fluidPage


# ---------------------------------------------------------
# Server
# ---------------------------------------------------------
server <- function(input, output) {

  # --------------------------
  # whether to remove player N
  # --------------------------
  dat = reactive({
    if (input$player_n == "yes") {
      tidy_dat = filter(tidy_dat, Player != "N")
    }
    tidy_dat
  })
  
  # ------------------------------------------------
  # Correspondence Analysis of one or more variables
  # ------------------------------------------------
  ca_output <- reactive({
    num_selected = length(input$checked)
    # Soreness by default when user checks no boxes
    if (num_selected == 0) {
      input$checked = "Soreness"
      num_selected = length(input$checked)
    }

    # assemble frequency tables as matrices, and 
    # storing them in a list
    list_matrices = vector("list", length = num_selected)
    for (j in seq_along(input$checked)) {
      var_name = input$checked[[j]]
      var_abbr = str_sub(var_name, start = 1, end = 4)
      var_values = pull(dat(), var_name)
      aux_var = paste0(var_abbr, "-", ceiling(var_values))

      ca_var_crosstable = table(dat()$Player, aux_var)
      list_matrices[[j]] = as.matrix(ca_var_crosstable)
    }
    names(list_matrices) = input$checked
    
    # apply CA
    ca_matrices = do.call("cbind", list_matrices)
    ca_output = CA(ca_matrices, graph = FALSE)
    ca_output
  })
  
  
  # --------------------------
  # CA graphic (ggplot object)
  # --------------------------
  output$graphic <- renderPlot({
    plot(ca_output())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
