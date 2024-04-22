library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2024-03-26')
team_results <- tuesdata$'team-results'
public_picks <- tuesdata$'public-picks'
library(readr)
cbb24 <- read_csv("cbb24.csv")
library(tidyverse)
library(shiny)

# let's add a win percentage variable to cbb24
cbb24 <- cbb24 |> mutate(win_perc = W / G * 100)

# let's get all of the possible statistics choices we can look at from cbb24 (as a vector)
rs_stat_choices <- names(cbb24)[c(4:21, 23:24)]


all_teams <- cbb24 |> mutate(TEAM = as.factor(TEAM)) |>
  pull(TEAM) |> levels()

tourney_teams <- public_picks |> mutate(TEAM = as.factor(TEAM)) |>
  pull(TEAM) |> levels()

public_picks_rounds <- names(public_picks)[c(4:9)]


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "data",
        label = "Choose a dataset to explore:",
        choices = c("Full Regular Season Data", "Public Picks Data", "Past Team Results Data")
      ),
      
      # For regular season data
      conditionalPanel(
        condition = "input.data == 'Full Regular Season Data'",
        # select if we want to look at all teams or just tournament teams
        radioButtons(
          inputId = "tourney_or_no",
          label = "All NCAA teams or only NCAA tournament teams:",
          choices = c("All D1 NCAA Teams", "NCAA Tournament Teams")
        ),
        radioButtons(
          inputId = "manual_select",
          label = "Would you like to manually select teams?",
          choices = c("Yes", "No")
        ),
        # if looking at all teams
        conditionalPanel(
          condition = "input.manual_select == `No`",
          # select statistics to look at
          selectizeInput(
            inputId = "nms_regular_season_stat",
            label = "Choose statistic(s) to look at:",
            choices = rs_stat_choices,
            selected = "win_perc",
            multiple = TRUE
          ),
          # select statistic to order by
          selectInput(
            inputId = "nms_regular_season_ordering",
            label = "Order teams by:",
            choices = NULL
          ),
          sliderInput(
            inputId = "nms_top_teams",
            label = "How many top teams would you like to look at?",
            min = 1,
            max = 50,
            value = 10
          )
        ),
        # if looking at tournament teams
        conditionalPanel(
          condition = "input.manual_select == `Yes`",
          # select teams to look at
          selectizeInput(
            inputId = "ms_team_name_reg_season",
            label = "Choose teams to look at:",
            choices = NULL,
            multiple = TRUE
          ),
          # select statistics to look at
          selectizeInput(
            inputId = "ms_regular_season_stat",
            label = "Choose statistic(s) to look at:",
            choices = rs_stat_choices,
            selected = "win_perc",
            multiple = TRUE
          ),
          # select statistic to order by
          selectInput(
            inputId = "ms_regular_season_ordering",
            label = "Order teams by:",
            choices = NULL
          ),
          # select how many top teams to look at
          sliderInput(
            inputId = "ms_top_teams",
            label = "How many top teams would you like to look at?",
            min = 1,
            max = 50,
            value = 10
          )
        )
      ),
      
      # For public picks data
      conditionalPanel(
        condition = "input.data == 'Public Picks Data'",
        # select if we want to look at individual teams or groups of teams
        selectizeInput(
          inputId = "public_picks_teams",
          label = "Choose teams to look at:",
          choices = tourney_teams,
          multiple = TRUE
        ),
        checkboxGroupInput(
          inputId = "public_picks_rounds",
          label = "Choose rounds to look at:",
          choices = public_picks_rounds
        ),
        selectInput(
          inputId = "public_picks_ordered_by",
          label = "What round would you like to order the teams by?",
          choices = NULL
        ),
        selectInput(
          inputId = "public_picks_ordering",
          label = "What order would you like to rank the teams in?",
          choices = c("Ascending", "Descending")
        )
      ),
      
      # For past team results data
      conditionalPanel(
        condition = "input.data == 'Past Team Results Data'",
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.data == 'Full Regular Season Data'",
        dataTableOutput(outputId = "regular_season_table"),
        plotOutput(outputId = "regular_season_plot")
      ),
      conditionalPanel(
        condition = "input.data == 'Public Picks Data'",
        dataTableOutput(outputId = "public_picks_table"),
        plotOutput(outputId = "public_picks_plot")
      ),
      conditionalPanel(
        condition = "input.data == 'Past Team Results Data'",
        dataTableOutput(outputId = "past_team_results_table"),
        plotOutput(outputId = "past_team_results_plot")
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$manual_select, {
    if (input$manual_select == "No") {
      updateSelectizeInput(session, "nms_regular_season_stat", choices = rs_stat_choices)
    } else if (input$manual_select == "Yes") {
      if (input$tourney_or_no == "All D1 NCAA Teams") {
        teams_choices <- all_teams
      } else if (input$tourney_or_no == "NCAA Tournament Teams") {
        teams_choices <- tourney_teams
      }
      updateSelectizeInput(session, "ms_team_name_reg_season", choices = teams_choices)
      updateSelectizeInput(session, "ms_regular_season_stat", choices = rs_stat_choices)
    }
  })
  
  observeEvent(input$ms_regular_season_stat, {
    updateSelectInput(session, "ms_regular_season_ordering", choices = input$ms_regular_season_stat)
  })
  
  observeEvent(input$nms_regular_season_stat, {
    updateSelectInput(session, "nms_regular_season_ordering", choices = input$nms_regular_season_stat)
  })
  
  regular_season_reactive <- reactive({
    if(input$tourney_or_no == "All D1 NCAA Teams") {
      if(input$manual_select == "No") {
        output_table <- cbb24 |> 
          select(TEAM, CONF, .data[[input$nms_regular_season_stat]]) |>
          arrange(desc(.data[[input$nms_regular_season_stat]])) |>
          slice(1:input$nms_top_teams)
      } else {
        output_table <- cbb24 |>
          select(TEAM, CONF, .data[[input$ms_regular_season_stat]]) |>
          filter(TEAM %in% input$ms_team_name_reg_season) |>
          arrange(desc(.data[[input$ms_regular_season_ordering]])) |>
          slice(1:input$ms_top_teams)
      }
    } else {
      if(input$manual_select == "No") {
        output_table <- cbb24 |>
          filter(TEAM %in% public_picks$TEAM) |>
          select(TEAM, CONF, .data[[input$nms_regular_season_stat]]) |>
          arrange(desc(.data[[input$nms_regular_season_ordering]])) |>
          slice(1:input$nms_top_teams)
      } else {
        output_table <- cbb24 |>
          filter(TEAM %in% public_picks$TEAM) |>
          select(TEAM, CONF, .data[[input$ms_regular_season_stat]]) |>
          filter(TEAM %in% input$ms_team_name_reg_season) |>
          arrange(desc(.data[[input$ms_regular_season_ordering]])) |>
          slice(1:input$ms_top_teams)
      }
    }
    output_table
  })
  
  observeEvent(input$public_picks_rounds, {
    updateSelectInput(session, "public_picks_ordered_by", choices = input$public_picks_rounds)
  })
  
  output$regular_season_table <- renderDataTable({
    regular_season_reactive()
  })
  
  
  
}

shinyApp(ui, server)

