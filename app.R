library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2024-03-26')
public_picks <- tuesdata$'public-picks'
library(readr)
cbb24 <- read_csv("cbb24.csv")
library(tidyverse)
library(shiny)

## let's add a win percentage variable to cbb24
cbb24 <- cbb24 |> mutate(win_perc = W / G * 100)

## let's get all of the possible statistics choices we can look at from cbb24 (as a vector)
rs_stat_choices <- names(cbb24)[c(4:21, 23:24)]

## Let's convert the percentages to numeric in public picks
public_picks[, c("R64", "R32", "S16", "E8", "F4", "FINALS")] <- 
  lapply(public_picks[, c("R64", "R32", "S16", "E8", "F4", "FINALS")], 
         function(x) as.numeric(sub("%", "", x)))

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
        choices = c("Full Regular Season Data", "Public Picks Data")
      ),
      
      # For regular season data
      conditionalPanel(
        condition = "input.data == 'Full Regular Season Data'",
        # select if we want to look at all teams or just tournament teams
        radioButtons(
          inputId = "rs_tourney_or_no",
          label = "All NCAA teams or only NCAA tournament teams:",
          choices = c("All D1 NCAA Teams", "NCAA Tournament Teams")
        ),
        radioButtons(
          inputId = "rs_manual_select",
          label = "Would you like to manually select teams?",
          choices = c("Yes", "No")
        ),
        # if looking at all teams
        conditionalPanel(
          condition = "input.rs_manual_select == `No`",
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
          ),
          actionButton(inputId = "rs_nms_run_app", label = "Update Stats")
        ),
        # if looking at tournament teams
        conditionalPanel(
          condition = "input.rs_manual_select == `Yes`",
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
          ),
          actionButton(inputId = "rs_ms_run_app", label = "Update Stats")
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
          inputId = "public_picks_rds",
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
        ),
        actionButton(inputId = "pp_run_app", label = "Update Stats")
      ),
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.data == 'Full Regular Season Data'",
        plotOutput(outputId = "regular_season_plot"),
        dataTableOutput(outputId = "regular_season_table")
      ),
      conditionalPanel(
        condition = "input.data == 'Public Picks Data'",
        plotOutput(outputId = "public_picks_plot"),
        dataTableOutput(outputId = "public_picks_table")
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Regular season data
  observeEvent(input$rs_tourney_or_no, {
    if (input$rs_tourney_or_no == "All D1 NCAA Teams") {
      teams_choices <- all_teams
    } else if (input$rs_tourney_or_no == "NCAA Tournament Teams") {
      teams_choices <- tourney_teams
    }
    updateSelectizeInput(session, "ms_team_name_reg_season", 
                         choices = teams_choices)
  })
  
  observeEvent(input$ms_regular_season_stat, {
    updateSelectInput(session, "ms_regular_season_ordering", 
                      choices = input$ms_regular_season_stat)
  })
  
  observeEvent(input$nms_regular_season_stat, {
    updateSelectInput(session, "nms_regular_season_ordering", 
                      choices = input$nms_regular_season_stat)
  })
  
  rs_ms_react <- eventReactive(input$rs_ms_run_app, {
    if(input$rs_tourney_or_no == "All D1 NCAA Teams") {
      output_table <- cbb24 |> 
        select(TEAM, CONF, any_of(input$ms_regular_season_stat)) |>
        filter(TEAM %in% input$ms_team_name_reg_season) |>
        arrange(desc(.data[[input$ms_regular_season_ordering]])) |>
        slice(1:input$ms_top_teams)
    } else if (input$rs_tourney_or_no == "NCAA Tournament Teams") {
      output_table <- cbb24 |>
        select(TEAM, CONF, any_of(input$ms_regular_season_stat)) |>
        filter(TEAM %in% input$ms_team_name_reg_season) |>
        arrange(desc(.data[[input$ms_regular_season_ordering]])) |>
        slice(1:input$ms_top_teams)
    }
  })
  
  rs_nms_react <- eventReactive(input$rs_nms_run_app, {
    if(input$rs_tourney_or_no == "All D1 NCAA Teams") {
      output_table <- cbb24 |> 
        select(TEAM, CONF, any_of(input$nms_regular_season_stat)) |>
        arrange(desc(.data[[input$nms_regular_season_ordering]])) |>
        slice(1:input$nms_top_teams)
    } else {
      output_table <- cbb24 |>
        select(TEAM, CONF, any_of(input$nms_regular_season_stat)) |>
        filter(TEAM %in% tourney_teams) |>
        arrange(desc(.data[[input$nms_regular_season_ordering]])) |>
        slice(1:input$nms_top_teams)
    }
  })
  
  ## regular season output tables
  output$regular_season_table <- renderDataTable({
    if (input$rs_manual_select == "No") {
      rs_nms_react()
    } else {
      rs_ms_react()
    }
  })
  
  ## regular season output graphs
  output$regular_season_plot <- renderPlot({
    if (input$rs_manual_select == "No") {
      rs_nms_plot <- rs_nms_react() |>
        select(everything(), -CONF)
      
      pivot_longer(rs_nms_plot, cols = -TEAM, 
                   names_to = "Variable", 
                   values_to = "Value") |>
        ggplot(aes(x = Variable, y = Value, fill = TEAM)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Variable", y = "Value",
             title = "Performance Comparison of Top College Basketball Teams",
             subtitle = "Data from 2023-2024 NCAA Basketball Regular Season") +
        theme_minimal(base_size = 20) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d()
    } else {
      rs_ms_plot <- rs_ms_react() |>
        select(everything(), -CONF)
      
      pivot_longer(rs_ms_plot, cols = -TEAM, 
                   names_to = "Variable", 
                   values_to = "Value") |>
        ggplot(aes(x = Variable, y = Value, fill = TEAM)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Variable", y = "Value",
             title = "Performance Comparison of Top College Basketball Teams",
             subtitle = "Data from 2023-2024 NCAA Basketball Regular Season") +
        theme_minimal(base_size = 20) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis_d()
    }
  })
  
  # public picks data
  observeEvent(input$public_picks_rds, {
    updateSelectInput(session, "public_picks_ordered_by", 
                      choices = input$public_picks_rds)
  })
  
  public_picks_react <- eventReactive(input$pp_run_app, {
    if (input$public_picks_ordering == "Ascending") {
      output_table <- public_picks |>
        select(TEAM, any_of(input$public_picks_rds)) |>
        filter(TEAM %in% input$public_picks_teams) |> 
        arrange(.data[[input$public_picks_ordered_by]])
    } else if (input$public_picks_ordering == "Descending") {
      output_table <- public_picks |>
        select(TEAM, any_of(input$public_picks_rds)) |>
        filter(TEAM %in% input$public_picks_teams) |> 
        arrange(desc(.data[[input$public_picks_ordered_by]]))
    }
  })
  
  output$public_picks_plot <- renderPlot({
    public_picks_top_teams_long <- pivot_longer(public_picks_react(),, 
                                                cols = -TEAM, 
                                                names_to = "Round", 
                                                values_to = "Percentage")
    
    public_picks_top_teams_long$Round <- factor(
      public_picks_top_teams_long$Round, 
      levels = rounds_order)
    
    ggplot(public_picks_top_teams_long, 
           aes(x = Round, y = Percentage, fill = TEAM)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Round", y = "Percentage",
           title = "Public Picks for 2024 NCAA Tournament Teams") +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d()
  })
  
  output$public_picks_table <- renderDataTable({
    public_picks_react()
  })
  
}

shinyApp(ui, server)

