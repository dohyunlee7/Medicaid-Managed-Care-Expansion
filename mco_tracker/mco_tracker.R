
library(dplyr)
library(shiny)
library(pubtheme)
library(tidyverse)

#d <- readRDS("full_table_with_proportions.rds")
d <- readRDS("new_merged_panel_inflation_adj.rds")

t_primes <- readRDS("t_primes.rds")
jumps <- readRDS("jumps.rds")

d <- d %>%
  left_join(t_primes, by = "state")

d <- d %>%
  left_join(jumps, by = "state")

# Remove DC and Puerto Rico
d <- d %>%
  filter(!state %in% c("District of Columbia", "Puerto Rico")) #%>%
  #filter(!year %in% 2003:2007)

ui <- fluidPage(
  titlePanel("Duggan & Hayford Expansion"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_states", "Select States:",
                  choices = unique(d$state),
                  multiple = TRUE)
      
    ),
    
    mainPanel(
      plotOutput("med_spending_plot"),
      plotOutput("mc_enrollment_plot"),
      plotOutput("comp_mco_enrollment_plot"),
      plotOutput("mco_spending_plot"),
      plotOutput("avg_enrollment_plot"),
      plotOutput("avg_total_spending")
    )
  )
)



# Define server logic to draw the time-series
server <- function(input, output) {
  
  # Filter based on selected states
  dd <- reactive({
    d %>%
      filter(state %in% input$selected_states)
  })
  
  # Calculate average enrollment across all states
  avg_enrollment <- reactive({
    d %>%
      group_by(year) %>%
      summarize(avg_enrollment = mean(comprehensive_mco_enr, na.rm = TRUE))
  })
  
  avg_total_spending <- reactive({
    d %>%
      group_by(year) %>%
      summarize(avg_total_spending = mean(`total medicaid (mt + at)`, na.rm = TRUE))
  })
  
  # Render time-series
  output$med_spending_plot <- renderPlot({
    ggplot(dd(), aes(x = year, 
                     y = `total medicaid (mt + at)` / 1e9, 
                     color = state)) +
      geom_line(size = 1) +
      labs(title = "Total Medicaid Spending Over Time by State",
           subtitle = "1991 - 2023, Adjusted for 2023 Dollars",
           x = "Year",
           y = "Total Medicaid Spending in USD (Billions)") +
      scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +
      scale_y_continuous(labels = scales::comma) +
      theme_pub()
  })
  
  output$mc_enrollment_plot <- renderPlot({
    ggplot(dd(), aes(x = year, y = pct_in_managed_care, color = state)) +
      geom_line(size = 1) +
      labs(title = "Share of Managed Care by State",
           subtitle = "1999-2022",
           x = "Year",
           y = "Share of Enrollees") +
      scale_x_continuous(breaks = seq(1999, 2023, by = 2),
                         limits = c(1999, 2023)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                         limits = c(0, 1),
                         labels = scales::percent) +
      geom_vline(data = dd() %>% filter(year == treatment_year), 
                 aes(xintercept = treatment_year, color = state), 
                 linetype = "dashed", size = 1) +      
      theme_pub()
  })
  
  output$comp_mco_enrollment_plot <- renderPlot({
    ggplot(dd(), aes(x = year, y = crb_mc_enrollees / total_med_enr, color = state)) +
      geom_line(size = 1) +
      labs(title = "Share of Comprehensive Risk-Based Managed Care by State",
           subtitle = "We only have Comprehensive MCO enrollment from 1999-2021",
           x = "Year",
           y = "Share of Enrollees") +
      scale_x_continuous(breaks = seq(1999, 2022, by = 2),
                         limits = c(1999, 2022)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                         limits = c(0, 1),
                         labels = scales::percent) +
      geom_vline(data = dd() %>% filter(year == t_prime), 
                 aes(xintercept = t_prime, color = state), 
                 linetype = "dashed", size = 1) +      
      theme_pub()
  })

  output$mco_spending_plot <- renderPlot({
    ggplot(dd(), aes(x = year,
                     y = `m-medicaid - mco` / `total medicaid (mt + at)`,
                     color = state)) +
      geom_line(size = 1) +
      labs(title = "Proportion of MCO Spending Over Time by State",
           x = "Year",
           y = "Proportion of MCO Spending") +
      scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                         limits = c(0, 1),
                         labels = scales::percent) +
      theme_pub()
  })
  
  output$avg_enrollment_plot <- renderPlot({
    ggplot(avg_enrollment(), aes(x = year, y = avg_enrollment)) +
      geom_line(size = 1) +
      labs(title = "Average Comprehensive MCO Enrollment Across All States",
           subtitle = "Average enrollment over time",
           x = "Year",
           y = "Average Enrollment") +
      scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +
      scale_y_continuous(labels = scales::comma) +
      theme_pub()
  })
  
  output$avg_total_spending <- renderPlot({
    ggplot(avg_total_spending(), aes(x = year, y = avg_total_spending / 1e9)) +
      geom_line(size = 1) +
      labs(title = "Average Total Medicaid Spending Across All States",
           subtitle = "Average spending over time",
           x = "Year",
           y = "Average Total Spending in USD (Billions)") +
      scale_x_continuous(breaks = seq(1990, 2023, by = 2)) +
      scale_y_continuous(labels = scales::comma) +
      theme_pub()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)









