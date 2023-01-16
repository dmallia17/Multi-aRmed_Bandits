source("bandits.R")
library(shiny)
library(ggplot2)

ui <- navbarPage("Multi-aRmed Bandits", 
  tabPanel("Welcome"),
  tabPanel("Single Run",
    sidebarLayout(
      sidebarPanel(
        # PROBLEM CONTROLS
        h1("Problem Controls:"),
        sliderInput("arms", "Number of arms", 1, 50, 10, step=1),
        numericInput("initmean", "Initial reward distribution mean", 0),
        numericInput("initsd", "Initial reward distribution standard dev.", 1),
        # radioButtons defaults to first choice
        radioButtons("class", "Problem Type",
                     choices=c("stationary", "nonstationary")),
        # Additional controls if non-stationary
        uiOutput("nonstationary_options"),
        actionButton("new_problem", "New Problem"),

        # ALGORITHM CONTROLS
        h1("Algorithm Controls:"),
        numericInput("steps", "Number of steps to run", 1000),
        radioButtons("update_weight", "Update weighting",
                     choices=c("Sample Average", "Constant")),
        uiOutput("update_options"),
        radioButtons("action_method", "Action selection",
                     choices=c("e-greedy", "Upper-Confidence-Bound")),
        uiOutput("explore_choice"),
        numericInput("init_q_val", "Initial q-value", 0),
        actionButton("run", "Run")
      ),
      mainPanel(
        plotOutput("mab_plot"),
        plotOutput("single_res_plot")
      )
    )),
  tabPanel("Batch Run")
)

server <- function(input, output) {
  vals <- reactiveValues(mab = new_multi_armed_bandit(), singleres=NULL)
  
  # A new bandit problem should only be created upon button press
  observeEvent(input$new_problem,
    vals$mab <- new_multi_armed_bandit(arms=input$arms, initmean=input$initmean,
      initsd=input$initsd, nsmean=input$nsmean, nssd=input$nssd,
      class=input$class))
  
  # Display violin plot of the current problem
  output$mab_plot <- renderPlot({plot(vals$mab)})
  
  # Add additional controls if the selected problem type is non-stationary
  output$nonstationary_options <- renderUI({
    req(input$class == "nonstationary")
    list(
      numericInput("nsmean", "Non-stationary shift distribution mean", 0),
      numericInput("nssd", "Non-stationary shift distribution standard dev.",
                 0.01)
    )
  })

  # Add additional control if the update weight should be constant
  output$update_options <- renderUI({
    req(input$update_weight == "Constant")
    numericInput("weight", "Update weight", 0.5)
  })

  # Present exploration parameter differently depending on action selection
  # method
  output$explore_choice <- renderUI({
    if(input$action_method == "e-greedy") {
      numericInput("exp_param", "Epsilon", 0.05)
    } else {
      numericInput("exp_param", "C", 2)
    }
  })

  # Only run upon click of the run button
  observeEvent(input$run, {
    # Fetch the appropriate functions
    if (input$update_weight == "Sample Average") {
      weight_func <- sample_average_weight_func
    } else {
      weight_func <- get_constant_weight_func(input$weight)
    }

    if (input$action_method == "e-greedy") {
      action_func <- e_greedy_action_func
    } else {
      action_func <- ucb_action_func
    }

    # Run the bandit algorithm
    vals$single_res <- simple_bandit_algorithm(vals$mab, input$steps,
                  weight_func, action_func, input$exp_param,input$init_q_val)
    }
  )

  output$single_res_plot <- renderPlot({
    # Only plot once a result is available
    req(vals$single_res)
    ggplot(data.frame(steps=1:input$steps, rewards=vals$single_res$rewards,
      optimal=vals$single_res$optimal_avg_rewards)) +
      geom_line(aes(x=steps, y=rewards)) +
      geom_line(aes(x=steps, y=optimal), color="red", linetype="dashed", size=2) +
      labs(title="Run results",
           subtitle="Dashed line indicates optimal average reward")
  })
}

shinyApp(ui=ui,server=server)
