source("bandits.R")
library(shiny)
library(ggplot2)

ui <- navbarPage("Multi-aRmed Bandits", 
  tabPanel("Welcome"),
  tabPanel("Single Run",
    sidebarLayout(
      sidebarPanel(
        # SETUP
        numericInput("single_seed", "Seed for RNG", 1234L),
        actionButton("single_seed_submit", "Seed RNG"),

        # PROBLEM CONTROLS
        h2("Problem Controls:"),
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
        h2("Algorithm Controls:"),
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
    )
  ),
  tabPanel("Batch Run",
    sidebarLayout(
     sidebarPanel(
       # SETUP
       numericInput("batch_seed", "Seed for RNG", 1234L),
       actionButton("batch_seed_submit", "Seed RNG"),

       # BATCH CONTROLS
       h2("Batch Controls:"),
       sliderInput("batch_size", "Number of bandit problems", 100, 5000, 1000,
                   step=100),

       # PROBLEM CONTROLS
       h2("Problem Controls:"),
       sliderInput("batch_arms", "Number of arms", 1, 50, 10, step=1),
       numericInput("batch_initmean", "Initial reward distribution mean", 0),
       numericInput("batch_initsd", "Initial reward distribution standard dev.", 1),
       # radioButtons defaults to first choice
       radioButtons("batch_class", "Problem Type",
                    choices=c("stationary", "nonstationary")),
       # Additional controls if non-stationary
       uiOutput("batch_nonstationary_options"),

       # ALGORITHM CONTROLS
       h2("Algorithm Controls:"),
       numericInput("batch_steps", "Number of steps to run", 1000),
       radioButtons("batch_update_weight", "Update weighting",
                    choices=c("Sample Average", "Constant")),
       uiOutput("batch_update_options"),
       radioButtons("batch_action_method", "Action selection",
                    choices=c("e-greedy", "Upper-Confidence-Bound")),
       uiOutput("batch_explore_choice"),
       numericInput("batch_init_q_val", "Initial q-value", 0),
       actionButton("batch_run", "Run")
     ),
     mainPanel(
       plotOutput("batch_res_plot")
     )
    )
  )
)

server <- function(input, output) {
  ##############################################################################
  # SINGLE RUN CONTEXT
  ##############################################################################
  vals <- reactiveValues(mab = new_multi_armed_bandit(), single_res=NULL,
                         steps=NULL)

  observeEvent(input$single_seed_submit, set.seed(input$single_seed))
  
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
    vals$steps <- input$steps
    }
  )

  output$single_res_plot <- renderPlot({
    # Only plot once a result is available
    req(vals$single_res)
    ggplot(data.frame(steps=1:vals$steps, rewards=vals$single_res$rewards,
      optimal=vals$single_res$optimal_avg_rewards)) +
      geom_line(aes(x=steps, y=rewards)) +
      geom_line(aes(x=steps, y=optimal), color="red", linetype="dashed", size=2) +
      labs(title="Run results",
           subtitle="Dashed line indicates optimal average reward")
  })

  ##############################################################################
  # BATCH RUN CONTEXT
  ##############################################################################
  batch_vals <- reactiveValues(batch_res=NULL, steps=NULL)

  observeEvent(input$batch_seed_submit, set.seed(input$batch_seed))

  # Add additional controls if the selected problem type is non-stationary
  output$batch_nonstationary_options <- renderUI({
    req(input$batch_class == "nonstationary")
    list(
      numericInput("batch_nsmean", "Non-stationary shift distribution mean", 0),
      numericInput("batch_nssd",
                   "Non-stationary shift distribution standard dev.", 0.01)
    )
  })

  # Add additional control if the update weight should be constant
  output$batch_update_options <- renderUI({
    req(input$batch_update_weight == "Constant")
    numericInput("batch_weight", "Update weight", 0.5)
  })

  # Present exploration parameter differently depending on action selection
  # method
  output$batch_explore_choice <- renderUI({
    if(input$batch_action_method == "e-greedy") {
      numericInput("batch_exp_param", "Epsilon", 0.05)
    } else {
      numericInput("batch_exp_param", "C", 2)
    }
  })

  # Only run upon click of the run button
  observeEvent(input$batch_run, {
    # Fetch the appropriate functions
    if (input$batch_update_weight == "Sample Average") {
      weight_func <- sample_average_weight_func
    } else {
      weight_func <- get_constant_weight_func(input$batch_weight)
    }

    if (input$batch_action_method == "e-greedy") {
      action_func <- e_greedy_action_func
    } else {
      action_func <- ucb_action_func
    }

    # Create a batch of bandit problems
    batch <- replicate(input$batch_size,
        new_multi_armed_bandit(arms=input$batch_arms,
          initmean=input$batch_initmean, initsd=input$batch_initsd,
          nsmean=input$batch_nsmean, nssd=input$batch_nssd,
          class=input$batch_class), simplify=F)

    # Run the bandit algorithm
    batch_vals$batch_res <- lapply(batch, function(mab) {
      simple_bandit_algorithm(mab, input$batch_steps, weight_func, action_func,
                              input$batch_exp_param, input$batch_init_q_val)})
    batch_vals$steps <- input$batch_steps
    }
  )

  output$batch_res_plot <- renderPlot({
    # Only plot once a result is available
    req(batch_vals$batch_res)
    avg_rewards <- rowMeans(sapply(batch_vals$batch_res,
                                   function(x) {x$rewards}))
    avg_optimal <- rowMeans(sapply(batch_vals$batch_res,
                                   function(x) {x$optimal_avg_rewards}))

    ggplot(
      data.frame(steps=1:batch_vals$steps, rewards=avg_rewards,
                 optimal=avg_optimal)) +
      geom_line(aes(x=steps, y=avg_rewards)) +
      geom_line(aes(x=steps, y=avg_optimal), color="red", linetype="dashed",
                size=2) +
      labs(title="Batch run results",
           subtitle="Dashed line indicates optimal average reward")
  })
}

shinyApp(ui=ui,server=server)
