source("bandits.R")
library(shiny)

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
        numericInput("init_q_vals", "Initial q-values", 0),
        actionButton("run", "Run")
      ),
      mainPanel(
        plotOutput("mab_plot")
      )
    )),
  tabPanel("Batch Run")
)

server <- function(input, output) {
  mab <- reactiveValues(mab = new_multi_armed_bandit())
  
  # A new bandit problem should only be created upon button press
  observeEvent(input$new_problem,
    mab$mab <- new_multi_armed_bandit(arms=input$arms, initmean=input$initmean,
      initsd=input$initsd, nsmean=input$nsmean, nssd=input$nssd,
      class=input$class))
  
  # Display violin plot of the current problem
  output$mab_plot <- renderPlot({plot(mab$mab)})
  
  # Add additional controls if the selected problem type is non-stationary
  output$nonstationary_options <- renderUI({
    req(input$class == "nonstationary")
    list(
      numericInput("nsmean", "Non-stationary shift distribution mean", 0),
      numericInput("nssd", "Non-stationary shift distribution standard dev.",
                 0.01)
    )
  })

  output$update_options <- renderUI({
    req(input$update_weight == "Constant")
    numericInput("weight", "Update weight", 0.5)
  })

  output$explore_choice <- renderUI({
    if(input$action_method == "e-greedy") {
      numericInput("exp_param", "Epsilon", 0.05)
    } else {
      numericInput("exp_param", "C", 2)
    }
  })

}

shinyApp(ui=ui,server=server)
