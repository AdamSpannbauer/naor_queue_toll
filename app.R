library(shiny)
library(bslib)
library(markdown)
library(ggplot2)
library(dplyr)
library(DT)

source("naor_model_helpers.R")

# Define UI
ui <- navbarPage(
  title = "Queueing with Naor",
  # Use the Flatly theme from Bootswatch with bslib
  theme = bs_theme(bootswatch = "flatly"),
  tabPanel(
    title = "Hands-on model",
    tags$head(
      tags$style(HTML("
      .form-group {
        margin-bottom: 2px !important;
      }
    "))
    ),
    titlePanel("Queueing Theory with Naor's Model*"),
    HTML("*as outlined in Naor, P. (1969). The regulation of queue size by levying tolls. Econometrica: journal of the Econometric Society, 15-24. <a href='https://doi.org/10.2307/1909200' target='_blank'>https://doi.org/10.2307/1909200</a>"),
    hr(),
    fluidRow(
      column(
        align = "center",
        offset = 2,
        width = 8,
        p(
          style = "margin-bottom: 0 !important",
          "The model simulates a single-server queue with Poisson arrivals and exponential service times. If each customer optimizes their own individual utilities, it won't result in a queue length that's best for all. By imposing a toll, we increase each customer's cost and their chance of balking (not joining the queue). We can use such a toll to influence customer decisions and decrease queue sizes to match a shorter more optimal length for the system."
        ),
      )
    ),
    hr(),

    # Sidebar layout
    sidebarLayout(
      sidebarPanel(
        h4("Is the service worth the wait?"),
        # Numeric input for R with a tooltip
        numericInput("R", label = "Reward (R)", value = 10, min = 0),
        helpText("R represents the reward a customer receives for being served"),
        br(), br(),

        # Numeric input for C with a tooltip
        numericInput("C", label = "Cost (C)", value = 5, min = 0),
        helpText("C is the customer's cost of waiting (per unit time); assumed to be the same for all customers - future work investigates heterogeneous cost; e.g. Knudsen (1972)"),
        br(), br(),

        # Numeric input for lambda with a tooltip
        numericInput("lambda", label = "Arrival Rate (lambda)", value = 2, min = 0),
        helpText("lambda is the arrival rate of customers (Poisson-distributed here); how quickly customers arrive"),
        br(), br(),

        # Numeric input for mu with a tooltip
        numericInput("mu", label = "Service Rate (mu)", value = 4, min = 0),
        helpText("mu is the service rate (exponential-distributed here); how quickly we process customers - note lambda and mu are central to queueing theory and rho represents the ratio of arrivals to service times."),
        hr(),
        p("The output tables show metrics for cases of"),
        tags$ul(
          tags$li(
            "No tolls imposed",
            tags$ul(
              tags$li("Self Optimized - If customers are selfishly optimize for themselves, this is how the queue is expected to behave"),
              tags$li("Overall Optimized - A queue operating at a maximum overall utility would behave like this")
            )
          ),
          tags$li(
            "With suggested toll",
            tags$ul(
              tags$li("Self Optimized - With suggested toll, this is how customer behavior would be affected (i.e. brought down to overall optimal queue length)")
            )
          )
        )
      ),

      # Main panel to show outputs
      mainPanel(
        fluidRow(
          column(
            width = 4,
            align = "center",
            plotOutput("poisson_plot")
          ),
          column(
            width = 4,
            align = "center",
            plotOutput("exponential_plot")
          ),
          column(
            width = 4,
            align = "center",
            plotOutput("queue_len_plot")
          ),
        ),
        hr(),
        fluidRow(
          column(
            width = 6,
            align = "left",
            h3("No tolls imposed:"),
            DTOutput("no_toll_policies")
          ),
          column(
            width = 6,
            align = "left",
            h3(textOutput("toll_title")),
            DTOutput("toll_policies")
          )
        )
      ) # mainPanel
    ) # sidebarLayout()
  ), # tabPanel - model tab
  tabPanel(
    title = "Slides",
    # App title
    titlePanel("Queueing Theory with Naor's Model*"),
    HTML("*as outlined in Naor, P. (1969). The regulation of queue size by levying tolls. Econometrica: journal of the Econometric Society, 15-24. <a href='https://doi.org/10.2307/1909200' target='_blank'>https://doi.org/10.2307/1909200</a>"),
    hr(),
    div(
      align = "center",
      style = "margin: 40px;",
      tags$iframe(
        src = "paper-pres-slides.pdf",
        width = "80%",
        height = "700px",
        style = "border: none;"
      )
    )
  ), # tabPanel - slides tab
  tabPanel(
    title = "Notes",
    # App title
    titlePanel("Queueing Theory with Naor's Model*"),
    HTML("*as outlined in Naor, P. (1969). The regulation of queue size by levying tolls. Econometrica: journal of the Econometric Society, 15-24. <a href='https://doi.org/10.2307/1909200' target='_blank'>https://doi.org/10.2307/1909200</a>"),
    hr(),
    wellPanel(
      style = "margin: 40px;",
      withMathJax(),
      includeMarkdown("regulation_of_queue_size_notes.md")
    )
  ) # tabPanel - notes tab
) # fluidPage()

# Define server logic
server <- function(input, output, session) {
  # Poisson plot
  output$poisson_plot <- renderPlot({
    plot_poisson(input$lambda)
  })

  # Exponential plot
  output$exponential_plot <- renderPlot({
    plot_exponential(input$mu)
  })

  output$queue_len_plot <- renderPlot({
    plot_queue_len_dist(
      modeled_queues()$self_opt$queue_len_policy,
      rho = input$lambda / input$mu
    )
  })

  modeled_queues <- reactive({
    lambda <- input$lambda
    mu <- input$mu
    R <- input$R
    C <- input$C

    modeled_queue <- model_queue_naor(lambda, mu, R, C)

    toll <- modeled_queue$suggested_toll
    TOLLED_modeled_queue <- model_queue_naor(lambda, mu, R - toll, C)

    self_opt_queue <- set_queue_len_policy(modeled_queue, n = modeled_queue$self_optimized_n)
    overall_opt_queue <- set_queue_len_policy(modeled_queue, n = modeled_queue$overall_optimized_n)

    TOLLED_self_opt_queue <- set_queue_len_policy(TOLLED_modeled_queue, n = TOLLED_modeled_queue$self_optimized_n)
    TOLLED_overall_opt_queue <- set_queue_len_policy(TOLLED_modeled_queue, n = TOLLED_modeled_queue$overall_optimized_n)

    self_opt_queue$suggested_toll <- NA
    TOLLED_self_opt_queue$suggested_toll <- NA
    TOLLED_overall_opt_queue$suggested_toll <- NA

    list(
      self_opt = self_opt_queue,
      overall_opt = overall_opt_queue,
      tolled_self_opt = TOLLED_self_opt_queue,
      tolled_overall_opt = TOLLED_overall_opt_queue
    )
  })

  # Data table for policy comparison
  output$no_toll_policies <- renderDT({
    self_opt <- modeled_queues()$self_opt
    overall_opt <- modeled_queues()$overall_opt

    drop <- c("lambda", "mu", "R", "C", "len_pmf", "gain_i", "self_optimized_n", "overall_optimized_n")
    self_opt[drop] <- NULL
    overall_opt[drop] <- NULL

    policy_comparison <- as.data.frame(cbind(self_opt, overall_opt))

    rownames(policy_comparison) <- gsub(
      pattern = "_",
      replacement = " ",
      x = rownames(policy_comparison)
    )
    rownames(policy_comparison) <- tools::toTitleCase(rownames(policy_comparison))

    policy_comparison$self_opt <- sprintf("%.2f", policy_comparison$self)
    policy_comparison$overall_opt <- sprintf("%.2f", policy_comparison$overall)
    names(policy_comparison) <- c("Self Optimized", "Overall Optimized")

    datatable(
      policy_comparison,
      options = list(
        dom = "t",
        pageLength = 50,
        autoWidth = TRUE
      )
    )
  })

  output$toll_title <- renderText({
    sprintf("With suggested toll of %.2f:", modeled_queues()$overall_opt$suggested_toll)
  })

  output$toll_policies <- renderDT({
    self_opt <- modeled_queues()$tolled_self_opt
    overall_opt <- modeled_queues()$tolled_overall_opt

    drop <- c("lambda", "mu", "R", "C", "len_pmf", "gain_i", "self_optimized_n", "overall_optimized_n")
    self_opt[drop] <- NULL
    overall_opt[drop] <- NULL

    policy_comparison <- as.data.frame(cbind(self_opt, overall_opt))

    rownames(policy_comparison) <- gsub(
      pattern = "_",
      replacement = " ",
      x = rownames(policy_comparison)
    )
    rownames(policy_comparison) <- tools::toTitleCase(rownames(policy_comparison))

    policy_comparison$self_opt <- sprintf("%.2f", policy_comparison$self)
    policy_comparison$overall_opt <- sprintf("%.2f", policy_comparison$overall)
    names(policy_comparison) <- c("Self Optimized", "Overall Optimized")

    policy_comparison[["Overall Optimized"]] <- NULL

    datatable(
      policy_comparison,
      options = list(
        dom = "t",
        pageLength = 50,
        autoWidth = TRUE
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
