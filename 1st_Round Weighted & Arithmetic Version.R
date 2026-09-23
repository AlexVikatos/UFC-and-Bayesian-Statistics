library(shiny)

library(ggplot2)

ui <- fluidPage(
  titlePanel("MMA 1st Round Significant Strikes - Model Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Fighter 1 Historical 1st Rounds"),
      textInput("fighter1_name", "Fighter 1 Name:", value = "Gamrot"),
      textAreaInput("fighter1_strikes", "Significant Strikes (comma-separated):", 
                    value = "10, 26, 19, 12, 9, 9", height = "80px"),
      textAreaInput("fighter1_duration", "1st Round Durations in minutes (comma-separated):", 
                    value = "5, 5, 5, 5, 5, 5", height = "80px"),
      helpText("Enter actual duration of each 1st round. If fight ended early (KO/Sub), use actual time."),
      
      hr(),
      
      h3("Fighter 2 Historical 1st Rounds"),
      textInput("fighter2_name", "Fighter 2 Name:", value = "Oliveira"),
      textAreaInput("fighter2_strikes", "Significant Strikes (comma-separated):", 
                    value = "9, 11, 9, 26, 19, 30", height = "80px"),
      textAreaInput("fighter2_duration", "1st Round Durations in minutes (comma-separated):", 
                    value = "2.5, 5, 5, 4, 5, 3", height = "80px"),
      helpText("Enter actual duration of each 1st round. If fight ended early (KO/Sub), use actual time."),
      
      hr(),
      
      actionButton("calculate", "Calculate & Compare Models", class = "btn-primary btn-lg"),
      
      br(), br(),
      helpText("Model predicts strikes for a FULL 5-minute 1st round."),
      
      hr(),
      
      h4("Model Descriptions:"),
      helpText(strong("Exposure Model:"), "Weights rounds by total minutes. A 5-minute round contributes more than a 2.5-minute round."),
      helpText(strong("Per-Minute Model:"), "Calculates strikes/minute for each round, then averages. All rounds weighted equally.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Comparison",
                 br(),
                 h4("Key Differences"),
                 tableOutput("model_comparison"),
                 hr(),
                 h4("Posterior Parameters Comparison"),
                 fluidRow(
                   column(6,
                          h5("Exposure Model"),
                          verbatimTextOutput("exposure_posteriors")
                   ),
                   column(6,
                          h5("Per-Minute Model"),
                          verbatimTextOutput("permin_posteriors")
                   )
                 )
        ),
        
        tabPanel("Statistics Comparison",
                 br(),
                 h4(textOutput("f1_stats_header")),
                 fluidRow(
                   column(6,
                          h5("Exposure Model"),
                          tableOutput("f1_exposure_stats")
                   ),
                   column(6,
                          h5("Per-Minute Model"),
                          tableOutput("f1_permin_stats")
                   )
                 ),
                 hr(),
                 h4(textOutput("f2_stats_header")),
                 fluidRow(
                   column(6,
                          h5("Exposure Model"),
                          tableOutput("f2_exposure_stats")
                   ),
                   column(6,
                          h5("Per-Minute Model"),
                          tableOutput("f2_permin_stats")
                   )
                 )
        ),
        
        tabPanel("Market Probabilities",
                 br(),
                 h4(textOutput("f1_market_header")),
                 fluidRow(
                   column(6,
                          h5("Exposure Model"),
                          tableOutput("f1_exposure_markets")
                   ),
                   column(6,
                          h5("Per-Minute Model"),
                          tableOutput("f1_permin_markets")
                   )
                 ),
                 hr(),
                 h4(textOutput("f2_market_header")),
                 fluidRow(
                   column(6,
                          h5("Exposure Model"),
                          tableOutput("f2_exposure_markets")
                   ),
                   column(6,
                          h5("Per-Minute Model"),
                          tableOutput("f2_permin_markets")
                   )
                 )
        ),
        
        tabPanel("Head-to-Head",
                 br(),
                 fluidRow(
                   column(6,
                          h4("Exposure Model"),
                          tableOutput("h2h_exposure")
                   ),
                   column(6,
                          h4("Per-Minute Model"),
                          tableOutput("h2h_permin")
                   )
                 )
        ),
        
        tabPanel("Visual Comparison",
                 br(),
                 h4("Exposure Model"),
                 plotOutput("exposure_combined", height = "350px"),
                 hr(),
                 h4("Per-Minute Model"),
                 plotOutput("permin_combined", height = "350px")
        ),
        
        tabPanel("Individual Plots",
                 br(),
                 fluidRow(
                   column(6,
                          h4(textOutput("f1_plot_header")),
                          plotOutput("f1_exposure_plot", height = "250px"),
                          plotOutput("f1_permin_plot", height = "250px")
                   ),
                   column(6,
                          h4(textOutput("f2_plot_header")),
                          plotOutput("f2_exposure_plot", height = "250px"),
                          plotOutput("f2_permin_plot", height = "250px")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$calculate, {
    
    # Parse input data
    f1_strikes <- as.numeric(unlist(strsplit(input$fighter1_strikes, ",")))
    f1_duration <- as.numeric(unlist(strsplit(input$fighter1_duration, ",")))
    f2_strikes <- as.numeric(unlist(strsplit(input$fighter2_strikes, ",")))
    f2_duration <- as.numeric(unlist(strsplit(input$fighter2_duration, ",")))
    
    # Validate data
    if(length(f1_strikes) != length(f1_duration) || length(f2_strikes) != length(f2_duration)) {
      showModal(modalDialog(
        title = "Data Error",
        "Number of strikes and durations must match for each fighter!",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    # ====== EXPOSURE MODEL ======
    total_strikes_f1 <- sum(f1_strikes)
    total_strikes_f2 <- sum(f2_strikes)
    total_time_f1 <- sum(f1_duration)
    total_time_f2 <- sum(f2_duration)
    
    update_gamma_exposure <- function(prior_alpha, prior_beta, total_strikes, total_time) {
      posterior_alpha <- prior_alpha + total_strikes
      posterior_beta <- prior_beta + total_time
      return(list(alpha = posterior_alpha, beta = posterior_beta))
    }
    
    shape1 <- 1; rate1 <- 1
    shape2 <- 1; rate2 <- 1
    
    posterior_f1_exp <- update_gamma_exposure(shape1, rate1, total_strikes_f1, total_time_f1)
    posterior_f2_exp <- update_gamma_exposure(shape2, rate2, total_strikes_f2, total_time_f2)
    
    # ====== PER-MINUTE MODEL ======
    f1_str_pm <- sum(f1_strikes / f1_duration)
    f2_str_pm <- sum(f2_strikes / f2_duration)
    fights_f1 <- length(f1_strikes)
    fights_f2 <- length(f2_strikes)
    
    update_gamma_permin <- function(prior_alpha, prior_beta, total_strikes_pm, fights) {
      posterior_alpha <- prior_alpha + total_strikes_pm
      posterior_beta <- prior_beta + fights
      return(list(alpha = posterior_alpha, beta = posterior_beta))
    }
    
    posterior_f1_pm <- update_gamma_permin(shape1, rate1, f1_str_pm, fights_f1)
    posterior_f2_pm <- update_gamma_permin(shape2, rate2, f2_str_pm, fights_f2)
    
    # ====== SIMULATIONS ======
    t <- 5
    set.seed(123)
    
    # Exposure model
    lambda_f1_exp <- rgamma(10000, posterior_f1_exp$alpha, posterior_f1_exp$beta)
    lambda_f2_exp <- rgamma(10000, posterior_f2_exp$alpha, posterior_f2_exp$beta)
    strikes_f1_exp <- rpois(10000, lambda_f1_exp * t)
    strikes_f2_exp <- rpois(10000, lambda_f2_exp * t)
    
    # Per-minute model
    lambda_f1_pm <- rgamma(10000, posterior_f1_pm$alpha, posterior_f1_pm$beta)
    lambda_f2_pm <- rgamma(10000, posterior_f2_pm$alpha, posterior_f2_pm$beta)
    strikes_f1_pm <- rpois(10000, lambda_f1_pm * t)
    strikes_f2_pm <- rpois(10000, lambda_f2_pm * t)
    
    # Return all results
    list(
      # Exposure model
      f1_strikes_exp = strikes_f1_exp,
      f2_strikes_exp = strikes_f2_exp,
      lambda_f1_exp = lambda_f1_exp,
      lambda_f2_exp = lambda_f2_exp,
      posterior_f1_exp = posterior_f1_exp,
      posterior_f2_exp = posterior_f2_exp,
      
      # Per-minute model
      f1_strikes_pm = strikes_f1_pm,
      f2_strikes_pm = strikes_f2_pm,
      lambda_f1_pm = lambda_f1_pm,
      lambda_f2_pm = lambda_f2_pm,
      posterior_f1_pm = posterior_f1_pm,
      posterior_f2_pm = posterior_f2_pm,
      
      # Raw data
      total_time_f1 = total_time_f1,
      total_time_f2 = total_time_f2,
      total_strikes_f1 = total_strikes_f1,
      total_strikes_f2 = total_strikes_f2,
      f1_str_pm = f1_str_pm,
      f2_str_pm = f2_str_pm,
      fights_f1 = fights_f1,
      fights_f2 = fights_f2
    )
  })
  
  # Dynamic headers
  output$f1_stats_header <- renderText({ paste(input$fighter1_name, "Statistics") })
  output$f2_stats_header <- renderText({ paste(input$fighter2_name, "Statistics") })
  output$f1_market_header <- renderText({ paste(input$fighter1_name, "Over/Under Markets") })
  output$f2_market_header <- renderText({ paste(input$fighter2_name, "Over/Under Markets") })
  output$f1_plot_header <- renderText({ input$fighter1_name })
  output$f2_plot_header <- renderText({ input$fighter2_name })
  
  # Model Comparison Table
  output$model_comparison <- renderTable({
    req(results())
    r <- results()
    data.frame(
      Metric = c(
        "Weighting Method",
        paste(input$fighter1_name, "Mean Strikes"),
        paste(input$fighter2_name, "Mean Strikes"),
        paste(input$fighter1_name, "Strike Rate/min"),
        paste(input$fighter2_name, "Strike Rate/min"),
        "Difference in Means"
      ),
      `Exposure Model` = c(
        "By Total Minutes",
        round(mean(r$f1_strikes_exp), 2),
        round(mean(r$f2_strikes_exp), 2),
        round(mean(r$lambda_f1_exp), 3),
        round(mean(r$lambda_f2_exp), 3),
        round(mean(r$f2_strikes_exp) - mean(r$f1_strikes_exp), 2)
      ),
      `Per-Minute Model` = c(
        "By Number of Fights",
        round(mean(r$f1_strikes_pm), 2),
        round(mean(r$f2_strikes_pm), 2),
        round(mean(r$lambda_f1_pm), 3),
        round(mean(r$lambda_f2_pm), 3),
        round(mean(r$f2_strikes_pm) - mean(r$f1_strikes_pm), 2)
      )
    )
  })
  
  # Posterior parameters
  output$exposure_posteriors <- renderPrint({
    req(results())
    r <- results()
    cat(sprintf("%s: ?? = %.2f, ?? = %.2f\n", input$fighter1_name, 
                r$posterior_f1_exp$alpha, r$posterior_f1_exp$beta))
    cat(sprintf("%s: ?? = %.2f, ?? = %.2f\n", input$fighter2_name, 
                r$posterior_f2_exp$alpha, r$posterior_f2_exp$beta))
    cat("\n")
    cat(sprintf("Lambda %s: %.3f strikes/min\n", input$fighter1_name, mean(r$lambda_f1_exp)))
    cat(sprintf("Lambda %s: %.3f strikes/min\n", input$fighter2_name, mean(r$lambda_f2_exp)))
  })
  
  output$permin_posteriors <- renderPrint({
    req(results())
    r <- results()
    cat(sprintf("%s: ?? = %.2f, ?? = %.2f\n", input$fighter1_name, 
                r$posterior_f1_pm$alpha, r$posterior_f1_pm$beta))
    cat(sprintf("%s: ?? = %.2f, ?? = %.2f\n", input$fighter2_name, 
                r$posterior_f2_pm$alpha, r$posterior_f2_pm$beta))
    cat("\n")
    cat(sprintf("Lambda %s: %.3f strikes/min\n", input$fighter1_name, mean(r$lambda_f1_pm)))
    cat(sprintf("Lambda %s: %.3f strikes/min\n", input$fighter2_name, mean(r$lambda_f2_pm)))
  })
  
  # Statistics tables
  create_stats_table <- function(strikes, lambda) {
    data.frame(
      Metric = c("Mean", "Median", "SD", "95% CI Lower", "95% CI Upper"),
      Value = c(
        round(mean(strikes), 2),
        median(strikes),
        round(sd(strikes), 2),
        quantile(strikes, 0.025),
        quantile(strikes, 0.975)
      )
    )
  }
  
  output$f1_exposure_stats <- renderTable({ 
    req(results())
    create_stats_table(results()$f1_strikes_exp, results()$lambda_f1_exp) 
  })
  output$f1_permin_stats <- renderTable({ 
    req(results())
    create_stats_table(results()$f1_strikes_pm, results()$lambda_f1_pm) 
  })
  output$f2_exposure_stats <- renderTable({ 
    req(results())
    create_stats_table(results()$f2_strikes_exp, results()$lambda_f2_exp) 
  })
  output$f2_permin_stats <- renderTable({ 
    req(results())
    create_stats_table(results()$f2_strikes_pm, results()$lambda_f2_pm) 
  })
  
  # Market probabilities
  create_market_table <- function(strikes) {
    data.frame(
      Line = c("Over 15.5", "Over 20.5", "Over 25.5", "Over 30.5"),
      Probability = paste0(round(c(
        mean(strikes > 15.5),
        mean(strikes > 20.5),
        mean(strikes > 25.5),
        mean(strikes > 30.5)
      ) * 100, 1), "%")
    )
  }
  
  output$f1_exposure_markets <- renderTable({ 
    req(results())
    create_market_table(results()$f1_strikes_exp) 
  })
  output$f1_permin_markets <- renderTable({ 
    req(results())
    create_market_table(results()$f1_strikes_pm) 
  })
  output$f2_exposure_markets <- renderTable({ 
    req(results())
    create_market_table(results()$f2_strikes_exp) 
  })
  output$f2_permin_markets <- renderTable({ 
    req(results())
    create_market_table(results()$f2_strikes_pm) 
  })
  
  # Head-to-head
  create_h2h_table <- function(strikes_f1, strikes_f2, name1, name2) {
    winner_f1 <- mean(strikes_f1 > strikes_f2)
    winner_f2 <- mean(strikes_f2 > strikes_f1)
    draw <- mean(strikes_f1 == strikes_f2)
    
    data.frame(
      Outcome = c(paste(name1, "Lands More"), paste(name2, "Lands More"), "Draw"),
      Probability = paste0(round(c(winner_f1, winner_f2, draw) * 100, 2), "%")
    )
  }
  
  output$h2h_exposure <- renderTable({
    req(results())
    r <- results()
    create_h2h_table(r$f1_strikes_exp, r$f2_strikes_exp, input$fighter1_name, input$fighter2_name)
  })
  
  output$h2h_permin <- renderTable({
    req(results())
    r <- results()
    create_h2h_table(r$f1_strikes_pm, r$f2_strikes_pm, input$fighter1_name, input$fighter2_name)
  })
  
  # Combined plots
  output$exposure_combined <- renderPlot({
    req(results())
    r <- results()
    combined <- rbind(
      data.frame(strikes = r$f1_strikes_exp, fighter = input$fighter1_name),
      data.frame(strikes = r$f2_strikes_exp, fighter = input$fighter2_name)
    )
    ggplot(combined, aes(x = strikes, fill = fighter)) +
      geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("orange", "cadetblue")) +
      labs(title = "Exposure Model: Predicted Strikes (5-min Round)", 
           x = "Significant Strikes", y = "Frequency") +
      theme_minimal(base_size = 13)
  })
  
  output$permin_combined <- renderPlot({
    req(results())
    r <- results()
    combined <- rbind(
      data.frame(strikes = r$f1_strikes_pm, fighter = input$fighter1_name),
      data.frame(strikes = r$f2_strikes_pm, fighter = input$fighter2_name)
    )
    ggplot(combined, aes(x = strikes, fill = fighter)) +
      geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("orange", "cadetblue")) +
      labs(title = "Per-Minute Model: Predicted Strikes (5-min Round)", 
           x = "Significant Strikes", y = "Frequency") +
      theme_minimal(base_size = 13)
  })
  
  # Individual plots
  output$f1_exposure_plot <- renderPlot({
    req(results())
    ggplot(data.frame(strikes = results()$f1_strikes_exp), aes(x = strikes)) +
      geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
      labs(title = "Exposure Model", x = "Strikes", y = "Freq") +
      theme_minimal()
  })
  
  output$f1_permin_plot <- renderPlot({
    req(results())
    ggplot(data.frame(strikes = results()$f1_strikes_pm), aes(x = strikes)) +
      geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
      labs(title = "Per-Minute Model", x = "Strikes", y = "Freq") +
      theme_minimal()
  })
  
  output$f2_exposure_plot <- renderPlot({
    req(results())
    ggplot(data.frame(strikes = results()$f2_strikes_exp), aes(x = strikes)) +
      geom_histogram(binwidth = 1, fill = "cadetblue", color = "black", alpha = 0.7) +
      labs(title = "Exposure Model", x = "Strikes", y = "Freq") +
      theme_minimal()
  })
  
  output$f2_permin_plot <- renderPlot({
    req(results())
    ggplot(data.frame(strikes = results()$f2_strikes_pm), aes(x = strikes)) +
      geom_histogram(binwidth = 1, fill = "cadetblue", color = "black", alpha = 0.7) +
      labs(title = "Per-Minute Model", x = "Strikes", y = "Freq") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
