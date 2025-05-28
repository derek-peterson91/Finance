

library(shiny)
library(shinydashboard)
library(tools)
library(scales)
library(DT)
library(MCMCpack)
library(tidyverse)
library(readxl)
library(tidyquant)
library(dplyr)



# === UI ===
ui <- dashboardPage(
  dashboardHeader(title = 'Portfolio Construction'),
  dashboardSidebar(
    fileInput('file', 'Upload Ticker List (.csv or .xlsx)', accept = c('.csv', '.xlsx', '.xls')),
    dateRangeInput(
      inputId = 'daterange',
      label = 'Date Range',
      start = Sys.Date() - 365,
      end = Sys.Date()
    ),
    downloadButton('downloadData', 'Download Price Data')
  ),
  dashboardBody(
    fluidRow(
      box(
        width = 12,
        title = "Return Statistics",
        solidHeader = TRUE,
        collapsible = TRUE,
        dataTableOutput("return_stats")
      ),
      box(
        title = "Return Distribution",
        width = 12,
        plotOutput('return_histograms'),
        collapsible = TRUE
      ),
      box(
        title = 'Efficient Frontier of 1000 Random Portfolios',
        width = 12,
        plotOutput('efficient_frontier'),
        collapsible = TRUE
      ),
      box(
        title = "Best Portfolios",
        width = 12,
        dataTableOutput('best_portfolios'),
        collapsible = TRUE
      )
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # 1. Read uploaded ticker file
  tickers_df <- reactive({
    req(input$file)
    ext <- file_ext(input$file$name)
    df <- switch(
      ext,
      csv = read_csv(input$file$datapath, show_col_types = FALSE),
      xlsx = read_excel(input$file$datapath),
      xls = read_excel(input$file$datapath),
      stop("Unsupported file format")
    )
    validate(
      need("Ticker" %in% colnames(df), "'Ticker' column is required.")
    )
    df
  })
  
  # 2. Download prices
  
  base_returns <- reactive({
    req(tickers_df())
    tickers <- tickers_df()$Ticker
    tq_get(tickers,
           from = Sys.Date() - (365 * 10),
           to = Sys.Date(),
           get = "stock.prices")
  })
  
  
  returns <- reactive({
    req(base_returns())
    
    base_returns() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      select(symbol, date, adjusted) %>%
      group_by(symbol) %>%
      tq_transmute(
        select = adjusted,
        mutate_fun = periodReturn,
        period = "daily",
        col_rename = "daily_return"
      ) %>%
      pivot_wider(names_from = symbol, values_from = daily_return) 
    
  })
  
  return_stats <- reactive({
    req(tickers_df())
    returns() %>%
      select(-date) %>%
      summarise(across(
        everything(),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          skewness = ~ skewness(.x, na.rm = TRUE),
          kurtosis = ~ kurtosis(.x, na.rm = TRUE)
        )
      )) %>%
      rename_with( ~ str_replace_all(.x, '.ret', ''), everything()) %>%
      pivot_longer(
        cols = everything(),
        names_to = c('Ticker', 'stat'),
        names_sep = "_"
        
      ) %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      # also calculate annualized versions of mean and sd
      mutate(annualized_mean = ((1 + mean) ^ 252) - 1,
             annualized_sd = sd * sqrt(252))
      
  })
  
  ##### Create Efficient Frontier #####
  
  
  cov_matrix <- reactive({
    req(returns())
    returns() %>%
    select(-date) %>%
    select(all_of(everything())) %>%
    filter(if_all(everything(), ~ !is.na(.))) %>%
    cov()
  })
  
  num_tickers <- reactive({
    req(return_stats())
    nrow(return_stats())
  })
  
  # number of portfolios to create
  num_portfolios <- 1000
  
  # create a dataframe of random weights
  weights <- reactive({
    req(return_stats())
    
    # Dirichlet distribution with low alpha for higher dispersion
    alpha <- rep(0.75, num_tickers())  # Lower values = more dispersed
    matrix <- rdirichlet(num_portfolios, alpha)
    df <- as.data.frame(matrix)
    names(df) <- return_stats()$Ticker
    df
  })
  
 
  
  # function to calculate variance for each random portfolio
  portfolio_variance_fn <- function(w, cov_matrix) {
    as.numeric(t(w) %*% cov_matrix %*% w)
  }
  
  # use the function to calculate the variance of each random portfolio
  portfolio_variances <- reactive({
    req(weights(), cov_matrix())
    weights_matrix <- as.matrix(weights())
    
    apply(weights_matrix, 1, function(w) {
      as.numeric(t(w) %*% cov_matrix() %*% w)
    })
  })
  

  # convert the return dataframe to a vector
  return_vector <- reactive({
    req(return_stats())
    return_stats() %>%
      select(Ticker, annualized_mean) %>%
      deframe()
  })
  
  
  # calculate expected return, variance, and sd for the random weight portfolios
  mpt_stats <- reactive({
    req(weights(), return_vector(), portfolio_variances())
    
    weights() %>%
      mutate(
        expected_return = rowSums(across(everything(), ~ .x * return_vector()[cur_column()])),
        variance = portfolio_variances(),
        sd = sqrt(variance) * sqrt(252)
      ) %>%
      rename_with(~ paste0(., '_weight'), 1:ncol(weights())) %>%
      mutate(
        sharpe = expected_return / sd,
        optimal_port = ifelse(sharpe == max(sharpe), "highlight", "normal")
      ) %>%
      arrange(-desc(sharpe))
  })
  
  
  
  # 3. Show table
  output$return_stats <- renderDataTable({
    req(return_stats())
    
    datatable(
      return_stats(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      colnames = c(
        "ETF",
        "Average Daily Return",                 # mean
        "Standard Deviation of Daily Returns", # sd
        "Skewness",
        "Kurtosis",
        "Annualized Return",
        "Annualized Risk (SD)"
      )
    ) %>%
      formatPercentage(c("mean", "sd", "annualized_mean", "annualized_sd"), digits = 2) %>%    # format mean and sd as %
      formatRound(c("skewness", "kurtosis"), digits = 2)   # round skewness & kurtosis to 2 decimals
  })
  
  output$return_histograms <- renderPlot({
    req(returns())
    
    returns() %>%
      pivot_longer(cols = -date,
                   names_to = 'Ticker',
                   values_to = 'Return') %>%
      ggplot(aes(x = Return)) +
      geom_histogram(
        bins = 50,
        fill = '#5d2f8d',
        color = 'white',
        alpha = 0.80
      ) +
      facet_wrap( ~ Ticker, scales = 'free') +
      scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
      theme_minimal() +
      labs(x = 'Daily Return', y = 'Frequency') +
      theme(strip.text = element_text(face = 'bold'))
  })
  
  output$efficient_frontier <- renderPlot({
    req(mpt_stats(), return_stats())
    
    mpt_stats() %>%
      ggplot(aes(x = sd, y = expected_return, color = optimal_port)) +
      geom_point(size = 5, alpha = 0.60) +
      scale_color_manual(values = c("highlight" = 'red', "normal" = '#0295b6')) +
      scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      theme_bw() +
      labs(
        x = 'Standard Deviation of Daily Returns',
        y = 'Expected Return',
        title = paste0(nrow(return_stats()), '-Asset Efficient Frontier')
      ) +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'gray75'),
        plot.title = element_text(hjust = 0.5, color = 'gray35'),
        axis.title = element_text(color = 'gray35'),
        legend.position = "none"
      )
  })
  
  output$best_portfolios <- renderDataTable({
    req(mpt_stats())
    
    df <- mpt_stats() %>%
      select(-c(optimal_port, variance))
    
    # Rename last 3 columns only
    n <- ncol(df)
    colnames(df)[(n - 2):n] <- c("Expected Return", "Standard Deviation", "Sharpe Ratio")
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        order = list(list(n - 1, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatPercentage(columns = colnames(df)[1:(n - 1)], digits = 2) %>%
      formatRound(columns = 'Sharpe Ratio', digits = 2)
  })
  
  # Download template
  template_data <- data.frame(
    Ticker = c("VOO", "VEA", "IEMG", "TLT", "BND", "VNQ")
  )
  
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      "template.csv"
    },
    content = function(file) {
      write_csv(template_data, file)
    }
  )
}

# === RUN APP ===
shinyApp(ui = ui, server = server)