# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DDA Shiny App: DDA Skewness Visualization
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 1. Load the App ----
library(shiny)
library(DT)
library(ggplot2)
library(gridExtra)
library(dda)

# 2. --- User Interface (UI) ----
# Define UI for application
ui <- navbarPage(
  # Set up the title
  title = "Direction Dependence Analysis (DDA)",
  
  # First tab - Upload Data
  tabPanel(
    # Title
    title = "Upload Data",
    # Side Bar
    sidebarLayout(
      sidebarPanel(
        fileInput(
          inputId = "document",
          label = "Upload A CSV File",
          multiple = FALSE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
          buttonLabel = "Browse",
          placeholder = "No file selected"
        )
      ),
      # Main Panel
      mainPanel(
        conditionalPanel(
          condition = "output.showMain1 == true",
          h2("Preview of Uploaded Data"),
          DTOutput("tablePreview")
        )
      )
    )
  ),
  
  # Second tab - Descriptive Statistics
  tabPanel(
    # Title
    title = "Descriptive Statistics",
    # Side Bar
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "cause",
          label = "Select Cause",
          choices = NULL,
          multiple = FALSE
        ),
        selectInput(
          inputId = "effect",
          label = "Select Effect",
          choices = NULL,
          multiple = FALSE
        )
      ),
      # Main Panel
      mainPanel(
        h2("Summary Tables"),
        tabPanel(title = "Summary", dataTableOutput("descTable_mainvars")),
        h2("Visualizations"),
        tabPanel(title = "Visualiazation", plotOutput("hist"))
      )
    )
  ),
  
  # Third tab - DDA
  tabPanel(
    # Title
    title = "DDA Summary",
    # Side Bar
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "covar_n",
          label = "Select Numerical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          inputId = "covar_c",
          label = "Select Categorical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        numericInput(
          inputId = "bootstrap_number",
          label = "Number of bootstrap samples",
          value = 500
        ),
        selectInput(
          inputId = "boot_type",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "confidence_interval",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        actionButton(inputId = "runDDA", label = "Run DDA")
      ),
      # Main Panel
      mainPanel(tabsetPanel(
        tabPanel(title = "Variables", verbatimTextOutput("DDA_var")),
        tabPanel(title = "Residuals", verbatimTextOutput("DDA_res")),
        tabPanel(title = "Independence", verbatimTextOutput("DDA_ind"))
      ))
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # --- Reactive: read uploaded data ---
  data <- reactive({
    req(input$document)
    read.csv(input$document$datapath)
  })
  
  # Conditional output
  output$showMain1 <- reactive({
    !is.null(input$document)
  })
  outputOptions(output, "showMain1", suspendWhenHidden = FALSE)
  
  output$tablePreview <- renderDT({
    req(data())
    datatable(head(data(), 10))
  })
  
  
  # --- Select variables and show descriptive statistics and visualization ---
  observe({
    req(data())
    updateSelectInput(
      session,
      inputId = "cause",
      choices = names(data()),
      selected = ""
    )
    updateSelectInput(
      session,
      inputId = "effect",
      choices = names(data()),
      selected = ""
    )
    updateSelectInput(
      session,
      inputId = "covar_n",
      choices = names(data()),
      selected = ""
    )
    updateSelectInput(
      session,
      inputId = "covar_c",
      choices = names(data()),
      selected = ""
    )
  })
  
  # Output: summary tables
  output$descTable_mainvars <- renderDataTable({
    req(data(), input$cause, input$effect)
    selected_data <- data()[, c(input$cause, input$effect), drop = FALSE]
    desc <- psych::describe(selected_data)
    desc_out <- as.data.frame(desc)
    desc_out <- desc_out[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")]
    num_cols <- c("mean", "sd", "min", "max", "skew", "kurtosis")
    desc_out[num_cols] <- lapply(desc_out[num_cols], function(x)
      round(x, 2))
    desc_out <- cbind(Variable = rownames(desc_out), desc_out)
    rownames(desc_out) <- NULL
    desc_out
  })
  
  # Output: visualization
  output$hist <- renderPlot({
    req(data(), input$cause, input$effect)
    
    # First histogram
    p1 <- ggplot(data(), aes(x = .data[[input$cause]])) +
      geom_histogram(
        aes(y = ..density..),
        bins = 30,
        fill = "blue",
        color = "black"
      ) +
      geom_density(color = "red", size = 1.2) +
      labs(
        title = paste("Histogram of", input$cause),
        x = input$cause,
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 18,
          face = "bold"
        ),
        panel.grid = element_blank()
      )
    
    # Second histogram
    p2 <- ggplot(data(), aes(x = .data[[input$effect]])) +
      geom_histogram(
        aes(y = ..density..),
        bins = 30,
        fill = "red",
        color = "black"
      ) +
      geom_density(color = "blue", size = 1.2) +
      labs(
        title = paste("Histogram of", input$effect),
        x = input$effect,
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 18,
          face = "bold"
        ),
        panel.grid = element_blank()
      )
    
    # Display side by side
    grid.arrange(p1, p2, ncol = 2)
  })
  
  # --- Select variables and show descriptive statistics and visualization ---
  # Action button
  rundda <- eventReactive(input$runDDA, {
    req(data(), input$cause, input$effect)
    
    # # Construct formula
    # covariates <- paste(input$covariable, collapse = " + ")
    # formula_str <- ifelse(covariates == "",
    #                       paste(input$effect, "~", input$casue),
    #                       paste(input$effect, "~", input$casue, "+", covariates)
    # )
    
    formula_str <- paste(input$effect, "~", input$cause)
    
    formula <- as.formula(formula_str)
    
    
    # Fit DDA
    out.var <- dda.vardist(
      formula,
      pred = input$cause,
      B = input$bootstrap_number,
      boot.type = input$boot_type,
      conf.level = input$confidence_interval,
      data = data()
    )
    out.res <- dda.resdist(
      formula,
      pred = input$cause,
      B = input$bootstrap_number,
      boot.type = input$boot_type,
      conf.level = input$confidence_interval,
      #prob.trans = TRUE,
      data = data()
    )
    out.ind <- dda.indep(
      formula,
      pred = input$cause,
      B = input$bootstrap_number,
      boot.type = input$boot_type,
      conf.level = input$confidence_interval,
      #hsic.method = "gamma",
      #nlfun = 2,
      #diff = TRUE,
      data = data()
    )
    
    # Return summary of DDA
    return(list(var = out.var, res = out.res, ind = out.ind))
  })
  
  
  
  # # Construct formula
  # # ## Combine numeric covariates if not NULL
  # covar_num <- if (!is.null(input$covar_n) &&
  #                  length(input$covar_n) > 0) {
  #   paste(input$covar_n, collapse = " + ")
  # } else {
  #   NULL
  # }
  #
  # # ## Combine categorical covariates if not NULL
  # covar_cat <- if (!is.null(input$covar_c) &&
  #                  length(input$covar_c) > 0) {
  #   paste(input$covar_c, collapse = " + ")
  # } else {
  #   NULL
  # }
  #
  # # ## Combine both covariates
  # covars <- paste(c(covar_num, covar_cat), collapse = " + ")
  # # ## Remove trailing/leading "+" if covars is empty
  # covars <- if (covars == "")
  #   NULL
  # else
  #   covars
  #
  # # ##Construct the formula string
  # formula_str <- if (is.null(covars)) {
  #   paste(input$effect, "~", input$cause)
  # } else {
  #   paste(input$effect, "~", input$cause, "+", covars)
  # }
  #
  # formula_str <- paste(input$effect, "~", input$cause)
  # # ## final formula
  # formula <- as.formula(formula_str)
  #
  #
  #   # Return summary of DDA
  #   return(list(
  #     var = out.var,
  #     res = out.res,
  #     ind = out.ind
  #   ))
  #
  
  
  # Render DDA summary output
  # Variable Distributions
  output$DDA_var <- renderPrint({
    req(rundda()) # Wait for eventReactive to trigger
    result <- rundda()
    print(result$var)
  })
  
  # Residual Distributions
  output$DDA_res <- renderPrint({
    req(rundda()) # Wait for eventReactive to trigger
    result <- rundda()
    print(result$res)
  })
  
  # Independence Properties
  output$DDA_ind <- renderPrint({
    req(rundda()) # Wait for eventReactive to trigger
    result <- rundda()
    print(result$ind)
  })
}
  
# 4. --- A Call to The ShinyApp ----
shinyApp(ui = ui, server = server)


# 1.show more descriptive statistics
# 2.diagnosis of DDA
# 3. show different options for those DDA functions.
#4. download button. 
