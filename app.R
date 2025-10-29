# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DDA Shiny App: DDA Skewness Visualization
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 1. Load the App ----
library(shiny)
library(DT)
library(ggplot2)
library(gridExtra)
library(dda)
library(moments)
library(shinyjs)

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
        h2("Select Covariates"),
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
        br(),
        h2("DDA - Variables"),
        numericInput(
          inputId = "bootstrap_number_v",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "boot_type_v",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "confidence_interval_v",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        h2("DDA - Residuals"),
        numericInput(
          inputId = "bootstrap_number_r",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "boot_type_r",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "confidence_interval_r",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        selectInput(
          inputId = "prob_trans",
          label = "Probability Integral Transformation",
          choices = c("TRUE", "FASLE"),
          selected = "TRUE"
        ),
        br(),
        h2("DDA - Independence"),
        numericInput(
          inputId = "nl_fun",
          label = "Values Used for Power Transformation",
          value = 2
        ),
        selectInput(
          inputId = "hetero_i",
          label = "Separate Homoscedasticity Tests",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        selectInput(
          inputId = "hsic_method",
          label = "HSCI Inference Method",
          choices = c("gamma", "eigenvalue", "boot", "permutation"),
          selected = "gamma"
        ),
        useShinyjs(),
        selectInput(
          inputId = "diff_test",
          label = "Differences Tests",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        selectInput(
          inputId = "parallelize_i",
          label = "Multiple Cores",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        numericInput(
          inputId = "cores_number",
          label = "Number of Cores",
          value = 1
        ),
        numericInput(
          inputId = "bootstrap_number_i",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "boot_type_i",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "confidence_interval_i",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        )
      ),

      # Main Panel
      mainPanel(tabsetPanel(
        tabPanel(title = "Variables", verbatimTextOutput("DDA_var")),
        tabPanel(title = "Residuals", verbatimTextOutput("DDA_res")),
        tabPanel(title = "Independence", verbatimTextOutput("DDA_ind"))
      ))
    )
  ),

  # Fouth tab - CDDA
  tabPanel(
    # Title
    title = "CDDA Summary",
    # Side Bar
    sidebarLayout(
      sidebarPanel(
        h2("Select Moderator"),
        selectInput(
          inputId = "mod_cdda",
          label = "",
          choices = NULL,
          multiple = FALSE
        ),
        h2("Select Covariates"),
        selectInput(
          inputId = "covar_n_cdda",
          label = "Select Numerical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          inputId = "covar_c_cdda",
          label = "Select Categorical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        br(),
        h2("CDDA - Variables"),
        textInput(
          inputId = "modval_cdda",
          label = "Methods to Specify the Moderator Value",
          value = "",
          placeholder = NULL
        ),
        numericInput(
          inputId = "bootstrap_number_v_cdda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "boot_type_v_cdda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "bca"
        ),
        numericInput(
          inputId = "confidence_interval_v_cdda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        h2("CDDA - Independence"),
        numericInput(
          inputId = "nl_fun_cdda",
          label = "Values Used for Power Transformation",
          value = 2
        ),
        selectInput(
          inputId = "hetero_i_cdda",
          label = "Separate Homoscedasticity Tests",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        selectInput(
          inputId = "hsic_method_cdda",
          label = "HSCI Inference Method",
          choices = c("gamma", "eigenvalue", "boot", "permutation"),
          selected = "gamma"
        ),
        useShinyjs(),
        selectInput(
          inputId = "diff_test_cdda",
          label = "Differences Tests",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        selectInput(
          inputId = "parallelize_i_cdda",
          label = "Multiple Cores",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        numericInput(
          inputId = "cores_number_cdda",
          label = "Number of Cores",
          value = 1
        ),
        numericInput(
          inputId = "bootstrap_number_i_cdda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "boot_type_i_cdda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "confidence_interval_i_cdda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        )
      ),

      # Main Panel
      mainPanel(tabsetPanel(
        tabPanel(title = "Variables", verbatimTextOutput("CDDA_var")),
        tabPanel(title = "Independence", verbatimTextOutput("CDDA_ind"))
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

    updateSelectInput(
      session,
      inputId = "mod_cdda",
      choices = names(data()),
      selected = ""
    )

    updateSelectInput(
      session,
      inputId = "covar_n_cdda",
      choices = names(data()),
      selected = ""
    )

    updateSelectInput(
      session,
      inputId = "covar_c_cdda",
      choices = names(data()),
      selected = ""
    )
  })

  observeEvent(input$diff_test, {
    if (input$diff_test == "TRUE") {
      shinyjs::enable("parallelize_i") # make selectable
    } else {
      shinyjs::disable("parallelize_i") # greyed out, unclickable
      updateSelectInput(session, "parallelize_i", selected = "FALSE")

      shinyjs::disable("cores_number")
      updateNumericInput(session, "cores_number", value = 1)
    }
  })

  observeEvent(input$parallelize_i, {
    if (input$parallelize_i == "TRUE") {
      shinyjs::enable("cores_number")
    } else {
      shinyjs::disable("cores_number")
      updateNumericInput(session, "cores_number", value = 1)
    }
  })


  # Output: summary tables
  output$descTable_mainvars <- renderDataTable({
    req(data(), input$cause, input$effect)
    selected_data <- data()[, c(input$cause, input$effect), drop = FALSE]
    desc <- psych::describe(selected_data)
    desc_out <- as.data.frame(desc)
    desc_out <- desc_out[, c("n", "mean", "sd", "min", "max", "skew", "kurtosis")]
    num_cols <- c("mean", "sd", "min", "max", "skew", "kurtosis")
    desc_out[num_cols] <- lapply(desc_out[num_cols], function(x) {
      round(x, 2)
    })
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
        aes(y = after_stat(density)),
        bins = 30,
        fill = "blue",
        color = "black"
      ) +
      geom_density(color = "red", linewidth = 1.2) +
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
        aes(y = after_stat(density)),
        bins = 30,
        fill = "red",
        color = "black"
      ) +
      geom_density(color = "blue", linewidth = 1.2) +
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

  # --- DDA ---
  # Variables
  rundda_var <- reactive({
    req(data(), input$cause, input$effect)

    formula_str <- paste(input$effect, "~", input$cause)
    formula <- as.formula(formula_str)

    out.var <- dda.vardist(
      formula,
      pred = input$cause,
      B = input$bootstrap_number_v,
      boot.type = input$boot_type_v,
      conf.level = input$confidence_interval_v,
      data = data()
    )
    return(out.var)
  })

  output$DDA_var <- renderPrint({
    req(rundda_var())
    print(rundda_var())
  })

  # Residuals
  rundda_res <- reactive({
    req(data(), input$cause, input$effect)

    # # Construct formula
    # # ## Combine numeric covariates if not NULL
    # covar_num <- if (!is.null(input$covar_n) &&
    #   length(input$covar_n) > 0) {
    #   paste(input$covar_n, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine categorical covariates if not NULL
    # covar_cat <- if (!is.null(input$covar_c) &&
    #   length(input$covar_c) > 0) {
    #   paste(input$covar_c, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine both covariates
    # covars <- paste(c(covar_num, covar_cat), collapse = " + ")

    # covars <- if (covars == "") {
    #   NULL
    # } else {
    #   covars
    # }

    # ## Construct the formula string
    # formula_str <- if (is.null(covars)) {
    #   paste(input$effect, "~", input$cause)
    # } else {
    #   paste(input$effect, "~", input$cause, "+", covars)
    # }

    formula_str <- paste(input$effect, "~", input$cause)
    formula <- as.formula(formula_str)

    out.res <- dda.resdist(
      formula,
      pred = input$cause,
      B = input$bootstrap_number_r,
      boot.type = input$boot_type_r,
      conf.level = input$confidence_interval_r,
      prob.trans = paste0('"', input$prob_trans, "'"),
      data = data()
    )

    return(out.res)
  })

  output$DDA_res <- renderPrint({
    req(rundda_res())
    print(rundda_res())
  })

  rundda_ind <- reactive({
    req(data(), input$cause, input$effect)

    # # Construct formula
    # # ## Combine numeric covariates if not NULL
    # covar_num <- if (!is.null(input$covar_n) &&
    #   length(input$covar_n) > 0) {
    #   paste(input$covar_n, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine categorical covariates if not NULL
    # covar_cat <- if (!is.null(input$covar_c) &&
    #   length(input$covar_c) > 0) {
    #   paste(input$covar_c, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine both covariates
    # covars <- paste(c(covar_num, covar_cat), collapse = " + ")

    # covars <- if (covars == "") {
    #   NULL
    # } else {
    #   covars
    # }

    # ## Construct the formula string
    # formula_str <- if (is.null(covars)) {
    #   paste(input$effect, "~", input$cause)
    # } else {
    #   paste(input$effect, "~", input$cause, "+", covars)
    # }

    formula_str <- paste(input$effect, "~", input$cause)
    formula <- as.formula(formula_str)

    out.ind <- dda.indep(
      formula,
      pred = input$cause,
      B = input$bootstrap_number_i,
      boot.type = input$boot_type_i,
      conf.level = input$confidence_interval_i,
      hsic.method = input$hsic_method,
      nlfun = input$nl_fun,
      hetero = input$hetero_i,
      diff = input$diff_test,
      parallelize = ifelse(input$diff_test == "TRUE", input$parallelize_i == "TRUE", "FALSE"),
      cores = ifelse(input$parallelize_i == "FALSE", 0, input$cores_number),
      data = data()
    )

    return(out.ind)
  })

  output$DDA_ind <- renderPrint({
    req(rundda_ind())
    print(rundda_ind())
  })

  # --- CDDA ---
  # Variables
  runcdda_var <- reactive({
    req(data(), input$cause, input$effect, input$mod_cdda)

    formula_str <- paste(input$effect, "~", paste(input$cause, "*", input$mod_cdda))
    formula <- as.formula(formula_str)

    out.cvar <- cdda.vardist(
      formula,
      pred = input$cause,
      mod = input$mod_cdda,
      modval = input$modval_cdda,
      B = input$bootstrap_number_v,
      boot.type = input$boot_type_v,
      conf.level = input$confidence_interval_v,
      data = data()
    )
    return(out.cvar)
  })

  output$CDDA_var <- renderPrint({
    req(runcdda_var())
    print(runcdda_var())
  })


  runcdda_ind <- reactive({
    req(data(), input$cause, input$effect)

    # # Construct formula
    # # ## Combine numeric covariates if not NULL
    # covar_num <- if (!is.null(input$covar_n) &&
    #   length(input$covar_n) > 0) {
    #   paste(input$covar_n, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine categorical covariates if not NULL
    # covar_cat <- if (!is.null(input$covar_c) &&
    #   length(input$covar_c) > 0) {
    #   paste(input$covar_c, collapse = " + ")
    # } else {
    #   NULL
    # }

    # # ## Combine both covariates
    # covars <- paste(c(covar_num, covar_cat), collapse = " + ")

    # covars <- if (covars == "") {
    #   NULL
    # } else {
    #   covars
    # }

    # ## Construct the formula string
    # formula_str <- if (is.null(covars)) {
    #   paste(input$effect, "~", input$cause)
    # } else {
    #   paste(input$effect, "~", input$cause, "+", covars)
    # }

    formula_str <- paste(input$effect, "~", input$cause)
    formula <- as.formula(formula_str)

    out.cind <- cdda.indep(
      formula,
      pred = input$cause,
      B = input$bootstrap_number_i,
      boot.type = input$boot_type_i,
      conf.level = input$confidence_interval_i,
      hsic.method = input$hsic_method,
      nlfun = input$nl_fun,
      hetero = input$hetero_i,
      diff = input$diff_test,
      parallelize = ifelse(input$diff_test == "TRUE", input$parallelize_i == "TRUE", "FALSE"),
      cores = ifelse(input$parallelize_i == "FALSE", 0, input$cores_number),
      data = data()
    )

    return(out.cind)
  })

  output$CDDA_ind <- renderPrint({
    req(runcdda_ind())
    print(runcdda_ind())
  })
}

# 4. --- A Call to The ShinyApp ----
shinyApp(ui = ui, server = server)


# 1.show more descriptive statistics
# 2.diagnosis of DDA
# 4. download button.
# 6. Add CDDA PAGE. -- Working on it: Constructing the formula in cdda.
