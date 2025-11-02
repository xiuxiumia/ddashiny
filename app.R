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
library(rmarkdown)

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
      mainPanel(tabsetPanel(
        tabPanel(title = "Summary Tables", dataTableOutput("descTable_mainvars")),
        tabPanel(title = "Visualiazation", plotOutput("hist"))
      ))
    )
  ),

  # Third tab - DDA
  tabPanel(
    # Title
    title = "DDA Summary",
    # For rmd
    useShinyjs(),
    # Side Bar
    sidebarLayout(
      sidebarPanel(
        h2("Select Covariates"),
        selectInput(
          inputId = "Covar_n_dda",
          label = "Select Numerical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          inputId = "Covar_c_dda",
          label = "Select Categorical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        br(),
        h2("DDA - Variables"),
        numericInput(
          inputId = "BootN_v_dda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "BootType_v_dda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "CI_v_dda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        h2("DDA - Residuals"),
        numericInput(
          inputId = "BootN_r_dda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "BootType_r_dda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "CI_r_dda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        selectInput(
          inputId = "ProbTrans_r_dda",
          label = "Probability Integral Transformation",
          choices = c("TRUE", "FASLE"),
          selected = "TRUE"
        ),
        br(),
        h2("DDA - Independence"),
        numericInput(
          inputId = "NlFun_i_dda",
          label = "Values Used for Power Transformation",
          value = 2
        ),
        selectInput(
          inputId = "Hetero_i_dda",
          label = "Separate Homoscedasticity Tests",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        selectInput(
          inputId = "HsicMethod_i_dda",
          label = "HSCI Inference Method",
          choices = c("gamma", "eigenvalue", "boot", "permutation"),
          selected = "gamma"
        ),
        selectInput(
          inputId = "DiffTest_i_dda",
          label = "Differences Tests",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        selectInput(
          inputId = "Paral_i_dda",
          label = "Multiple Cores",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        numericInput(
          inputId = "CoresN_i_dda",
          label = "Number of Cores",
          value = 1
        ),
        numericInput(
          inputId = "BootN_i_dda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "BootType_i_dda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "CI_i_dda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        downloadButton("downloadReport1", "Export Report", icon = shiny::icon("file-pdf"))
      ),

      # Main Panel
      mainPanel(tabsetPanel(
        tabPanel(title = "Variables", verbatimTextOutput("dda_var")),
        tabPanel(title = "Residuals", verbatimTextOutput("dda_res")),
        tabPanel(title = "Independence", verbatimTextOutput("dda_ind"))
      ))
    )
  ),

  # Fouth tab - CDDA
  tabPanel(
    # Title
    title = "CDDA Summary",
    # For rmd
    useShinyjs(),
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
        br(),
        h2("Select Covariates"),
        selectInput(
          inputId = "Covar_n_cdda",
          label = "Select Numerical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          inputId = "Covar_c_cdda",
          label = "Select Categorical Covariates",
          choices = NULL,
          multiple = TRUE
        ),
        br(),
        h2("CDDA - Variables"),
        textInput(
          inputId = "Modval_v_cdda",
          label = "Methods to Specify the Moderator Value",
          value = "mean",
          placeholder = NULL
        ),
        numericInput(
          inputId = "BootN_v_cdda",
          label = "Number of Bootstrap Samples",
          value = 100
        ),
        selectInput(
          inputId = "BootType_v_cdda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "CI_v_cdda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        h2("CDDA - Independence"),
        textInput(
          inputId = "Modval_i_cdda",
          label = "Methods to Specify the Moderator Value",
          value = "c(-1, 1)",
          placeholder = NULL
        ),
        selectInput(
          inputId = "Hetero_i_cdda",
          label = "Separate Homoscedasticity Tests",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        selectInput(
          inputId = "DiffTest_i_cdda",
          label = "Differences Tests",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        selectInput(
          inputId = "Paral_i_cdda",
          label = "Multiple Cores",
          choices = c("TRUE", "FALSE"),
          selected = "FALSE"
        ),
        numericInput(
          inputId = "CoresN_i_cdda",
          label = "Number of Cores",
          value = 1
        ),
        numericInput(
          inputId = "NlFun_i_cdda",
          label = "Values Used for Power Transformation",
          value = 2
        ),
        selectInput(
          inputId = "HsicMethod_i_cdda",
          label = "HSCI Inference Method",
          choices = c("gamma", "eigenvalue", "boot", "permutation"),
          selected = "gamma"
        ),
        numericInput(
          inputId = "BootN_i_cdda",
          label = "Number of Bootstrap Samples",
          value = 200
        ),
        selectInput(
          inputId = "BootType_i_cdda",
          label = "Bootstrap Type",
          choices = c("perc", "bca"),
          selected = "perc"
        ),
        numericInput(
          inputId = "CI_i_cdda",
          label = "Bootstrap Confidence Intervals",
          value = .95,
          min = 0,
          max = 1
        ),
        br(),
        downloadButton("downloadReport2", "Export Report", icon = shiny::icon("file-pdf"))
      ),

      # Main Panel
      mainPanel(tabsetPanel(
        tabPanel(title = "Variables", verbatimTextOutput("cdda_var")),
        tabPanel(title = "Independence", verbatimTextOutput("cdda_ind"))
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
      inputId = "Covar_n_dda",
      choices = names(data()),
      selected = ""
    )
    updateSelectInput(
      session,
      inputId = "Covar_c_dda",
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
      inputId = "Covar_n_cdda",
      choices = names(data()),
      selected = ""
    )

    updateSelectInput(
      session,
      inputId = "Covar_c_cdda",
      choices = names(data()),
      selected = ""
    )
  })

  observeEvent(list(input$HsicMethod_i_dda, input$DiffTest_i_dda, input$BootType_i_dda, input$CI_i_dda), {
    if (input$HsicMethod_i_dda %in% c("boot", "permutation") || input$DiffTest_i_dda == "TRUE") {
      shinyjs::show("BootN_i_dda") # make selectable
      updateNumericInput(session, "BootN_i_dda", value = 100)

      shinyjs::show("BootType_i_dda") # greyed out, unclickable
      updateSelectInput(session, "BootType_i_dda", selected = "perc")

      shinyjs::show("CI_i_dda") # greyed out, unclickable
      updateNumericInput(session, "CI_i_dda", value = "0.95")
    } else {
      shinyjs::hide("BootN_i_dda") # greyed out, unclickable
      updateNumericInput(session, "BootN_i_dda", value = NULL)

      shinyjs::hide("BootType_i_dda") # greyed out, unclickable
      updateSelectInput(session, "BootType_i_dda", selected = NULL)

      shinyjs::hide("CI_i_dda") # greyed out, unclickable
      updateNumericInput(session, "CI_i_dda", value = NULL)
    }
  })

  observeEvent(input$DiffTest_i_dda, {
    if (input$DiffTest_i_dda == "TRUE") {
      shinyjs::show("Paral_i_dda") # make selectable
    } else {
      shinyjs::hide("Paral_i_dda") # greyed out, unclickable
      updateSelectInput(session, "Paral_i_dda", selected = "FALSE")

      shinyjs::hide("CoresN_i_dda")
      updateNumericInput(session, "CoresN_i_dda", value = 1)
    }
  })

  observeEvent(input$Paral_i_dda, {
    if (input$Paral_i_dda == "TRUE") {
      shinyjs::show("CoresN_i_dda")
    } else {
      shinyjs::hide("CoresN_i_dda")
      updateNumericInput(session, "CoresN_i_dda", value = 1)
    }
  })

  observeEvent(list(input$HsicMethod_i_cdda, input$DiffTest_i_cdda, input$BootType_i_cdda, input$CI_i_cdda), {
    if (input$HsicMethod_i_cdda %in% c("boot", "permutation") || input$DiffTest_i_cdda == "TRUE") {
      shinyjs::show("BootN_i_cdda") # make selectable
      updateNumericInput(session, "BootN_i_cdda", value = 100)

      shinyjs::show("BootType_i_cdda") # greyed out, unclickable
      updateSelectInput(session, "BootType_i_cdda", selected = "perc")

      shinyjs::show("CI_i_cdda") # greyed out, unclickable
      updateNumericInput(session, "CI_i_cdda", value = "0.95")
    } else {
      shinyjs::hide("BootN_i_cdda") # greyed out, unclickable
      updateNumericInput(session, "BootN_i_cdda", value = NULL)

      shinyjs::hide("BootType_i_cdda") # greyed out, unclickable
      updateSelectInput(session, "BootType_i_cdda", selected = NULL)

      shinyjs::hide("CI_i_cdda") # greyed out, unclickable
      updateNumericInput(session, "CI_i_cdda", value = NULL)
    }
  })

  observeEvent(input$DiffTest_i_cdda, {
    if (input$DiffTest_i_cdda == "TRUE") {
      shinyjs::show("Paral_i_cdda") # make selectable
    } else {
      shinyjs::hide("Paral_i_cdda") # greyed out, unclickable
      updateSelectInput(session, "Paral_i_cdda", selected = "FALSE")

      shinyjs::hide("CoresN_i_cdda")
      updateNumericInput(session, "CoresN_i_cdda", value = 1)
    }
  })

  observeEvent(input$Paral_i_cdda, {
    if (input$Paral_i_cdda == "TRUE") {
      shinyjs::show("CoresN_i_cdda")
    } else {
      shinyjs::hide("CoresN_i_cdda")
      updateNumericInput(session, "CoresN_i_cdda", value = 1)
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

    out_var <- dda.vardist(
      formula,
      pred = input$cause,
      B = input$BootN_v_dda,
      boot.type = input$BootType_v_dda,
      conf.level = input$CI_v_dda,
      data = data()
    )
    return(out_var)
  })

  output$dda_var <- renderPrint({
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

    out_res <- dda.resdist(
      formula,
      pred = input$cause,
      B = input$BootN_r_dda,
      boot.type = input$BootType_r_dda,
      conf.level = input$CI_r_dda,
      prob.trans = paste0('"', input$ProbTrans_r_dda, "'"),
      data = data()
    )

    return(out_res)
  })

  output$dda_res <- renderPrint({
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

    # build argument list
    args <- list(
      formula = formula,
      pred = input$cause,
      hsic.method = input$HsicMethod_i_dda,
      nlfun = input$NlFun_i_dda,
      hetero = input$Hetero_i_dda,
      diff = input$DiffTest_i_dda,
      parallelize = ifelse(input$DiffTest_i_dda == "TRUE", input$Paral_i_dda == "TRUE", FALSE),
      cores = ifelse(input$Paral_i_dda == "FALSE", 0, input$CoresN_i_dda),
      data = data()
    )

    # Add optional arguments only if relevant
    if (input$HsicMethod_i_dda %in% c("boot", "permutation") || input$DiffTest_i_dda == "TRUE") {
      args$B <- input$BootN_i_dda
      args$boot.type <- input$BootType_i_dda
      args$conf.level <- input$CI_i_dda
    }

    # Run the function
    out_ind <- do.call(dda.indep, args)

    return(out_ind)
  })

  output$dda_ind <- renderPrint({
    req(rundda_ind())
    print(rundda_ind())
  })

  ## Download report
  output$downloadReport1 <- downloadHandler(
    filename = "DDA Report.pdf",
    content = function(file) {
      src <- normalizePath("DDA_Report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "DDA_Report.Rmd", overwrite = TRUE)

      out <- render("DDA_Report.Rmd",
        output_format = pdf_document(),
        params = list(
          rundda_var = rundda_var(),
          rundda_res = rundda_res(),
          rundda_ind = rundda_ind()
        ),
        envir = new.env(parent = globalenv())
      )

      file.rename(out, file)
    }
  )

  # --- CDDA ---
  # Variables
  runcdda_var <- reactive({
    req(data(), input$cause, input$effect, input$mod_cdda)

    pred <- input$cause
    mod <- input$mod_cdda

    formula_str <- paste(input$effect, "~", paste(input$cause, "*", input$mod_cdda))
    formula <- as.formula(formula_str)

    m <- lm(formula, data = data())

    out_var <- cdda.vardist(
      m,
      pred = pred,
      mod = mod,
      modval = input$Modval_v_cdda,
      B = input$BootN_v_cdda,
      boot.type = input$BootType_v_cdda,
      conf.level = input$CI_v_cdda,
      data = data()
    )
    return(out_var)
  })

  output$cdda_var <- renderPrint({
    req(runcdda_var())
    print(runcdda_var())
  })

  runcdda_ind <- reactive({
    req(data(), input$cause, input$effect, input$mod_cdda)

    pred <- input$cause
    mod <- input$mod_cdda

    formula_str <- paste(input$effect, "~", paste(input$cause, "*", input$mod_cdda))
    formula <- as.formula(formula_str)

    m <- lm(formula, data = data())

    # build argument list
    args <- list(
      formula = formula,
      pred = input$cause,
      hsic.method = input$HsicMethod_i_cdda,
      nlfun = input$NlFun_i_cdda,
      hetero = input$Hetero_i_cdda,
      diff = input$DiffTest_i_cdda,
      parallelize = ifelse(input$DiffTest_i_cdda == "TRUE", input$Paral_i_cdda == "TRUE", FALSE),
      cores = ifelse(input$Paral_i_cdda == "FALSE", 0, input$CoresN_i_cdda),
      data = data()
    )

    # Add optional arguments only if relevant
    if (input$HsicMethod_i_cdda %in% c("boot", "permutation") || input$DiffTest_i_cdda == "TRUE") {
      args$B <- input$BootN_i_cdda
      args$boot.type <- input$BootType_i_cdda
      args$conf.level <- input$CI_i_cdda
    }

    # Run the function
    out_ind <- do.call(dda.indep, args)
    return(out_ind)
  })

  output$cdda_ind <- renderPrint({
    req(runcdda_ind())
    print(runcdda_ind())
  })

  ## Download report
  output$downloadReport2 <- downloadHandler(
    filename = "CDDA Report.pdf",
    content = function(file) {
      src <- normalizePath("CDDA_Report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "CDDA_Report.Rmd", overwrite = TRUE)

      out <- render("CDDA_Report.Rmd",
        output_format = pdf_document(),
        params = list(
          runcdda_var = runcdda_var(),
          runcdda_ind = runcdda_ind()
        ),
        envir = new.env(parent = globalenv())
      )

      file.rename(out, file)
    }
  )
}

# 4. --- A Call to The ShinyApp ----
shinyApp(ui = ui, server = server)


# 1.show more descriptive statistics
# 2.diagnosis of DDA
# 3.Add plots of CDDA

# Problem 1
# When I use the bootstrap type as bca, the number of bootstrap samples has to be what number?
# now 1300 need to find the number, this happend in both dda and cdda.

# Problem 2
# When I select boot for HSIC Inference method
# Error: object 'critical_value' not found

# Problem 3
# Do i need to change variables to factors in both DDA and CDDA?

# Problem 5
# Modaval input? Do i need that?, what should i put, can the model run without it?

# nlfun
# A logical value indicating whether non-linear correlation tests should be returned when using summary, default is FALSE.
