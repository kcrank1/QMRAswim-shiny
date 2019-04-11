
pathogensUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('ui'))
}

pathogens <- function(input, output, session, n, sample_n, ww_dose) {
  ns <- session$ns

  # UI ----------------------------------------------------------------------

  output$ui <- renderUI({
    tagList(
      uiOutput(ns("ui_name")),
      uiOutput(ns("ui_resp_mod")),
      uiOutput(ns("ui_resp_mod_pars")),
      uiOutput(ns("ui_dist")),
      uiOutput(ns("ui_dist_pars")),
      uiOutput(ns("ui_sewage_dist")),
      uiOutput(ns("ui_sewage_dist_pars"))
    )
  })

  output$ui_name <- renderUI({
    textInput(
      inputId = ns("name"),
      label   = paste("Name of pathogen", n()),
      value   = paste("Pathogen", n())
    )
  })

  output$ui_resp_mod <- renderUI({
    selectInput(
      inputId = ns("resp_mod"),
      label   = paste("Dose response model for pathogen", n()),
      choices = c("single-parameter exponential", "two-parameter beta-poisson", "two-parameter hypergeometric1f1")
    )
  })

  output$ui_resp_mod_pars <- renderUI({
    validate(need(input$resp_mod, "Loading..."))
    if(input$resp_mod == "single-parameter exponential") {
      numericInput(
        inputId = ns("resp_mod_par_lambda"),
        label   = "Exponential parameter, lambda",
        value   = 0.4172,
        min     = 0,
        max     = 100
      )
    } else if(input$resp_mod %in% c("two-parameter beta-poisson", "two-parameter hypergeometric1f1")) {
      tagList(
        numericInput(
          inputId = ns("resp_mod_par_alpha"),
          label   = "Alpha",
          value   = 1,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = ns("resp_mod_par_beta"),
          label   = "Beta",
          value   = 1,
          min     = 0,
          max     = 100
        )
      )
    }

  })

  output$ui_dist <- renderUI({
    selectInput(
      inputId = ns("dist"),
      label   = paste("Pill/inf for pathogen", n()),
      choices = c("single value", "uniform")
    )
  })

  output$ui_dist_pars <- renderUI({
    validate(need(input$dist, "Loading..."))
    if(input$dist == "single value") {
      numericInput(
        inputId = ns("dist_par_single"),
        label   = "Probability of illness if infected in decimal form",
        value   = 0.5,
        min     = 0,
        max     = 1
      )
    } else if(input$dist == "uniform") {
      tagList(
        numericInput(
          inputId = ns("dist_par_min"),
          label   = "Min value in decimal form",
          value   = 0.5,
          min     = 0,
          max     = 1
        ),
        numericInput(
          inputId = ns("dist_par_max"),
          label   =  "Max value in decimal form",
          value   = 0.5,
          min     = 0,
          max     = 1
        )
      )
    }
  })

  output$ui_sewage_dist <- renderUI({
    selectInput(
      inputId = ns("sewage_dist"),
      label   = paste("Distribution of pathogen", n(), "in sewage"),
      choices = c("log uniform", "log normal")
    )
  })

  output$ui_sewage_dist_pars <- renderUI({
    validate(need(input$sewage_dist, "Loading..."))
    if(input$sewage_dist == "log uniform") {
      tagList(
        numericInput(
          inputId = ns("sewage_dist_par_min"),
          label   = paste("Min concentration", n(), "in sewage (log10 copies/L)"),
          value   = 5.5,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = ns("sewage_dist_par_max"),
          label   = paste("Max concentration", n(), "in sewage (log10 copies/L)"),
          value   = 8,
          min     = 0,
          max     = 100
        )
      )
    } else if(input$sewage_dist == "log normal") {
      tagList(
        numericInput(
          inputId = ns("sewage_dist_par_alpha"),
          label   = "Alpha (mean) in log10 copies/L",
          value   = 1,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = ns("sewage_dist_par_beta"),
          label   = "Beta value (standard deviation) in log10 copies/L",
          value   = 1,
          min     = 0,
          max     = 100
        )
      )
    }
  })

  # DATA --------------------------------------------------------------------

  cp_sew <- reactive({
    if(input$sewage_dist == "log uniform") {
      runif(
        n   = sample_n(),
        min = input$sewage_dist_par_min,
        max = input$sewage_dist_par_max
      )
    } else if(input$sewage_dist == "log normal") {
      rlnorm(
        n       = sample_n(),
        meanlog = input$sewage_dist_par_alpha,
        meansd  = input$sewage_dist_par_beta
      )
    }
  })

  e <- reactive({
    ww_dose() * 10 ^ cp_sew()
  })

  # Probability of illness
  p_ill <- reactive({
    req(input$resp_mod)
    # Probability of infection
    if(input$resp_mod == "single-parameter exponential") {
      req(input$resp_mod_par_lambda)
      p_inf <- f1(input$resp_mod_par_lambda, e())
    } else if(input$resp_mod == "two-parameter beta-poisson") {
      p_inf <- f2(input$resp_mod_par_alpha, input$resp_mod_par_beta, e())
    } else if(ns("resp_mod") == "two-parameter hypergeometric1f1") {
      p_inf <- f3(input$resp_mod_par_alpha, input$resp_mod_par_beta, e())
    }
    if(input$dist == "uniform") {
      dist <- runif(
        n   = sample_n(),
        min = input$dist_par_min,
        max = input$dist_par_max
      )
    } else if(input$dist == "single value") {
      dist <- input$dist_par_single
    }
    p_inf * dist
  })

  path_name <- reactive({
    input$name
  })

  return(
    list(
      p_ill = p_ill,
      name  = path_name
    )
  )

}
