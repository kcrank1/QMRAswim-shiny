
shinyServer(function(input, output, session) {
  
   # For loop for module assignment 
  #Load modules
  # path_num_vector<-reactive(1:input$path_num)

  # for (i in path_num_vector()) {
  #   assign(paste0("path",i),callModule(module= pathogens, id= paste0("path,i"),n=reactive(i),sample_n=sample_n, wwdose_wwdose))
  # 
  # }
  # paths<-reactive({
  #   lapply(path_num_vector(), function(x)  assign(paste0("path",x),
  #                                                  callModule(module = pathogens, id = paste0("path,i"), n = reactive(x), sample_n = sample_n, ww_dose = ww_dose)
  #          ))
  # })
  path1 <- callModule(module = pathogens, id = 'path1', n = reactive(1), sample_n = sample_n, ww_dose = ww_dose)
  path2 <- callModule(module = pathogens, id = 'path2', n = reactive(2), sample_n = sample_n, ww_dose = ww_dose)
  path3 <- callModule(module = pathogens, id = 'path3', n = reactive(3), sample_n = sample_n, ww_dose = ww_dose)
  path4 <- callModule(module = pathogens, id = 'path4', n = reactive(4), sample_n = sample_n, ww_dose = ww_dose)
  path5 <- callModule(module = pathogens, id = 'path5', n = reactive(5), sample_n = sample_n, ww_dose = ww_dose)
  path6 <- callModule(module = pathogens, id = 'path6', n = reactive(6), sample_n = sample_n, ww_dose = ww_dose)
  path7 <- callModule(module = pathogens, id = 'path7', n = reactive(7), sample_n = sample_n, ww_dose = ww_dose)
  path8 <- callModule(module = pathogens, id = 'path8', n = reactive(8), sample_n = sample_n, ww_dose = ww_dose)
  path9 <- callModule(module = pathogens, id = 'path9', n = reactive(9), sample_n = sample_n, ww_dose = ww_dose)
  path10 <- callModule(module = pathogens, id = 'path10', n = reactive(10), sample_n = sample_n, ww_dose = ww_dose)
  # UI ----------------------------------------------------------------------

  output$ui <- renderUI({

    tagList(
      
      h1(id="big-heading", "QMRAswim, A quantitative microbiological risk assesment tool"),
      tags$style(HTML("#big-heading{color: #1A5276;}")), # "#big-heading{background-color:#943126}")), can use to change background color

      #titlePanel("SWIM_R, A quatitative microbiological risk assesment tool"), #1A5276 of swimming in sewage impacted environmental waters"),
      tags$hr(), #horizontal rule
      tags$p(text_blocks$appinfo),
      tags$hr(), #horizontal

      fluidRow(
        column(
          width = 3,
          # Indicator
          uiOutput("ui_indic")
        ),
        column(
          width = 3,
          # Indicator in sewage
          uiOutput("ui_indic_sewage")
        ),
        column(
          width = 3,
          # Seed and count for sampling
          uiOutput("ui_sample")
        ),
        column(
          width = 3,
          # Parameters for distribution of water consumed by swimmers
          uiOutput('ui_dose_pars')
        )
      ),

      fluidRow(
        column(
          width = 6,
          plotOutput("plot_box1"),
          tags$p(text_blocks$plot1)
        ),
        column(
          width = 6,
          plotOutput("plot_hist1")
        )
      ),

      # UI conditional on input$ui_dose_num_pathogens

      fluidRow(
        uiOutput("ui_pathogens")
      ),

      fluidRow(
        uiOutput("ui_download")
      )

    )

  })

  # UI ROW 1 ----------------------------------------------------------------

  output$ui_indic <- renderUI({
    numericInput(
      inputId = "indic",
      label   = "Concentration of indicator in impacted water (genome copies/100mL) *Note, this is not in log10 format.",
      value   = 1,
      min     = 0,
      max     = 100
    )
  })

  output$ui_indic_sewage <- renderUI({
    tagList(
      uiOutput("ui_indic_sewage_dist"),
      uiOutput("ui_indic_sewage_dist_pars")
    )
  })

  output$ui_indic_sewage_dist <- renderUI({
    selectInput(
      inputId  = "indic_sewage_dist",
      label    = "Indicator distribution in sewage",
      choices  = c("log uniform", "log normal"),
      selected = "Log Uniform"
    )
  })

  output$ui_indic_sewage_dist_pars <- renderUI({
    validate(need(input$indic_sewage_dist, 'Loading...'))
    if(input$indic_sewage_dist == "log uniform") {
      tagList(
        numericInput(
          inputId = "indic_sewage_dist_par_max",
          label   = "Maximum concentration (log10 copies/L) ",
          value   = 9.5,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = "indic_sewage_dist_par_min",
          label   = "Minimum concentration (log10 copies/L)",
          value   = 7,
          min     = 0,
          max     = 100
        )
      )
    } else if(input$indic_sewage_dist == "log normal") {
      tagList(
        numericInput(
          inputId = "indic_sewage_dist_par_alpha",
          label   = "Alpha",
          value   = 1,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = "indic_sewage_dist_par_beta",
          label   = "Beta",
          value   = 1,
          min     = 0,
          max     = 100
        )
      )
    }

  })

  output$ui_sample <- renderUI({
    tagList(
      uiOutput("ui_sample_seed"),
      uiOutput("ui_sample_n")
    )
  })

  output$ui_sample_seed <- renderUI({
    numericInput(
      inputId = "sample_seed",
      label   = "Set seed of random number generator",
      value   = 1,
      min     = 0,
      max     = 1000000000
    )
  })

  output$ui_sample_n <- renderUI({
    sliderInput(
      inputId = "sample_n",
      label   = "Number of samplings",
      min     = 0,
      max     = 100000,
      value   = 10000,
      step    = 5000
    )
  })

  output$ui_dose_pars <- renderUI({
    tagList(
      tags$p(text_blocks$doseresponseinfo),
      uiOutput("ui_dose_mean"),
      uiOutput("ui_dose_sd"),
      uiOutput("ui_path_num")
    )
  })

  output$ui_dose_mean <- renderUI({
    numericInput(
      inputId = "dose_mean",
      label   = "Dose mean",
      min     = 0,
      max     = 100,
      value   = 2.92
    )
  })

  output$ui_dose_sd <- renderUI({
    numericInput(
      inputId = "dose_sd",
      label   = "Dose standard deviation",
      min     = 0,
      max     = 10,
      value   = 1.42
    )
  })

  output$ui_path_num <- renderUI({
    numericInput(
      inputId = "path_num",
      label   = "Number of pathogens, up to 10 currently supported.",
      min     = 1,
      max     = 10,
      value   = 1,
      step    = 1
    )
  })

  # UI ROW 2 ----------------------------------------------------------------

  output$plot_box1 <- renderPlot({
    ggplot(data = df_final(), mapping = aes(x = path_id, y = p_ill)) +
      scale_y_log10(labels = trans_format(trans = 'log10', format = math_format(10 ^ .x))) +
      geom_boxplot() +
      ylab("") +
      xlab("") +
      ggtitle("Probability of Illness from Swimming in Contaminated Water") +
      geom_hline(yintercept = 0.03, linetype = "dashed") +
      annotation_logticks(sides = "l") +
      theme_linedraw(base_size = 15) +
      stat_summary(
        fun.y       = mean,
        colour      = "black",
        geom        = "point",
        show.legend = FALSE,
        size        = 1
      ) +
      stat_summary(
        mapping     = aes(label = round(..y.., digits = 1)),
        fun.y       = mean,
        colour      = "black",
        geom        = "text",
        show.legend = FALSE,
        vjust       = -0.7
      )
  })

  output$plot_hist1 <- renderPlot({
    ggplot(data = ww_dose(), mapping = aes(x = dose)) +
      scale_x_log10(labels = trans_format(trans = "log10", format = math_format(10 ^ .x))) +
      geom_histogram(bins = 50) +
      xlab("Dose (mL)") +
      theme_linedraw(base_size = 15)
  })

  # UI ROW 3 ----------------------------------------------------------------

  output$ui_pathogens <- renderUI({
    validate(need(input$path_num, 'Loading...'))
    lapply(X = seq_len(input$path_num), FUN = function(n) {
      column(width = 4, pathogensUI(paste0('path', n)))
    })
  })

  # UI ROW 4 ----------------------------------------------------------------

  output$ui_download <- renderUI({
    tagList(
      uiOutput("ui_download_data"),
      tableOutput("ui_download_preview"),
      tableOutput("ui_meanwwdose"),      ##output style testing, keeping in case I must return to the dark ages
      # tableOutput("ui_cp_sew"),
      # tableOutput("ui_exposure"),
      tableOutput("ui_Pill"),
      tags$p(text_blocks$download_info)

    )
  })

  output$ui_download_data <- renderUI({
    tagList(
      tags$p("Download a dataset generated by this app"),
      selectInput(
        inputId = "dl_data_name",
        label   = "Choose a dataset:",
        choices = c("wastewater dose", "probability of illness", "reshaped probability of illness")
      ),
      downloadButton(
        outputId = "dl_data",
        label    = "Download"
      ),
      actionButton(
        inputId = "dl_data_preview",
        label   = "Preview Data"
      )
    )
  })

  output$ui_download_preview <- renderTable({
    head(dl_dataset())
  }, digits = -1)
 # EXPORTING VALUES FOR TESTING --------------------------------------------- 
 
  dfnew1<-reactive({
    means<-colMeans(df())
    df<-t(data.frame(means))
    format(df, scientific = TRUE)
    })
 output$ui_Pill<-renderTable({dfnew1()})
  
  ## PROBLEM AREA  Note (*maynot be true*), exportTestValues cannot take data frames as arguments, so each data frame of important information has to be disassembled into their component values which is annoying
  # exportTestValues(
  #  mean_dose=mean_wwdose()#,
  #  #tot_p_ill=mean_df_final()#,
  #  #indiv_p_ill=dfnew1()
  # )

  
### used this stone age relic since exportTestValues doesn't seem to work to make sure everything is working, a double double check if you will
   mean_wwdose<-reactive(mean(ww_dose()[,1]))
#   mean_df_final<-reactive(mean(df_final()[,2]))
#   df_head<-reactive((df()[,]))
#   c_p<-reactive(mean(path1$cp_sewkc()))
#   e_p<-reactive(colMeans(path1$ekc()))
#   pinf_t<-reactive(mean(path1$pill()$pinf))
 output$ui_meanwwdose<-renderText({(mean_wwdose())})
# output$ui_meandffinal<-renderText({e_p()})
# output$ui_cp_sew<-renderText({c_p()})
# output$ui_exposure<-renderText({e_p()})



  # DOWNLOAD HANDLER --------------------------------------------------------

  dl_dataset <- eventReactive(
    eventExpr = {
      input$dl_data
      input$dl_data_preview
    },
    valueExpr = {
      if(input$dl_data_name == "wastewater dose") {
        ww_dose()
      } else if(input$dl_data_name == "probability of illness") {
        df_final()
      } else if (input$dl_data_name == "reshaped probability of illness"){
        df()
      }
    }
  )

  output$dl_data <- downloadHandler(
    filename = function() {
      input$dl_data_name %>%
        gsub(pattern = ' ', replacement = '_') %>%
        paste0(., ".csv")
    },
    content = function(file) {
      write.csv(x = isolate(dl_dataset()), file = file, row.names = FALSE)
    }
  )

  # SAMPLING ----------------------------------------------------------------

  # Set seed
  observe({
    set.seed(input$sample_seed)
  })

  # WASTEWATER DOSE FUNCTION ------------------------------------------------

  # for log uniform, WWdose=EnvWaterDose/1000*(fractionWW)
  # exposure volume while swimming event occurs in mL changed
  # to L/indicator concentration changed divided by the range of concentration in sewage.
  # The EnvWaterDose is equal to the rlnorm distribution with the dose mean and dose sd as arguments.
  # This is the water in mouth from swimming value that is default.
  # The fractionWW is equal to the concentration of the indicator in the environment
  # divided by 10^Ci_Sew, or Concentration in Sewage.This is to convert to log10. Concentration in
  # Sewage, Ci_Sew, is calculated using the rlnorm, or normal distribution with the inputs being
  # alpha and beta respectively.
  # For log normal, it is similarly calculated except that Ci_Sew, the concentration of indicator in
  # sewage, is a runif, or a uniform distribution with the min and max arguments included.

  ww_dose <- reactive({
    validate(need(input$indic_sewage_dist, 'Loading...'))
    if(input$indic_sewage_dist == "log uniform") {
      validate(need(input$indic_sewage_dist_par_min, 'Loading...'))
      validate(need(input$indic_sewage_dist_par_max, 'Loading...'))
      dist <- runif(
        n   = input$sample_n,
        min = input$indic_sewage_dist_par_min,
        max = input$indic_sewage_dist_par_max
      )
    } else if(input$indic_sewage_dist == "log normal") {
      validate(need(input$indic_sewage_dist_par_alpha, 'Loading...'))
      validate(need(input$indic_sewage_dist_par_beta, 'Loading...'))
      dist <- rlnorm(
        n       = input$sample_n,
        meanlog = input$indic_sewage_dist_par_alpha,
        sdlog   = input$indic_sewage_dist_par_beta
      )
    }
    env_water_dose <- rlnorm(
      n       = input$sample_n,
      meanlog = input$dose_mean,
      sdlog   = input$dose_sd
    )
    fraction_ww <- input$indic / 10 ^ dist
    dose <- env_water_dose / 1000 * fraction_ww
    data.frame(dose)
  })

  # PROBABILITY ILLNESS FUNCTION --------------------------------------------

  sample_n <- reactive({
    input$sample_n
  })

  p_ill_tot <- reactive({
    req(input$path_num)
    if(input$path_num == 1) {
      1 - (1 - path1$p_ill())
    } else if(input$path_num == 2) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) )
    } else if(input$path_num == 3) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()))
    } else if(input$path_num == 4) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) )
    } else if(input$path_num == 5) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill()))
    } else if(input$path_num == 6) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill())* (1 - path6$p_ill()))
    } else if(input$path_num == 7) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill())* (1 - path6$p_ill())* (1 - path7$p_ill()))
    } else if(input$path_num == 8) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill())* (1 - path6$p_ill())* (1 - path7$p_ill())* (1 - path8$p_ill()))
    } else if(input$path_num == 9) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill())* (1 - path6$p_ill())* (1 - path7$p_ill())* (1 - path8$p_ill())* (1 - path9$p_ill()))
    } else if(input$path_num == 10) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) * (1 - path4$p_ill()) * (1 - path5$p_ill())* (1 - path6$p_ill())* (1 - path7$p_ill())* (1 - path8$p_ill())* (1 - path9$p_ill())* (1 - path10$p_ill()))
  }
  })
#  p_ill_tot <-reactive({
#   req(input$path_num)
#   1-(1-path$p_ill())^input$path_num# RENAME INPUT$PATH_NUM  towards beginning of server path_num<-reactive(input$path_num)
# })
  
  #loop for i in 1:n add column to data frame of path add names as you add columns, create list of names or vector of names
  #create list of length input$path_numm that contains 
  df <- reactive({
    req(input$path_num)
    if(input$path_num == 1) {
      data.frame(path1$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), "Total Pill"))
    } else if(input$path_num == 2) {
      data.frame(path1$p_ill(), path2$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), "Total Pill"))
    } else if(input$path_num == 3) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), "Total Pill"))
    } else if(input$path_num == 4) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), "Total Pill"))
    } else if(input$path_num == 5) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), "Total Pill"))
    } else if(input$path_num == 6) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), path6$p_ill(),  p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), path6$name(), "Total Pill"))
    } else if(input$path_num == 7) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), path6$p_ill(), path7$p_ill(),  p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), path6$name(), path7$name(), "Total Pill"))
    } else if(input$path_num == 8) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), path6$p_ill(), path7$p_ill(), path8$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), path6$name(), path7$name(), path8$name(), "Total Pill"))
    } else if(input$path_num == 9) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), path6$p_ill(), path7$p_ill(), path8$p_ill(),path9$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), path6$name(), path7$name(),path8$name(),path9$name(), "Total Pill"))
    } else if(input$path_num == 10) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), path4$p_ill(), path5$p_ill(), path6$p_ill(), path7$p_ill(), path8$p_ill(),path9$p_ill(),path10$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), path4$name(), path5$name(), path6$name(), path7$name(),path8$name(),path9$name(),path10$name(), "Total Pill"))
    }
  })

  
  
  df_final <- reactive({
    melt(
      data          = df(),
      variable.name = "path id",
      value.name    = "p_ill"
    ) %>%
      set_names(c("path_id", "p_ill"))
  })
})
