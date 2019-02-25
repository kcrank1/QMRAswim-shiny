
# Define server logic
shinyServer(function(input, output, session) {

  output$appinfo <- renderText(print(text_blocks$appinfo))
  output$doseresponseinfo <- renderText(print(text_blocks$doseresponseinfo))

  # Wastewater Dose function
  #WastewaterDose<-function(input.indic_enviro_conc, input.indic_sewage_dist='Log Uniform',input.min,input.max, input.alpha, input.beta, input.seed =1, input.count=10000,input.dosemean, input.dosesd){
  count<-reactive({input$count})
  reactive({set.seed(reactive({input$seed}))})
  a<-reactive(input$indic_enviro_conc)
  b<-reactive(input$indic_sewage_dist)
  c1<-reactive(input$min)
  d<-reactive(input$max)
  e<-reactive(input$alpha)
  f<-reactive(input$beta)
  g<-reactive(input$dosemean)
  h<-reactive(input$dosesd)
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
  WWdose<-reactive(if (b()=='Log Uniform')

    ((rlnorm(count(),g(),h()))/1000*((a()/10^(runif(count(),min=c1(),max=d())))))

    else if (b()=='Log Normal')

      ((rlnorm(count(),g(),h())/1000*(a()/10^(rlnorm(count(),e(),f())))))

    else print("an error occurred between lines 889 and 893")

  )
  WWdosedf<-reactive(data.frame(WWdose()))

  #  reactive({if (input$indic_sewage_dist== 'Log Uniform'){
  #   Ci_Sew<-runif(count,min=input$min,max=input$max)
  #   EnvWaterDose<-(rlnorm(count, input$dosemean,input$dosesd))
  #   fractionWW<-(input$indic_enviro_conc/10^Ci_Sew)
  #   WWdose<-(EnvWaterDose/1000*(fractionWW))
  #   return(WWdose)
  # } else if(input$indic_sewage_dist == 'Log Normal'){
  #     Ci_Sew<-rlnorm(count,input$alpha,input$beta)
  #     EnvWaterDose<-(rlnorm(count, input$dosemean,input$dosesd))
  #     fractionWW<-(input$indic_enviro_conc/10^Ci_Sew)
  #     WWdose<-(EnvWaterDose/1000*(fractionWW))###exposure volume while swimming event occurs in mL changed to L/indicator concentration changed divided by the range of concentration in sewage
  #    return(WWdose)
  #      } else
  #     stop("Invalid entry for distribution, must be either 'Log Uniform' or 'Log Normal'")
  #  })

  # output$histWWdose<-renderPlot({
  #   p<-qplot(WWdosedf()[,1], geom="histogram")
  #   print(p)
  #   })

  #WWdoseprint<-isolate(WWdose()) to check if it is working
  # print(head(WWdoseprint)

  #Probability_Illness function

  number_of_pathogens<-reactive(input$number_of_pathogens)
  doseresp1o1<-reactive(input$doseresp1o1) #1/1
  alpha1o1 <-reactive(input$alpha1o1)
  beta1o1<-reactive(input$beta1o1)
  lambda1o1<-reactive(input$lambda1o1)
  Pillinfcat1o1 <-reactive(input$Pillinfcat1o1)
  Pillinf1o1<-reactive(input$Pillinf1o1)
  Pillinfmin1o1<-reactive(input$Pillinfmin1o1)
  Pillinfmax1o1<-reactive(input$Pillinfmax1o1)
  path_sewage_dist1o1  <-reactive(input$path_sewage_dist1o1)
  p_min1o1<-reactive(input$p_min1o1)
  p_max1o1<-reactive(input$p_max1o1)
  p_alpha1o1<-reactive(input$p_alpha1o1)
  p_beta1o1 <-reactive(input$p_beta1o1)
  doseresp1o2<-reactive(input$doseresp1o2) #1/2
  alpha1o2<-reactive(input$alpha1o2)
  beta1o2 <-reactive(input$beta1o2)
  lambda1o2<-reactive(input$lambda1o2)
  Pillinfcat1o2 <-reactive(input$Pillinfcat1o2)
  Pillinf1o2<-reactive(input$Pillinf1o2)
  Pillinfmin1o2<-reactive(input$Pillinfmin1o2)
  Pillinfmax1o2 <-reactive(input$Pillinfmax1o2)
  path_sewage_dist1o2 <-reactive(input$path_sewage_dist1o2)
  p_min1o2<-reactive(input$p_min1o2)
  p_max1o2 <-reactive(input$p_max1o2)
  p_alpha1o2 <-reactive(input$p_alpha1o2)
  p_beta1o2 <-reactive(input$p_beta1o2)
  doseresp2o2 <-reactive(input$doseresp2o2)#2/2
  alpha2o2<-reactive(input$alpha2o2)
  beta2o2 <-reactive(input$beta2o2)
  lambda2o2 <-reactive(input$lambda2o2)
  Pillinfcat2o2<-reactive(input$Pillinfcat2o2)
  Pillinf2o2<-reactive(input$Pillinf2o2)
  Pillinfmin2o2 <-reactive(input$Pillinfmin2o2)
  Pillinfmax2o2<-reactive(input$Pillinfmax2o2)
  path_sewage_dist2o2<-reactive(input$path_sewage_dist2o2)
  p_min2o2<-reactive(input$p_min2o2)
  p_max2o2<-reactive(input$p_max2o2)
  p_alpha2o2 <-reactive(input$p_alpha2o2)
  p_beta2o2 <-reactive(input$p_beta2o2)
  doseresp1o3 <-reactive(input$doseresp1o3) #1/3
  alpha1o3<-reactive(input$alpha1o3)
  beta1o3<-reactive(input$beta1o3)
  lambda1o3 <-reactive(input$lambda1o3)
  Pillinfcat1o3<-reactive(input$Pillinfcat1o3)
  Pillinf1o3<-reactive(input$Pillinf1o3)
  Pillinfmin1o3<-reactive(input$Pillinfmin1o3)
  Pillinfmax1o3<-reactive(input$Pillinfmax1o3)
  path_sewage_dist1o3 <-reactive(input$path_sewage_dist1o3)
  p_min1o3<-reactive(input$p_min1o3)
  p_max1o3<-reactive(input$p_max1o3)
  p_alpha1o3<-reactive(input$p_alpha1o3)
  p_beta1o3 <-reactive(input$p_beta1o3)
  doseresp2o3 <-reactive(input$doseresp2o3)#2/3
  alpha2o3<-reactive(input$alpha2o3)
  beta2o3<-reactive(input$beta2o3)
  lambda2o3 <-reactive(input$lambda2o3)
  Pillinfcat2o3<-reactive(input$Pillinfcat2o3)
  Pillinf2o3 <-reactive(input$Pillinf2o3)
  Pillinfmin2o3 <-reactive(input$Pillinfmin2o3)
  Pillinfmax2o3 <-reactive(input$Pillinfmax2o3)
  path_sewage_dist2o3 <-reactive(input$path_sewage_dist2o3)
  p_min2o3 <-reactive(input$p_min2o3)
  p_max2o3 <-reactive(input$p_max2o3)
  p_alpha2o3 <-reactive(input$p_alpha2o3)
  p_beta2o3 <-reactive(input$p_beta2o3)
  doseresp3o3 <-reactive(input$doseresp3o3)#3/3
  alpha3o3<-reactive(input$alpha3o3)
  beta3o3 <-reactive(input$beta3o3)
  lambda3o3 <-reactive(input$lambda3o3)
  Pillinfcat3o3 <-reactive(input$Pillinfcat3o3)
  Pillinf3o3 <-reactive(input$Pillinf3o3)
  Pillinfmin3o3<-reactive(input$Pillinfmin3o3)
  Pillinfmax3o3 <-reactive(input$Pillinfmax3o3)
  path_sewage_dist3o3<-reactive(input$path_sewage_dist3o3)
  p_min3o3<-reactive(input$p_min3o3)
  p_max3o3<-reactive(input$p_max3o3)
  p_alpha3o3 <-reactive(input$p_alpha3o3)
  p_beta3o3<-reactive(input$p_beta3o3)
  pill1_name1o3<-reactive(input$pill1_name1o3)
  pill2_name2o3<-reactive(input$pill2_name2o3)
  pill3_name3o3<-reactive(input$pill3_name3o3)
  pill1_name1o1<-reactive(input$pill1_name1o1)
  pill1_name1o2<-reactive(input$pill1_name1o2)
  pill2_name2o2<-reactive(input$pill2_name2o2)
  #total_pi<-reactive(input$total_pi)





  ###ORGANIZED BY TYPE
  #########Possible Error with log normal, since inputting variables maybe need just rnorm not rlnorm
  Cp_Sew1 <<- reactive( if (number_of_pathogens()== 1) (

    if (path_sewage_dist1o1() == "Log Uniform") (
      runif(count(),min=p_min1o1(),max=p_max1o1()) ###removed log check
    ) else if (path_sewage_dist1o1()== "Log Normal")(
      rlnorm(count(),p_alpha1o1(),p_beta1o1())
    )else ( print("equal is not working"))
  )
  )
  Cp_Sew1o2<-reactive(if (number_of_pathogens()==2)(
    #1/2
    if (path_sewage_dist1o2()== 'Log Uniform')(
      runif(count(),min=p_min1o2(),max=p_max1o2())
    ) else if (path_sewage_dist1o2 ()== 'Log Normal')(
      rlnorm(count(),p_alpha1o2(),p_beta1o2())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew2o2<-reactive( if (number_of_pathogens()==2)(
    if (path_sewage_dist2o2()== 'Log Uniform')(
      runif(count(),min=p_min2o2(),max=p_max2o2())
    ) else if (path_sewage_dist2o2 ()== 'Log Normal')(
      rlnorm(count(),p_alpha2o2(),p_beta2o2())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew1o3<-reactive( if (number_of_pathogens()==3)(
    #1/3
    if (path_sewage_dist1o3()== 'Log Uniform')(
      runif(count(),min=p_min1o3(),max=p_max1o3())
    ) else if (path_sewage_dist1o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha1o3(),p_beta1o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew2o3<-reactive( if (number_of_pathogens()==3)(
    if (path_sewage_dist2o3()== 'Log Uniform')(
      runif(count(),min=p_min2o3(),max=p_max2o3())
    ) else if (path_sewage_dist2o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha2o3(),p_beta2o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))

  Cp_Sew3o3<-reactive( if (number_of_pathogens()==3)(
    if (path_sewage_dist3o3()== 'Log Uniform')(
      runif(count(),min=p_min3o3(),max=p_max3o3())
    ) else if (path_sewage_dist3o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha3o3(),p_beta3o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  e1o1<<-reactive(WWdose()*10^Cp_Sew1())
  e1o2<<-reactive(WWdose()*10^Cp_Sew1o2())
  e2o2<<-reactive(WWdose()*10^Cp_Sew2o2())
  e1o3<<-reactive(WWdose()*10^Cp_Sew1o3())
  e2o3<<-reactive(WWdose()*10^Cp_Sew2o3())
  e3o3<<-reactive(WWdose()*10^Cp_Sew3o3())
  pi1o1<-reactive( if (number_of_pathogens()==1)( ##[p]robability of [i]nfection
    if (doseresp1o1()== "Single parameter exponential")  (
      fu1(lambda1o1(),e1o1())
    ) else if (doseresp1o1 ()=="Two-parameter beta-poisson") (
      f2(alpha1o1(), beta1o1(), e1o1())
    ) else if (doseresp1o1 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o1(), beta1o1(), e1o1())
    )  else(
      stop("Invalid entry for Dose Response")
    ) )
  )
  pi1o2<-reactive(  if (number_of_pathogens()==2)(
    if (doseresp1o2 ()== "Single parameter exponential")  (
      fu1(lambda1o2(),e1o2())
    ) else if (doseresp1o2 ()=="Two-parameter beta-poisson") (
      f2(alpha1o2(), beta1o2(), e1o2())
    ) else if (doseresp1o2 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o2(), beta1o2(), e1o2())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi2o2<-reactive( if (number_of_pathogens()==2)(
    if (doseresp2o2 ()== "Single parameter exponential")  (
      fu1(lambda2o2(),e2o2())
    ) else if (doseresp2o2 ()=="Two-parameter beta-poisson") (
      f2(alpha2o2(), beta2o2(), e2o2())
    ) else if (doseresp2o2 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha2o2(), beta2o2(), e2o2())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi1o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp1o3 ()== "Single parameter exponential")  (
      fu1(lambda1o3(),e1o3())
    ) else if (doseresp1o3 ()=="Two-parameter beta-poisson") (
      f2(alpha1o3(), beta1o3(), e1o3())
    ) else if (doseresp1o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o3(), beta1o3(), e1o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi2o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp2o3 ()== "Single parameter exponential")  (
      fu1(lambda2o3(),e2o3())
    ) else if (doseresp2o3 ()=="Two-parameter beta-poisson") (
      f2(alpha2o3(), beta2o3(), e2o3())
    ) else if (doseresp2o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha2o3(), beta2o3(), e2o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi3o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp3o3 ()== "Single parameter exponential")  (
      fu1(lambda3o3(),e3o3())
    ) else if (doseresp3o3 ()=="Two-parameter beta-poisson") (
      f2(alpha3o3(), beta3o3(), e3o3())
    ) else if (doseresp3o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha3o3(), beta3o3(), e3o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  Pillinf1o1a<-reactive(if (number_of_pathogens()==1)
    if (Pillinfcat1o1()== "uniform")(
      runif(count(),min=Pillinfmin1o1(),max=Pillinfmax1o1())
    ) else if (Pillinfcat1o1()=="single value")(
      Pillinf1o1()
    ) else(
      stop("Something is wrong between lines 888 and 911")
    )
  )
  Pillinf1o2a<-reactive( if (number_of_pathogens()==2)
    if (Pillinfcat1o2()== "uniform")(
      runif(count(),min=Pillinfmin1o2(),max=Pillinfmax1o2())
    ) else if (Pillinfcat1o2()=="single value")(
      Pillinf1o2()
    ) else(
      stop("Something is wrong between lines 888 and 911")
    )
  )
  Pillinf2o2a<-reactive( if (number_of_pathogens()==2)
    if (Pillinfcat2o2()== "uniform")(
      runif(count(),min=Pillinfmin2o2(),max=Pillinfmax2o2())
    ) else if (Pillinfcat2o2()=="single value")(
      Pillinf2o2()
    ))
  Pillinf1o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat1o3()== "uniform")(
      runif(count(),min=Pillinfmin1o3(),max=Pillinfmax1o3())
    ) else  if (Pillinfcat1o3()=="single value")(
      Pillinf1o3()
    ))
  Pillinf2o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat2o3()== "uniform")(
      runif(count(),min=Pillinfmin2o3(),max=Pillinfmax2o3())
    ) else if (Pillinfcat2o3()=="single value")(
      Pillinf2o3()
    ))
  Pillinf3o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat3o3()== "uniform")(
      runif(count(),min=Pillinfmin3o3(),max=Pillinfmax3o3())
    ) else if (Pillinfcat3o3()=="single value")(
      Pillinf3o3()
    ))
  pill1o1<<-reactive(if (number_of_pathogens()==1) #[p]robability of [ill]ness
    pi1o1()*Pillinf1o1a())
  pill1o2<<-reactive(if (number_of_pathogens()==2)
    pi1o2()*Pillinf1o2a())
  pill2o2<<-reactive(if (number_of_pathogens()==2)
    pi2o2()*Pillinf2o2a())
  pill1o3<<-reactive(if (number_of_pathogens()==3)
    pi1o3()*Pillinf1o3a())
  pill2o3<<-reactive(if (number_of_pathogens()==3)
    pi2o3()*Pillinf2o3a())
  pill3o3<<-reactive(if (number_of_pathogens()==3)
    pi3o3()*Pillinf3o3a())

  totpill1o1<-reactive(if (number_of_pathogens()==1)
    1-((1-pill1o1())))

  totpill2<-reactive(if (number_of_pathogens()==2)
    1-((1-pill1o2())*(1-pill2o2())))

  totpill3<-reactive(if (number_of_pathogens()==3)
    1-((1-pill1o3())*(1-pill2o3())*(1-pill3o3())))

  df1 <-reactive({
    df1p<-data.frame(pill1o1(),totpill1o1())
    colnames(df1p) <- c(pill1_name1o1(),"Total Pill")
    df1p
  })
  df2 <-reactive({
    df2p<-data.frame(pill1o2(),pill2o2(),totpill2())
    colnames(df2p) <- c(pill1_name1o2(),pill2_name2o2(),"Total Pill")
    df2p
  })
  df3 <- reactive({
    df3p<-data.frame(pill1o3(),pill2o3(),pill3o3(),totpill3())
    colnames(df3p) <- c(pill1_name1o3(), pill2_name2o3(),pill3_name3o3(),"Total Pill")   #c(pill1_name1o2(),pill2_name2o2(),"Total Pill")
    df3p
  })
  dfm1<-reactive(if (number_of_pathogens()==1)(
    melt(df1(),variable.name = "pathogen id", value.name = "p_ill")))
  dfm2<-reactive(if (number_of_pathogens()==2)(
    melt(df2(),variable.name = "pathogen id", value.name = "p_ill")))
  dfm3<-reactive(if (number_of_pathogens()==3)
    melt(df3(),variable.name = "pathogen id", value.name = "p_ill"))


  finaldf<-reactive(if (is.data.frame(dfm1())==TRUE)
    dfm1()
    else if (is.data.frame(dfm2())==TRUE)
      dfm2()
    else if (is.data.frame(dfm3())==TRUE)
      dfm3()
    else print("All dataframes are null, check input values")
  )


  #plots
  output$boxplot1<-renderPlot({ggplot(finaldf(),
                                      aes(x = finaldf()[,1], y=finaldf()[,2])) +
      scale_y_log10(labels=trans_format('log10',math_format(10^.x)))+
      geom_boxplot() +
      ylab("")+
      xlab("")+
      geom_hline(yintercept=0.03,linetype="dashed")+
      annotation_logticks(sides="l") +
      theme_linedraw(base_size = 15)+
      stat_summary(fun.y=mean, colour="black", geom="point", hape=18, size=1,show_legend = FALSE)+
      stat_summary(fun.y=mean, colour="black", geom="text", show_legend = FALSE,
                   vjust=-0.7, aes( label=round(..y.., digits=1)))

  })
  output$histWWdose<-renderPlot({ggplot(data=WWdosedf(),
                                        aes(x=WWdosedf()[,1])) +
      scale_x_log10(labels=trans_format('log10',math_format(10^.x)))+
      geom_histogram(bins=50)  +
      #ggtitle("Wastewater Dose Distribution") +
      xlab("Dose (mL)") +
      theme_linedraw(base_size = 15)
  })
  #
  # output$boxplot1<-renderPlot(boxplot(pill1(),pill2(),totpill, log = 'y',main="Probability of Illness from Swimming in Contaminated Water", ylab="Probability of Illness"))
  #
  #

  ##functions for use by Probability_Illness code

  fu1<-function(x,N){ ##single parameter exponential
    1-(exp(-x*N))
  }

  f2<-function(a,b,N){##two parameter beta-poisson
    1-(1+N/a)^-b
  }

  f3<-function(a,b,N){###hypergeometric from packageCharFun
    1- hypergeom1F1(-N,a,b)
  }

  dfEXPORT<-reactive(if (is.data.frame(df1())==TRUE)
    df1()
    else if (is.data.frame(df2())==TRUE)
      df2()
    else if (is.data.frame(df3())==TRUE)
      df3()
    else print("All dataframes are null, check input values")
  )

  datasetInput <- reactive({
    switch(input$dataset,
           "Wastewater Dose" = isolate(WWdosedf()),
           "Probability of Illness" = isolate(dfEXPORT()))

  })
  # Table of selected dataset ----

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  observeEvent(input$do,{
    output$table <- renderTable({
      head(datasetInput())

    },
    digits = -2)
  })
  dfmprint<-isolate({df1()}) #to check if it is working
  print(head(dfmprint))

})
