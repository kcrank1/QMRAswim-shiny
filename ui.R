library(shinythemes)
fluidPage(theme=shinytheme("spacelab"),
  tags$head(
    # tags$meta(tags$title('')),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    tags$script(src = 'script.js')
  ),
  tagList(
    useShinyjs(),
    uiOutput('ui'),
    tags$b("Note:"),
    p(),
    tags$p("This is intended for academic use. This content is not intended to be a substitute for professional medical advice, diagnosis, or treatment. Always seek the advice of your physician or other qualified health provider with any questions you may have regarding a medical condition. We do not make any money on this and we will not sell your data. We ask that you cite this app and the publications when you use the results."),
    p(),
    tags$b("To cite this application:"),
    p(),
    p(" Crank, K., Petersen, S., Bibby, K. (2019). QMRAswim: Quantifing the Probability of Illness of Swimming in Sewage Impacted Water. http://xxxxx."),
    tags$b("More information can be found in the following publication:"),
    p(),
    tags$a(href="https://placeholder", "Placeholder (2000)"),
    p(),
    tags$hr(),
    tags$i("Source code for this app is",
    a(href="https://github.com/kcrank1/QMRAswim", "here")),
    tags$hr()
            
)
)
