
fluidPage(
  tags$head(
    # tags$meta(tags$title('')),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    tags$script(src = 'script.js')
  ),
  tagList(
    useShinyjs(),
    uiOutput('ui')
  )
)
