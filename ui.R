library(shiny)
fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('TextFile', 'Choose Text file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain'
                )
      ),
      tags$hr(),
      radioButtons('skipper', 'Lines to skip',
                             c(Zero=0,
                               One=1
                               )),
      tags$hr(),
      numericInput('year','Enter Starting Year',value =2017 ),
      tags$hr(),
      numericInput('month','Enter Starting month',value=01),
      tags$hr(),
      numericInput('frequency','Enter Frequency',value=12)
    ),

    mainPanel(
      tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
    ),
      fluidRow("ARIMA",
        splitLayout(cellWidths = c("50%", "50%"),
                    plotOutput("contents1"), plotOutput("contents2"))

      ))
  )
)
