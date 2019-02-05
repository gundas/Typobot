library(shiny)

fluidPage(theme = "bootstrap.css",
          fluidRow( align='center',
              titlePanel("Typing Assistant"),
              tags$style(type='text/css', "body { background-color:lightgrey }"),
              tags$style(type='text/css', "#txtInput { background-color:#D7D7D7 }"),
              tags$style(type='text/css', "#txtInput { width: 50%; align: center }"),
              tags$style(type='text/css', "#spacer1 { height: 100px; align: center }"),
              tags$style(type='text/css', "footer { position: fixed; bottom: 10px; color:grey }"),
              
              
              
              
              tags$div(id='spacer1'),
              column ( width=12,
                  tags$div(
                    tags$strong('Input text:'),
                    tags$br(),
                    tags$textarea(id="txtInput", class="form-control", rows=10, cols=80, autofocus='true',""),
                    tags$br(),
                    tags$strong('Prediction'), 
                    #tags$br(),
                    '(press extra space to accept)',
                    tags$strong(':'), 
                    align='center',
                  tags$h1(textOutput("prediction"), align='center' ))
                  
              )
          ),
          
          tags$footer(
            textOutput("mem_used"), 
            textOutput("mem_used_model"))
        
)
