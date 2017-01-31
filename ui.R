library(shiny)



shinyUI(fluidPage(
  # Application title
  headerPanel("What face did you see?"),
  fluidRow(
    column(4,
           #fileInput('file1', 'Choose Image to Upload',
            #         accept=c('jpg','jpeg')),
           tags$h3('Starting Point'),
           radioButtons("gender", label = "Which Gender?",
                        choices = list("female" = 1, "male" = 0), 
                        selected = 0),
           sliderInput("skintone", "Skin Tone", 
                       min=-3, max=3, value=0, step = .001),
           sliderInput("trust", "Trustworthiness", 
                       min=-3, max=3, value=0, step = .001),
           radioButtons("weights", label = "Which weights?",
                        choices = list("All" = 1, "Only p <. 05" = .05, "Only p < .01 " = .01, "only p < .001" = .001), 
                        selected = .001),
           actionButton("button", "Submit!"),
           tags$h3('Additional Adjustments'),
           uiOutput('pc1slider'),
           uiOutput('pc2slider'),
           uiOutput('pc3slider'),
           uiOutput('pc4slider'),
           uiOutput('pc5slider'),
           uiOutput('pc6slider'),
           uiOutput('pc7slider'),
           uiOutput('pc8slider'),
           uiOutput('pc10slider'),
           uiOutput('pc11slider'),
           uiOutput('pc12slider'),
           uiOutput('pc13slider'),
           uiOutput('pc14slider'),
           uiOutput('pc15slider'),
           uiOutput('pc16slider'),
           
           actionButton("reset_input", "Reset inputs")
    ),
    column(4, 
           tags$h3('Starting Point'),
           imageOutput("reconFace")
    ),
    column(4, 
           tags$h3('Additional Adjustments'),
           imageOutput('myImage')
    )   
           
          
  )
  
))

