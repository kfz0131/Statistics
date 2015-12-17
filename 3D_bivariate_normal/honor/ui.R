shinyUI(fluidPage(
  # Title
  headerPanel("Bivariate Normal 3D-Plot"),
  
  # Interactive input
  sidebarLayout(
    sidebarPanel(

        sliderInput(
          "vr",
          label = "Vertical Rotation:",
          min = -180,
          max = 180,
          value = 0
        ),
        sliderInput(
          "hr",
          label = "Horizontal Rotation:",
          min = -180,
          max = 180,
          value = 0
        ),
      checkboxInput("condition", 
                    label = "Condition",
                    value = FALSE),
      conditionalPanel(
        condition = "input.condition == true",
        checkboxInput("X", 
                      label = "Given X",
                      value = FALSE),
        conditionalPanel(
          condition = "input.X == true",
          numericInput("cx",
                       label = "Given X",
                       value = 0)
        ),
        checkboxInput("Y", 
                      label = "Given Y",
                      value = FALSE),
        conditionalPanel(
          condition = "input.Y == true",
          numericInput("cy",
                       label = "Given Y",
                       value = 10)
        )
      ),
      numericInput("mu_x",
                   label = "Mean of x",
                   value = 0),
      numericInput("mu_y",
                   label = "Mean of y",
                   value = 10),
      numericInput("sx",
                   label = "Standard Devidation of x",
                   value = 0.15),
      numericInput("sy",
                   label = "Standard Devidation of y",
                   value = 1),
      numericInput("r",
                   label = "Correlation (Enter between 0 and 1)",
                   value = 0.6),
      radioButtons(
        "resolution", label = h3("Resolution"),
        choices = list("Low", "High")
      )
    ),
    # Show the plot of the 3D bivariate normal distribution
    mainPanel(
      tabsetPanel(
        tabPanel("3D Plot", plotOutput("distPlot")),
        tabPanel("Cutoff 3D Plot", plotOutput("cutoffPlot")),
        tabPanel("Condition Joint Probability", plotOutput("slicePlot"))
      )    
    ),
#     position = c('left','right'),
#     fluid = TRUE
  )
))
