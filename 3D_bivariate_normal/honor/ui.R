shinyUI(fluidPage(
  # Title
  headerPanel("Bivariate Normal 3D-Plot"),
  
  # Interactive input
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(10,
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
#       sliderInput(
#         "cx",
#         label = "Given x:",
#         min = 0 - 3.5 * 0.15,
#         max = 0 + 3.5 * 0.15,
#         value = 0
#       ),
#       sliderInput(
#         "cy",
#         label = "Given y:",
#         min = 10 - 3.5 * 1,
#         max = 10 + 3.5 * 1,
#         value = 10
#       ),
      numericInput("cx",
             label = "Given X",
             value = -999),
      numericInput("cy",
             label = "Given Y",
             value = -999),
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
      )))
    ),
    # Show the plot of the 3D bivariate normal distribution
    mainPanel(
      tabsetPanel(
        tabPanel("3D Plot", plotOutput("distPlot")),
        tabPanel("Conditional 3D Plot", plotOutput("conditionPlot")),
        tabPanel("Contour Plot", plotOutput("contour"))
      )    
    ),
    position = c('left','right'),
    fluid = TRUE
  )
))
