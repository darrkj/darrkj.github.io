shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Elementary Cellular Automata"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    numericInput("size", "Size of Grid:", 51),
    numericInput("rule", "Rule:", 30)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("CAPlot")
  )
))