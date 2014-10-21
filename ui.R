library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("University"),
  sidebarPanel(
    h3('Add a new university'),
    textInput('fractFac', label = 'What fraction of the people at the university are faculty members?'),
    textInput('sat', label = 'What is the average sat score for this university?'), 
    actionButton('addButton', 'Add')
  ),
  mainPanel(
    p('The average sat-score at a university as a function 
      of the fraction of people who are faculty members, divided into three groups'),
    
    conditionalPanel(condition = "input.addButton == 0", plotOutput('origPlot')), # shown when page loads
    conditionalPanel(condition = "input.addButton != 0", plotOutput('newPlot')), # shown when the user has added a data point
    # plotOutput('newPlot'),
    
    p('Data from', a('UCI',href = "https://archive.ics.uci.edu/ml/datasets/University"))
  )
))