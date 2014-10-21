# setwd("~/coursera/data products/project/shiny")
library(shiny)
library(RColorBrewer)
library(ggplot2)

# calculate a fraction based on a ratio
# the input is a factor vector of the form a:b
fraction <- function (v) {
  v <- as.character(v)
  fract.first <- c()
  for (rat in v) {
    fracts <- as.numeric(strsplit(rat, split = ':')[[1]]) # isolate the factors
    tot <- fracts[1] + fracts[2] # compute the total
    first <- fracts[1]
    fract.first <- c(fract.first, round(first / tot, 2)) # compute the fraction
  }
  fract.first
}

# costumized plot
# creates a scatterplot of clustered individuals colored by cluster
myPlot <- function(df) {
  clusterCols <- c("red", "#99CC00", "#FF9900")
  names(clusterCols) <- levels(df$cluster)
  clusterColScale <- scale_colour_manual(name= "cluster", values = clusterCols)
  
  plot <- ggplot(data = df, aes(x = log(faculty), y = sat, colour = cluster, shape = src)) + 
    geom_point(size = 3) +
    theme(panel.background = element_rect(fill = '#E8EDFB')) +
    clusterColScale
}

# read in data and create the desired format
unis <- read.csv("unis.csv", header = TRUE)
unis$uniName <- as.character(unis$uniName)
unis$sat.verbal <- as.numeric(unis$sat.verbal)
unis$sat.math <- as.numeric(unis$sat.math)

fract.faculty <- 1 - fraction(unis$student.faculty.ratio)
unisF <- cbind(unis, fract.faculty)

# the relevant data
uniQuality <- data.frame(university = unisF$uniName,
                         faculty = unisF$fract.faculty, 
                         sat = (unisF$sat.math + unisF$sat.verbal) / 2 )

# select only those entries which does not contain any NA
# (NA causes problems with clustering)
completeEntries <- complete.cases(uniQuality$faculty, uniQuality$sat)
dataForClustering <- data.frame(faculty = uniQuality$faculty, sat = uniQuality$sat)[completeEntries,]

set.seed(113)

# use k-means to cluster the data into 3 groups
model <- kmeans(dataForClustering, centers = 3)

# create a column which indicates where a certain datapoint comes from
# this will become usefull in the interactive part where users can add their own datapoints
src <- rep(factor('file'), nrow(dataForClustering))
levels(src) = c('file', 'user')
lengthSrc <- length(src)

uniQC <- cbind(dataForClustering, cluster = as.factor(model$cluster), src = src)

# plot the dta from the file
plot <- myPlot(uniQC)


# this code deals with the input/output from the user interface (ui.R)
shinyServer(
  function(input, output) {
    # Plot shown before any new datapoints are collected
    output$origPlot <- renderPlot(print(plot))
    
    # Setting up reactive variables to store data for the session
    values <- reactiveValues()
    values$df <- uniQC
    values$counter <- lengthSrc
    values$forClustering <- dataForClustering
    values$src <- src
    
    # Collect new data
    addData <- observe({
      input$addButton
      isolate({
        # read in the new data
        fractFac <- as.numeric(input$fractFac)
        sat <- as.numeric(input$sat)
        # add it to the original
        if (!is.na(sat)) {
          values$forClustering <- rbind(values$forClustering, data.frame(faculty = fractFac, sat = sat))
          values$counter <- values$counter + 1
          values$src[values$counter] <- 'user'
        }
        # compute new clusters
        set.seed(113)
        model <- kmeans(values$forClustering, centers = 3)
        values$df <- cbind(values$forClustering, cluster = as.factor(model$cluster), src = values$src)
      })
    })
    
    # New plot
    output$newPlot <- renderPlot({
      input$addButton
      plot <- myPlot(values$df)
      print(plot)
    })
  }
)