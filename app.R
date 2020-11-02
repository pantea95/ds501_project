
library(magrittr)
library(rvest)
library(readxl)
library(maps)
library(ggplot2)
library(cluster)
library(ggvis)
library(dplyr)
library(shiny)
library(ggiraph)
library(RColorBrewer)
library(plsdepot)
library(visreg)
library(caret)
library(leaflet)
library(shinythemes)
library(DT)
library(SpatialEpi)

## read the data:
# realEstate <- read_excel("C:\\Users\\pante\\Desktop\\uni\\Fall 2020\\DS501 - Intro to Data Science\\case study 3\\dataset\\realEstate\\Real estate valuation data set.xlsx")
realEstate <- read_excel("Real estate valuation data set.xlsx")
# realEstate <- read_excel(destfile)
# add transaction year column:
realEstate$TXyear <- floor(realEstate$`X1 transaction date`)
# remove first column (contains ID):
realEstate <- realEstate[,-1]
names(realEstate) <- c("TXDate","age","MRTDist","numStores","latitude","longitude", "unitPrice","TXYear")
realEstate$xcoord <- latlong2grid(cbind(realEstate$longitude,realEstate$latitude))$x
realEstate$ycoord <- latlong2grid(cbind(realEstate$longitude,realEstate$latitude))$y
realEstate$rcoord <- sqrt(realEstate$xcoord^2 + realEstate$ycoord^2)

ui = navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                "Real Estate in New Taipei", id="main",
                tabPanel("Map",
                         div(class="outer",
                             leafletOutput("bbmap", height=1000),
                             absolutePanel(id = "controls", class = "panel panel-default",
                                           top = 75, left = 55, width = 250, fixed=TRUE,
                                           draggable = TRUE, height = "auto",
                                           sliderInput("priceRange", 
                                                       label = "Range of unit price:",
                                                       min = 0, max = ceiling(max(realEstate$unitPrice))+10, value = c(0, ceiling(max(realEstate$unitPrice))+10))
                             )
                         )),
                tabPanel("Scatter Plots",
                         div(class="outer",
                             sidebarPanel(
                                 selectInput('x','X-Axis',
                                             list("Distance to the nearest MRT station" = "MRTDist", 
                                                  "House age" = "age",
                                                  "Num of convenience stores in close proximity" = "numStores",
                                                  "Geographic latitude" = "latitude",
                                                  "Geographic longitude" = "longitude",
                                                  "Transaction date" = "TXDate", 
                                                  "Price per unit area" = "unitPrice"), selected="MRTDist"),
                                 selectInput('y','Y-Axis',
                                             list("Distance to the nearest MRT station" = "MRTDist", 
                                                  "House age" = "age",
                                                  "Num of convenience stores in close proximity" = "numStores",
                                                  "Geographic latitude" = "latitude",
                                                  "Geographic longitude" = "longitude",
                                                  "Transaction date" = "TXDate", 
                                                  "Price per unit area" = "unitPrice"), selected="unitPrice")
                             ),
                             mainPanel(uiOutput("scatterplot"))
                         )
                ),
                tabPanel("Clusters",
                         div(class="outer",
                             sidebarPanel(
                                 numericInput('numOfClusters', 'Cluster count', 5, min = 1, max = 15)
                             ),
                             mainPanel(plotOutput('kmeanCluster'))
                         )
                ),
                tabPanel("Regression",
                         div(class="outer",
                             sidebarPanel(
                                 selectInput('indepvar','Independent Variable ',
                                             list("Distance to the nearest MRT station" = "MRTDist", 
                                                  "House age" = "age",
                                                  "Num of convenience stores in close proximity" = "numStores",
                                                  "Geographic latitude" = "latitude",
                                                  "Geographic longitude" = "longitude",
                                                  "Transaction date" = "TXDate"),
                                             selected="MRTDist"),
                                 selectInput("regModel", label ="Regression Model",
                                             choices = list("Linear", "Logarithmic"), selected = 1)
                             ),
                             mainPanel(verbatimTextOutput("LRSummary")
                                       # ,uiOutput("regressionplot")
                                       )
                         )
                ),
                tabPanel("Data",
                         DT::dataTableOutput("data"),
                         tags$br(),tags$br(),
                         "Adapted data from ", tags$a(href="https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set#", 
                                                      "Real Estate Valuation Data Set.")
                ),
                tabPanel("About",
                         # includeCSS("C:\\Users\\pante\\Desktop\\uni\\Fall 2020\\DS501 - Intro to Data Science\\case study 3\\codes\\realEstate.html")
                         includeCSS("realEstate.html")
                )
)



server = shinyServer(function(input, output) {
   
    mainData <- cbind(realEstate$latitude,realEstate$longitude, realEstate$unitPrice, realEstate$xcoord, realEstate$ycoord)
    mainData <- as.data.frame(mainData)
    colnames(mainData) <- c("Latitude", "Longitude", "Price", "xcoord", "ycoord")

    # new column for the popup label
    mainData <- mainData %>% 
        mutate(cntnt=paste0('<strong>Unit Price: </strong>',Price))
    
    # create a color paletter for category type in the data file
    domain <- range(mainData$Price)
    # generate colors
    n <- 60
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    pal <- colorFactor(pal = col_vector, domain = domain)

    # leaflet_data <- mainData
    leaflet_data <- reactive({
        subset(mainData,mainData$Price>=input$priceRange[1] & mainData$Price<=input$priceRange[2])
    })
    
    # create the leaflet map  
    output$bbmap <- renderLeaflet({
        leaflet(leaflet_data()) %>% 
            addTiles() %>%
            addCircleMarkers(data = leaflet_data(), lat =  ~Latitude, lng =~Longitude,
                             radius = ~sqrt(Price)*2, fillOpacity = 0.5, weight = 1,
                             label = sprintf("<strong>Unit Price: %s</strong>", mainData$Price) %>% lapply(htmltools::HTML),
                             # popup = ~as.character(cntnt),
                             # color = ~pal(clusters()$cluster),
                             color = "#045a8d",
                             stroke = FALSE)%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs",
                title="ME"
            ))
    })
    
    #create a data object to display data
    output$data <-DT::renderDataTable(datatable(
        mainData %>% select(c(Longitude, Latitude, Price))
    ))
    
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        cbind(mainData$xcoord,mainData$ycoord)
    })
    # make clusters:
    clusters <- reactive({
        kmeans(selectedData(), input$numOfClusters)
    })

    # create k-means:
    output$kmeanCluster <- renderPlot({
        palette(col_vector)
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             xlab="X Coordinate", ylab="Y Coordinate",
             col = clusters()$cluster,
             pch = 20, cex = 3)
    })
    
    
    output$plot <- renderPlot({
        p <- ggplot(realEstate,aes_string(x=input$x, y=input$y))+geom_point()+theme_bw()
        print(p)
    })
    output$scatterplot <- renderUI(plotOutput("plot",brush = brushOpts("plot_brush")))
    
    
    
    is.bad <- function(x) any(is.na(x) | is.infinite(x))
    
    # Regression output
    output$LRSummary <- renderPrint({
        
        ## split the data to generate the train and test data sets:
        set.seed(324)
        splitPrice = caret::createDataPartition(realEstate$unitPrice, p = 0.8, list=F, times=1)
        trainSet = realEstate[splitPrice,]
        testSet = realEstate[-splitPrice,]
        
        ## train
        if (input$regModel == "Linear") {
            myFormula <- as.formula(paste('unitPrice ~',input$indepvar))
            mm <- model.matrix(myFormula, trainSet)
            mm[which(mm==-Inf)] = NA
            fit <- lm(myFormula, data=trainSet, subset=!apply(mm, 1, is.bad))
        } else {
            myFormula <- as.formula(paste('unitPrice ~ log(',input$indepvar,')'))
            mm <- model.matrix(myFormula, trainSet)
            mm[which(mm==-Inf)] = NA
            fit <- lm(myFormula, data=trainSet, subset=!apply(mm, 1, is.bad))
        }

        ## test:
        mm <- model.matrix(myFormula, testSet)
        mm[which(mm==-Inf)] = NA
        predPrice = data.frame(predict(fit, newdata=testSet, subset=!apply(mm, 1, is.bad)))
        colnames(predPrice)[1] = 'Predicted'
        predPrice$Reference = testSet[,c('unitPrice')]
        predPrice$Predicted <- unlist(predPrice$Predicted)
        predPrice$Reference <- unlist(predPrice$Reference)
        
        ## evaluation:
        predPrice$Reference[which(predPrice$Reference==Inf)] = NA
        predPrice$Reference[which(predPrice$Reference==-Inf)] = NA
        predPrice$Predicted[which(predPrice$Predicted==Inf)] = NA
        predPrice$Predicted[which(predPrice$Predicted==-Inf)] = NA
        PRESS = sum((predPrice$Reference - predPrice$Predicted)^2, na.rm = TRUE)
        SST = sum((predPrice$Reference - mean(predPrice$Reference))^2, na.rm = TRUE)
        R2 = 1 - (PRESS/SST)
        print(paste0("R2 = ", R2))
        
    })
    
    
    
    # output$plot_fit <- renderPlot({
    #     if (input$regModel == "Linear"){ 
    #         p.data <- qplot(input$indepvar, unitPrice, data=realEstate) + geom_point(colour = "#3366FF", size = 3)
    #         # p.data <- ggplot(realEstate,aes_string(x=input$indepvar, y=unitPrice))+geom_point()+theme_bw()
    #         fit <- lm(as.formula(paste('unitPrice ~',input$indepvar)), data=trainSet, na.action = na.exclude)
    #     } else {
    #         p.data <- qplot(as.formula(paste('log(input$indepvar)')), unitPrice, data=realEstate) + geom_point(colour = "#3366FF", size = 3)
    #         fit <- lm(as.formula(paste('unitPrice ~ log(',input$indepvar,')')), data=trainSet, na.action = na.exclude)
    #     }
    #     p.fit = p.data + geom_abline(intercept = fit[1]$coefficients[1], slope = fit[1]$coefficients[2], color="red")
    #     
    #     print(p.fit)
    # })
    # output$regressionplot <- renderUI(plotOutput("plot_fit",brush = brushOpts("plot_brush")))
    
    
    
})

shinyApp(ui = ui, server = server)