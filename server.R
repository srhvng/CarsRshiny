library(shiny)
library(tidyverse)
library(ggplot2)
library(knitr)
library(DT)
library(psych)
library(rpart)
library(partykit)
library(randomForest)
library(readr)

dat.car <- read_delim("car.txt",delim=";",col_names = TRUE)
dat.car$Origin <- as.factor(dat.car$Origin)
car.int <- dat.car %>% select(-c(Car))

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        newData <- dat.car
    })
    
    #FOR TAB INFORMATION
    #create text info
    output$info <- renderText({
        #Info about data and app
        paste("This app ...")
        paste("The dataset consist of 406 cars with 8 characteristics such as horsepower,acceleration, etc. from their various brand origins")
    })
    
    #FOR TAB EXPLORATION
    #create summary tables
    output$summary <- renderPrint({
        if(input$p=='MPG'){
            newData <- car.int %>% select(MPG)
        }
        if(input$p=='Cylinders'){
            newData <- car.int %>% select(Cylinders)
        } 
        if(input$p=='Displacement'){
            newData <- car.int %>% select(Displacement)
        } 
        if(input$p=='Horsepower'){
            newData <- car.int %>% select(Horsepower)
        } 
        if(input$p=='Weight'){
            newData <- car.int %>% select(Weight)
        } 
        if(input$p=='Acceleration'){
            newData <- car.int %>% select(Acceleration)
        } 
        if(input$p=='Model'){
            newData <- car.int %>% select(Model)
        } 
        
        summary(newData)
    })
    
    output$histogram <- renderPlot({
        #get filtered data
        newData <- getData()
        
        #create plot
        if(input$p=='MPG'){
            g <- ggplot(newData, aes(x = MPG))
        }
        if(input$p=='Cylinders'){
            g <- ggplot(newData, aes(x = Cylinders))
        } 
        if(input$p=='Displacement'){
            g <- ggplot(newData, aes(x = Displacement))
        } 
        if(input$p=='Horsepower'){
            g <- ggplot(newData, aes(x = Horsepower))
        } 
        if(input$p=='Weight'){
            g <- ggplot(newData, aes(x = Weight))
        } 
        if(input$p=='Acceleration'){
            g <- ggplot(newData, aes(x = Acceleration))
        } 
        if(input$p=='Model'){
            g <- ggplot(newData, aes(x = Model))
        } 
        
        if(input$facet){
            g + geom_histogram(aes(y=..density..),binwidth = 1) + geom_density(color="red",line=2) + facet_wrap(~ Origin)
        } else {
            g + geom_histogram(aes(y=..density..),binwidth = 1) + geom_density(color="red",line=2) 
        }
    })
    
    output$click <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                   " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
        }
        
        paste0(
            "click: ", xy_str(input$plot_click),
            "dblclick: ", xy_str(input$plot_dblclick),
            "hover: ", xy_str(input$plot_hover),
            "brush: ", xy_range_str(input$plot_brush)
        )
    })
    
    output$extitle <- renderUI({
        title<- titlePanel(paste("Investigation of Car Data"))
        h1(title)
    })
    
    #FOR TAB CLUSTERING/COMPONENT ANALYSIS
    stand <- function(x){ (x-mean(x))/sd(x) }              # function to standardise
    toinclude <- c( "MPG", "Cylinders", "Displacement", "Horsepower", "Weight", "Acceleration", "Model" ) # selected variables
    standcars <- sapply(subset(dat.car, select=toinclude), "stand")       # apply fun
    rownames(standcars) <- rownames(dat.car)                              # car names
    
    
    output$distPlot <- renderPlot({
        
        # checkboxes and cluster numbers mean plot redrawn so these are reactive
        tocluster <- c(input$MPG, input$Cylinders, input$Displacement, input$Horsepower, input$Weight, input$Acceleration, input$Model)
        if (sum(tocluster)==0){ 
            plot(c(0,1,3), c(1,0,2), type="l", xaxt='n', yaxt='n',       #tick shape
                 main="Please choose one or more variable checkboxes",   #reminder
                 xlab="It doesn't work if you don't",                    #please
                 ylab="You know you want to")                            #joke
        }else{
            dmat <- dist(standcars[, tocluster], method = "euclidean")                   # distances 
            fit <- hclust(dmat, method="ward.D")                           # Hierarchical clustering
            plot(fit, main=paste("Cluster Dendrogram. Mean height (i.e. distance):", round(mean(dmat),1)),
                 xlab="You can choose how many clusters and which variables")    # Display dendogram
            rect.hclust(fit, k=input$clusters, border="red")              # red boxes round clusters
        }
        
    })
    
    #Download dendrogram
    output$dwnlddendrogram <- downloadHandler(
        filename = function(){
            paste("Dendrogram","png",sep = ".")
        },
        
        content = function(file){
            png(file)
            tocluster <- c(input$MPG, input$Cylinders, input$Displacement, input$Horsepower, input$Weight, input$Acceleration, input$Model)
            if (sum(tocluster)==0){ 
                plot(c(0,1,3), c(1,0,2), type="l", xaxt='n', yaxt='n',
                     main="Please choose one or more variable checkboxes",
                     xlab="It doesn't work if you don't",
                     ylab="You know you want to")
            }else{
                dmat <- dist(standcars[, tocluster], method = "euclidean")                   # distances 
                fit <- hclust(dmat, method="ward.D")                           # Hierarchical clustering
                plot(fit, main=paste("Cluster Dendrogram. Mean height (i.e. distance):", round(mean(dmat),1)))
                rect.hclust(fit, k=input$clusters, border="red")              # red boxes round clusters
            }
            dev.off()
        }
    )
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        car.int[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    #Download K-means plot
    output$downloadplot <- downloadHandler(
        filename = function(){
            paste("K-means_Plot","png",sep = ".")
        },
        
        content = function(file){
            png(file)
            palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
            
            par(mar = c(5.1, 4.1, 0, 1))
            plot(selectedData(),
                 col = clusters()$cluster,
                 pch = 20, cex = 3)
            points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
            dev.off()
        }
    )
    
    #FOR TAB MODELING 
    bs <- reactive({
        x <- car.int
        describe(x)[2:13]
    })
    
    
    makecorPlot <- function(){
        x <- car.int
        pairs.panels(x)
    }
    
    output$corPlot <- renderPlot({
        print(makecorPlot())
    })
    
    
    
    
    output$varselect1 <- renderUI({
        
        dat <- car.int
        cols <- names(dat)
        selectInput("vars1", "Select one criterion (outcome) variable:", choices=cols, selected=NULL, multiple=FALSE)
        
    })
    
    
    output$varselect2 <- renderUI({
        
        dat <- car.int
        cols <- names(dat)
        selectInput("vars2", "Click the box below and select the explanatory variables:", choices=cols, multiple=T)
        
    })
    
    
    
    
    decisiontree <-  reactive({
        
        if (input$explvars == "all") {
            
            dat <- car.int
            #outcome <- dat[,input$vars1]
            #dat[,input$vars1] <- NULL
            #res <- rpart(outcome ~., data=dat)
            res <- rpart(as.formula(paste(input$vars1, "~.")),data=dat)
            print(res)
            
        } else { # select
            
            if (is.null(input$vars2) == TRUE){
                res <- c("You need to select the explanatory variables.")
                print(res)
                
            } else {
                dat <- car.int
                res <- rpart(as.formula(paste(input$vars1," ~ ",paste(input$vars2,collapse="+"))),data=dat)
                print(res)
                
            }
        }
        
        list(res = res)
        
    })
    
    output$dtree <- renderPrint({
        decisiontree()
    })
    
    
    
    
    dtreePlot <- function(){
        
        if (input$explvars == "all") {
            
            res <- decisiontree()$res
            plot(as.party(res))
            
        } else {
            
            if (is.null(input$vars2) == TRUE){
                
                NULL
                
            } else {
                
                res <- decisiontree()$res
                plot(as.party(res))
            }
        }
    }
    
    output$dtreePlot <- renderPlot({
        print(dtreePlot())
    })
    
    
    
    
    randforest <-  reactive({
        
        if (input$explvars == "all") {
            
            dat <- car.int
            forest <- randomForest(as.formula(paste(input$vars1,"~.")),data=dat)
            impt <- forest$importance
            
        } else { # select
            
            if (is.null(input$vars2) == TRUE){
                forest <- c("You need to select the explanatory variables.")
                impt <- c("You need to select the explanatory variables.")
            } else {
                dat <- car.int
                forest <- randomForest(as.formula(paste(input$vars1," ~ ",paste(input$vars2,collapse="+"))),data=dat)
                impt <- forest$importance
            }
        }
        
        list(result = forest, importance = impt)
        
    })
    
    output$randforest <- renderPrint({
        randforest()
    })
    
    
    
    
    errorPlot <- function(){
        
        if (input$explvars == "all") {
            
            forest <- randforest()$result
            plot(forest, main="Error rate")
            
        } else {
            
            if (is.null(input$vars2) == TRUE){
                
                NULL
                
            } else {
                
                forest <- randforest()$result
                plot(forest, main="Error rate")
            }
        }
    }
    
    output$errorPlot <- renderPlot({
        print(errorPlot())
    })
    
    
    
    
    varimPlot <- function(){
        
        if (input$explvars == "all") {
            
            forest <- randforest()$result
            varImpPlot(forest, main="")
            
        } else {
            
            if (is.null(input$vars2) == TRUE){
                
                NULL
                
            } else {
                
                forest <- randforest()$result
                varImpPlot(forest, main="")
            }
        }
    }
    
    output$varimPlot <- renderPlot({
        print(varimPlot())
    })
    
    
    
    
    
    
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
        info2 <- paste("It was executed on ", date(), ".", sep = "")
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })
    #FOR TAB DATA
    #create output of observations    
    output$table <- DT::renderDataTable({
        getData()
    })
    
    #Download data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Cars_Data", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(getData(), file, row.names = FALSE)
        }
    )
    
    #dynamic title
    output$title <- renderUI({
        title<- titlePanel(paste("Investigation of",str_to_title(input$Origin), "Mammal Sleep Data"))
        h1(title)
    })
    
})