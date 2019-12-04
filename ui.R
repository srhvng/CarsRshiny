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

shinyUI(navbarPage("Project 3",
                   tabPanel(
                       "Information",
                       
                       #add in latex functionality if needed
                       withMathJax(),
                       
                       strong('Note'),
                       p('This web application is developed with',
                         a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                         ''),
                       
                       br(),
                       
                       strong('List of Packages Used'), br(),
                       code('library(shiny)'),br(),
                       code('library(shinyAce)'),br(),
                       code('library(psych)'),br(),
                       code('library(rpart)'),br(),
                       code('library(partykit)'),br(),
                       code('library(randomForest)'),br(),
                       code('library(ggplot2)'),br(),
                       code('library(tidyverse)'),br(),
                       code('library(knitr)'),br(),
                       code('library(DT)'),br(),
                       code('library(readr)'),br(),
                       
                       br(),
                       
                       
                       strong('Application'),
                       p('This application studies a cars data set by means of summary statistics, clustering, and modeling.',
                         'There are a total of 5 tabs, each with their own purpose - this tab is the',
                         'information tab - where information such as the purpose of the application and',
                         'the data is provided. :)'
                         ),
                       
                       p('The EXPLORATION tab provides common numeric and',
                         'graphical summaries of the characteristics of the',
                         'cars data set.'),
                       
                       p('The CLUSTERING tab applies the Ward hierarchical clustering and',
                         'the K-means clustering. On this tab, feel free to download',
                         'the output plots.'),
                       
                       p('The MODELING tab applies the Decision tree and Random forest methods providing',
                         'variable importance and error rates.'
                       ),
                       
                       strong('Data'),
                       p('The data consist of car information of different brands from',
                         'USA, Europe, and Japan obtained from',
                         a('here.', href='https://perso.telecom-paristech.fr/eagan/class/igr204/datasets',
                           target="_blank")),
                       p('It is a data set of 406 cars and 8 characteristics.'),
                       
                       br(),
                       
                       strong('Code'),
                       p('The code for this web application is available at',
                         a('GitHub.', href='https://github.com/srhvng/CarsRshiny', target="_blank")),
                       
                       p('If you want to run this code on your computer (in a local R session), download the data and run the code below:',
                         br(),
                         code('library(shiny)'),br(),
                         code('runGitHub("CarsRshiny","srhvng")')
                       ),
                       
                       p(br())
                   ),
############################################################################################################################################
###############---------- NEW TAB ----------################################################################################################
############################################################################################################################################
                   tabPanel("Exploration",
                   # Application title
                   uiOutput("extitle"),
                   # Sidebar with options for the data set
                   sidebarLayout(
                     sidebarPanel(
                       h3("Cars Data Set"),
                       radioButtons("p", "Select column of Car dataset:",
                                    list("MPG"='MPG', "Cylinders"='Cylinders', "Displacement"='Displacement', "Horsepower"='Horsepower', 
                                         "Weight"='Weight', "Acceleration"='Acceleration',"Model"='Model')),
                       checkboxInput("facet", h4("View by Origin", style = "color:red;"))
                     ),
                     
                     mainPanel(
                       verbatimTextOutput("summary"),
                       plotOutput("histogram", 
                                  click = "plot_click",
                                  dblclick = "plot_dblclick",
                                  hover = "plot_hover",
                                  brush = "plot_brush"),
                       verbatimTextOutput("click")
                     )
                    )
                    ),
############################################################################################################################################
###############---------- NEW TAB ----------################################################################################################
############################################################################################################################################
                   tabPanel(
                       "Clustering",
                       # Application title
                       headerPanel('Clustering of the Cars Data'),
                       
                       # Sidebar with a slider input for number of observations and checkboxes
                       sidebarPanel(
                         sliderInput("clusters", "Number of clusters:", 
                                     min = 2,        # 1 cluster is pointless
                                     max = 10,       # too many is too crowded
                                     value = 4) ,    # sensible start
                         br(),
                         h3("Cluster Dendrogram: Select Variable(s)"),
                         helpText("Note: This Cluster Dendrogram runs Ward hierarchical clustering",
                                  "using a Euclidean distance metric" ,
                                  "(\\(d_{ij}=d(\\{X_i\\}, \\{X_j\\}) = { \\|X_i - X_j\\|^2}\\))",
                                  "and standardised versions of the variables" ,
                                  "(i.e. with mean=0 sd=1) you select in the checkboxes below.",
                                  "You can choose the number of clusters with the slider above.") ,
                         br(),
                         checkboxInput("mpg",  "miles per gallon",   TRUE) , # as in regression project
                         checkboxInput("Displacement", "displacement",      FALSE) ,
                         checkboxInput("Horsepower",   "gross horsepower",  FALSE) ,
                         checkboxInput("Cylinders", "cylinders",   FALSE) ,
                         checkboxInput("Weight",   "weight",             TRUE) , 
                         checkboxInput("Acceleration", "acceleration", FALSE), 
                         checkboxInput("Model", "model", FALSE),
                         
                         #download Dendrogram plot
                         downloadButton("dwnlddendrogram","Download Cluster Dendrogram"),
                         helpText("NOTE: To save file, add extention to 'File Name'" ,
                                  "(ie: .png)"),
                         
                         br(),
                         h3("Scatter Plot: Select Variable"),
                         helpText("Note: This cluster plot runs the K-means clustering.",
                                  "You can choose the axis and the number of clusters with",
                                  "the slider above."),
                         selectInput('xcol', 'X Variable', names(car.int)),
                         selectInput('ycol', 'Y Variable', names(car.int),
                                     selected=names(car.int)[[2]]),
                         
                         #download K-means plot
                         downloadButton("downloadplot","Download K-means Plot"),
                         helpText("NOTE: To save file, add extention to 'File Name'" ,
                                  "(ie: .png)")
                       ),
                       
                       
                       # Show a plot of the generated cluster dendrogram
                       mainPanel(
                         plotOutput("distPlot"),
                         br(),
                         plotOutput('plot1')
                       )
                   ),
############################################################################################################################################
###############---------- NEW TAB ----------################################################################################################
############################################################################################################################################
                   tabPanel(
                       "Modeling",
                       strong("Scatter plot matrices"),
                       br(),
                       
                       plotOutput("corPlot", width="120%"),
                       
                       br(),
                       
                       h3("Decision tree"),
                       uiOutput("varselect1"),
                       
                       radioButtons("explvars", "Choose explanatory variables:",
                                    list("All" = "all", "Select" = "select"), selected = "all"),
                       
                       # Display this only if "expl.vars" is "select"
                       conditionalPanel(condition = "input.explvars == 'select'",
                                        uiOutput("varselect2")
                       ),
                       
                       verbatimTextOutput("dtree"),
                       
                       br(),
                       
                       h3("Plotting the decision tree"),
                       br(),
                       
                       plotOutput("dtreePlot", width="120%"),
                       
                       br(),
                       
                       h3("Random forest"),
                       verbatimTextOutput("randforest"),
                       
                       plotOutput("errorPlot", width="80%"),
                       
                       br(),
                       
                       strong("Variable importance"),
                       
                       plotOutput("varimPlot"),
                       
                       br(),
                       
                       br(),
                       br(),
                       
                       strong('R session info'),
                       verbatimTextOutput("info.out")
                   ),
############################################################################################################################################
###############---------- NEW TAB ----------################################################################################################
############################################################################################################################################
                   tabPanel(
                       "Data",
                       sidebarLayout(
                           sidebarPanel(
                               h3("Cars Data Set"),
                               #Download Data
                               downloadButton("downloadData", "Download"),
                               helpText("NOTE: To save file, add extention to 'File Name'" ,
                                        "(ie: .csv or .txt)")
                           ),
                           mainPanel(
                               DT::dataTableOutput("table")
                           )
                       )
                   )))
