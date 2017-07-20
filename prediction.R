#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#
library(shiny)
library(e1071)
library(shinyjs)
library(ggplot2)



jscode <- "shinyjs.closeWindow = function() { window.close(); }"
# Define UI for application that draws a histogram
ui <- fluidPage(
 
   # Application title
   titlePanel("MARK PREDICTION"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( 
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        titlePanel("INPUTS "),
        numericInput("admno",label = "Admisson No",value = 0),
        
        helpText(" Please carefully fill the details being true to yourself"),
        
        selectInput("Fail", label = h4("Failures"), 
                    choices = list("0" = 0, "1" = 1, "2" = 2,"3"=3), 
                    selected = NULL,selectize = TRUE),
        selectInput("Fedu", label = h4("Father's Education"), 
                    choices = list("None" = 0, "Primary Edu" = 1, "Secondary Edu" = 2,"Higher Secondary Edu"=3," Higher Education "=4), 
                    selected = NULL,selectize = TRUE),
        selectInput("Medu", label = h4("Mother's Education"), 
                    choices = list("None" = 0, "Primary Edu" = 1, "Secondary Edu" = 2,"Higher Secondary Edu"=3," Higher Education "=4), 
                    selected = NULL),
        
        selectInput("st", label = h4("Weekly Study Time"), 
                    choices = list("< 2 hrs" = 1, "2-5 hrs" = 2, "5- 10 hrs" = 3,"> 10 hrs "=4), 
                    selected = NULL),
        numericInput("g1",label = "FA 1 Marks",value = 0, min = 0 , max = 20),
        numericInput("g2",label = "FA 2 Marks",value = 0, min = 0, max = 20),
        numericInput("g3",label = "FA 3 Marks",value = 0, min = 0, max = 20),
        numericInput("g4",label = "FA 4 Marks",value = 0, min = 0, max = 20),
        
        
        actionButton("set",label = "PREDICT", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        actionButton("close",label = "CLOSE",style = "color: #fff; background-color: #E91020; border-color: #940812")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        titlePanel("OUTPUT"),
        plotOutput("gframe"),
        textOutput("frame")
        
       
       
        
      
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  tdata  =read.csv2("StudentData.csv")
  tdata = tdata[,c(5,2,3,4,6,7,8,9,10)]
  
  smodel = svm(G5~.,data=tdata,cost = 200)
  
  
  df <- reactiveValues(adm = 1 , failures = 1,Fedu = 1, Medu = 1,studytime = 1,t1 = 1,t2 = 1, t3 = 1,t4 = 1)
  
  observeEvent( input$set,{ df$adm <-input$admno
  
  df$Fedu<- input$Fedu
  df$Medu <-input$Medu
  df$studytime <- input$st
  df$failures <- input$Fail
  df$t1 <- input$g1
  df$t2 <- input$g2
  df$t3 <- input$g3
  df$t4 <- input$g4
  dataf = data.frame(failures = c(df$failures,0,0),Medu = c( df$Medu,0,0),Fedu = c ( df$Fedu,0,0),studytime = c(df$studytime,0,0), G1= c(df$t1,0,0), G2 = c(df$t2,0,0),G3= c(df$t3,0,0), G4 = c(df$t4,0,0))
  
  
  dataf$failures = as.integer(dataf$failures)
  dataf$studytime = as.integer(dataf$studytime)
  dataf$Fedu = as.integer(dataf$Fedu)
  dataf$Medu = as.integer(dataf$Medu)
  
   
    dataf$G1 = as.integer(dataf$G1)
  
    dataf$G2=as.integer(dataf$G2)
  
    dataf$G3 = as.integer(dataf$G3)
  
    dataf$G4=as.integer(dataf$G4)
  
  
  
  
  
   
  marks = c(input$g1,input$g2,input$g3,input$g4)
  test = c ("FA1","FA2","FA3","FA4")
  
  gdata = data.frame(marks,test)
  
  output$gframe <- renderPlot(ggplot(gdata , aes(x = test,y = marks,group = 1,color = c("green")))+geom_line()+geom_point())
  
  l <- predict(smodel,dataf)
  l <- round(l)
  
  output$frame <- renderText(print(c("Your Predicted Mark for FA 5 is",l[1])))
  output$f <- renderText(print(l[1]))
  
  
    })
  
  
  observeEvent(input$close,{
    if(input$close > 0)
    { js$closeWindow()
      stopApp()
    }
    })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

