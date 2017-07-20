#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(crosstalk)
library(devtools)
library(d3scatter)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("VISUALIZATION"),
   
   # Sidebar with a slider input for number of bins 
      
      # Show a plot of the generated distribution
      mainPanel(
                  print(h5("FA1 MARKS DISTRIBUTION")),
                  d3scatterOutput("sc1"),
                  print(h5("FA2 MARKS DISTRIBUTION")),
                  d3scatterOutput("sc2"),
                  print(h5("FA3 MARKS DISTRIBUTION")),
                  d3scatterOutput("sc3"),
                  print(h5("FA4 MARKS DISTRIBUTION")),
                  d3scatterOutput("sc4"),
                  print(h5("STUDY TIME OF STUDENTS")),
                  print(h6("< 2 hrs = 1")),
                  print(h6("2-5 hrs = 2")),
                  print(h6("5-10 hrs = 3")),
                  print(h6(">10 hrs = 4")),
                  plotOutput("study"),
                  print(h5("NO. OF PAST FAILURES OF STUDENTS")),
                  plotOutput("failures")
                 
              
        
        )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
   df<- read.csv2("StudentData.csv") 
   g1 = character(length = length(df$G1))
   g2 = character(length = length(df$G2))
   g3 = character(length = length(df$G3))
   g4 = character(length = length(df$G4))
   
    g1[df$G1>=0 & df$G1<8]<-"Low"
    g1[df$G1>=8 & df$G1<16]<-"Average"
    g1[df$G1>=16 & df$G1<21]<-"High"
    
    g2[df$G2>=0 & df$G2<8]<-"Low"
    g2[df$G2>=8 & df$G2<16]<-"Average"
    g2[df$G2>=16 & df$G2<21]<-"High"
    
    g3[df$G3>=0 & df$G3<8]<-"Low"
    g3[df$G3>=8 & df$G3<16]<-"Average"
    g3[df$G3>=16 & df$G3<21]<-"High"
    
    g4[df$G4>=0 & df$G4<8]<-"Low"
    g4[df$G4>=8 & df$G4<16]<-"Average"
    g4[df$G4>=16 & df$G4<21]<-"High"
    
    
  
    
    
    df = cbind(df,g1,g2,g3,g4)
  
    
   s_dataf <- SharedData$new(df)
   output$sc1 <- renderD3scatter(d3scatter(s_dataf,~X,~G1,~g1,width="10%",height = "50%",x_label = "No.of Students"))
   output$sc2 <- renderD3scatter(d3scatter(s_dataf,~X,~G2,~g2, width="50%",height = "50%",x_label = "No. of Students"))
   output$sc3 <- renderD3scatter(d3scatter(s_dataf,~X,~G3,~g3, width ="50%",height = "50%",x_label = "No. of Students"))
   output$sc4 <- renderD3scatter(d3scatter(s_dataf,~X,~G4,~g4, width ="50%",height = "50%",x_label = "No. of Students"))
   
   
   data2 <- debounce(reactive(s_dataf$data(withSelection = TRUE)), 250)
   output$study <- renderPlot({
     ggplot(data2(), aes(studytime, fill = crosstalk::selection_factor(selected_))) +
       geom_bar(stat = "count") +
       crosstalk::scale_fill_selection("#ff9966", "#1ab2ff")
   })
   
   output$failures <- renderPlot({
     ggplot(data2(), aes(failures, fill = crosstalk::selection_factor(selected_))) +
       geom_bar(stat = "count") +
       crosstalk::scale_fill_selection("#ff9966", "#1ab2ff")
   })
}


# Run the application 
shinyApp(ui = ui, server = server)


