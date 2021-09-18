library(shiny)
library(shinydashboard)
library(ggplot2)
library(esquisse)

ui <- dashboardPage(skin = c('red'),
  dashboardHeader(title = "Dataset Management"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Home", icon = icon("dashboard")),
      menuItem("Dataset", tabName = "Dataset", icon = icon("th")),
      menuItem("Summary",tabName = "Summary",icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Home",
              fluidRow(
                box(background ='blue',plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",background = "blue",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              fluidRow(
                box(background ='blue',plotOutput("plot2", height = 250)),
                
                box(
                  title = "Controls",background = "blue",
                  sliderInput("slider2", "Number of observations:", 1, 100, 50)
                )
              ),
              fluidRow(
                box(background ='blue',plotOutput("plot3", height = 250)),
                
                box(
                  title = "Controls",background = "blue",
                  sliderInput("slider3", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Dataset",
              sidebarLayout(
                sidebarPanel(
                  fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                  radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                  selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                  downloadButton('downloaddatset', "Download"),
                  hr(),
                  radioButtons("trans1", "Transformation:", choices = c("Not-Required", "log", "inverselog", "exponential")),
                  hr()
                  
                ), 
                
                mainPanel(tableOutput("tab1"))
              )
      ),
      
      tabItem(tabName = "Summary",
              sidebarLayout(
                sidebarPanel(
                  selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                  radioButtons("ssoption", "Select Option", choices = c("Summary", "Length", "Dim", "Type of", "Class"))
                  
                ), 
                mainPanel(
                  fluidRow(
                    h3("Summary Statistics"),
                    div(
                      verbatimTextOutput("summar")
                    )
                  )
                )
              ))
      
      
    )
  )
)

server <- function(input, output,session) {
  set.seed(122)
  
  #load the csv here
  df1 = read.csv("./1. logr_svm_heart_data.csv")
  histdata <- df1$age
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    scatter.smooth(x=df1$age[seq_len(input$slider2)],y=df1$chol[seq_len(input$slider2)],xlab = "Age",ylab = "Cholestrol")
    
    
  })
  
  output$plot3 <- renderPlot({
  barplot(height = df1$age[seq_len(input$slider3)],ylab = "Age",xlab ="No. of obs" )
  })
  
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  data_input2 <- reactive({
    infile2 <- input$svmtest
    req(infile2)
    data.frame(read.csv(infile2$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
  }
  )
  
  logdata <- reactive({
    df <- data_input()
    ld <- log(df[, input$cols])
    return(ld)
  })
  
  invlogdata <- reactive({
    df <- data_input()
    ild <- 1/log(df[, input$cols])
    return(ild)
  })
  
  expdata <- reactive({
    df <- data_input()
    expd <- log(df[input$cols])
    return(expd)
  })
  
  
  output$tab1 <- renderTable(
    {
      df <- data_input()
      
      if (input$indata == "Full"){
        print(df)
      } else if(input$trans1 == "Not-Required"){
        data <- df[, input$cols]
        print(data)
      } else if(input$trans1 == "log"){
        logdata()
        
      } else if(input$trans1 == "inverselog"){
        invlogdata()
      } else if(input$trans1 == "exponential"){
        expdata()}
      
    }
  )
  
  
  output$downloaddatset <- downloadHandler(
    
    filename <- function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content <- function(file){
      df <- data_input()
      if(input$trans1 == "log"){
        write.csv(logdata(), file, row.names = TRUE)
      } else if(input$trans1 == "inverselog"){
        write.csv(invlogdata(), file, row.names = TRUE)
      } else if(input$trans1 == "exponential"){
        write.csv(expdata(), file, row.names = TRUE)
      } else if(input$trans1 == "lognormal"){
        write.csv(logno(), file, row.names = TRUE)
      } else if(input$trans1 == "standardize"){
        write.csv(standout(), file, row.names = TRUE)
      }
      
    }
    
  )
  
  # summary statistics
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    if (input$ssoption == "Summary"){
      su <- summary(var1)
      return(su)
    } else if (input$ssoption == "Length"){
      return(length(var1))
    } else if(input$ssoption == "Dim"){
      return(dim(var1))
    } else if (input$ssoption == "Type of"){
      return(typeof(var1))
    } else if(input$ssoption == "Class"){
      return(class(var1))
    }
  })
  
  output$summar <- renderPrint({
    
    if (input$ssoption == "Summary"){
      summ()
    } else if (input$ssoption == "Length"){
      summ()
    } else if(input$ssoption == "Dim"){
      summ()
    } else if (input$ssoption == "Type of"){
      summ()
    } else if(input$ssoption == "Class"){
      summ()
    }
  })
}
shinyApp(ui, server)
