library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  titlePanel("Marcom Project Dashboard Update"),
  
  fluidRow(
    
    column(3, wellPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("processData", "Process!"),
      tags$hr(),
      tags$h5("Select an action once the data populates:"),
      actionButton("sendData", "Send to dashboard"),
      tags$h5("or"),
      downloadButton("downloadData", "Download .csv")
    
    )),
    
    # Main panel for displaying outputs ----
    column(6,
      h4("Viewing Panel"),
      # Output: Data file ----
      textOutput("status"),
      tableOutput("contents")
      
    ))
)


server <- function(input, output) {
  source("processData.R")
  source("sendData.R")
  
  # load and process the file when button is pushed
  data1 <- reactive({
    if(input$processData == 0){
      return()
    }
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    
    isolate({ 
      input$processData
      my_data <- read.csv(inFile$datapath, stringsAsFactors =FALSE)
      
      
      
      withProgress(message = "Processing...", value = 0, {
        processed <- processData(my_data)
      })
    })
    
    processed
    
  })
  
  # send to dashboard
  dash <- eventReactive(input$sendData, {
    withProgress(message = "Sending...", value = 0, { 
      sendData(data1())
    })
  })
  
  # Output the table
  output$contents <- renderTable({
    if(length(data1())==1){
      data1() 
    } else {
      data1()[,c(1:4,6,9)]
    }
  },
  include.rownames=FALSE) 
  
  # output that we sent it
  output$status <- renderText({
    dash()
  })

  # Download csv 
  output$downloadData <- downloadHandler(
    filename = function() {
     "marcom_project_billables.csv"
    },
    content = function(file) {
      write.csv(data1(), file, row.names = FALSE)
    }
  )
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
