#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(ggplot2)
library(DT)
library(hashids)

# for online deployment, one has to use remote storing, an article about which
# can be found here: https://shiny.rstudio.com/articles/persistent-data-storage.html#basic
#####
#Functions
saveData <- function(in_data, current_data, fileName, dat_ID) {
  out_data <- data.frame("ID" = dat_ID,
                         "date_start" = in_data$date_start, 
                         "date_stop" = in_data$date_stop, 
                         "tr_type" = in_data$tr_type,
                         "number_inspections" = in_data$number_inspections,
                         "total_rides" = in_data$total_rides,
                         "faction" = in_data$number_inspections /in_data$total_rides,
                         "empirical_b" = 0)
  #combine the data frames
  if (!is.null(current_data)){
    latest_data <- rbind(current_data, out_data)
    write.csv(latest_data, fileName, row.names = FALSE)
  } else {
    print("First save")
    # Write the file to the local system
    write.csv(out_data, fileName, row.names = FALSE)
  }
}

loadData <- function(fileName) {
    if (file.exists(fileName)){
      ldata <- as.data.frame(read.csv(fileName, stringsAsFactors = FALSE, header = TRUE))
      #recover correct date time
      ldata$date_start <- as.Date(ldata$date_start, origin = "1970-01-01")
      ldata$date_stop <- as.Date(ldata$date_stop, origin = "1970-01-01")
      return(ldata)
    } else {
      cat("No file: ", fileName)
    }
}

#####

#Upfront working directory setup
data_filename <- "data/conductor.csv"
if (!dir.exists("data")) {
  dir.create("data")
}

#####

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Bayesian Conductor"),
   # Sidebar with 
   sidebarLayout(
      sidebarPanel(
        h4("Insert your data"),
        p("Create an individual data record where you enter your experienced ticket inspections during
          your total number of train rides."),
        selectInput("tr_type", 
                    label="Select the train type you used: ", 
                    choices = list("ICE" = 1, "RE/RB" = 2,
                                   "S-Bahn" = 3), selected = 1),
        helpText("Note: Please provide a date range during which you took train rides,",
                 "a count of ticket inspections you experienced (see below),",
                 "and the total number of train rides you took during the reported date range."),
        dateRangeInput("date_range", h4("Date range")),
        sliderInput("total_rides", h4("Total number of train rides"),
                    min = 0, max = 100, value = 5),
        sliderInput("number_inspections", "Total number of ticket inspections for these rides",
                    min = 0, max = 100, value = 3),
        actionButton("Submit", "Submit your data")
      ),
      mainPanel(DT::dataTableOutput("responses", width = 300), tags$hr(),
                textOutput("type_selected"),
                textOutput("date_range_sel"),
                textOutput("total_rides"),
                textOutput("total_inspections"),
                textOutput("hello_world"),
                plotOutput("dat_plot"))
   )
)

# Define server logic
server <- function(input, output) {
  
  # set up reactive values, i.e. dynamic values
  values <- reactiveValues(counter = 0, current_dat = NULL)
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    newdata <- list(date_start= as.Date(input$date_range[1], origin = "1970-01-01"), 
                    date_stop= as.Date(input$date_range[2], origin = "1970-01-01"), 
                    tr_type= as.integer(input$tr_type), 
                    number_inspections= input$number_inspections, 
                    total_rides= input$total_rides)
    newdata #return
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$Submit, {
    values$counter <- values$counter + 1
    saveData(formData(), current_data =  values$current_dat, fileName = data_filename, dat_ID = values$counter)
    #get latest data after saving
    values$current_dat <- loadData(fileName = data_filename)
    output$hello_world <- renderText("Thank you for your data!")
  })
  
  output$type_selected <- renderText({paste("You have selected type: ", input$tr_type)})
  output$date_range_sel <- renderText({paste("dates from: ", as.Date(input$date_range[1], origin = "1970-01-01"), " until: ", as.Date(input$date_range[2], origin = "1970-01-01"))})
  output$total_rides <- renderText({paste("total number of train rides:", input$total_rides)})
  output$total_inspections <- renderText({paste("total number of ticket inspections: ", input$number_inspections)})
  
  #render Table
  output$responses <- DT::renderDataTable({
    print("Render load of data")
    #dynamically input this new data to the reactive environment
    values$current_dat <- loadData(data_filename)
    #load latest ID from data 
    if (values$current_dat$ID[nrow(values$current_dat)]>1){
      values$counter <- values$current_dat$ID[nrow(values$current_dat)]
    }
    #render current data
    input$Submit
    loadData(fileName = data_filename)
    })     
}

# Run the application 
shinyApp(ui = ui, server = server)

