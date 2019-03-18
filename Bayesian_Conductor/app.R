#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(hashids)

# for online deployment, one has to use remote storing, an article about which
# can be found here: https://shiny.rstudio.com/articles/persistent-data-storage.html#basic

saveData <- function(in_data, fileName, dat_ID) {
  out_data <- data.frame("ID" = dat_ID,
                         "date_start" = in_data$date_start, 
                         "date_stop" = in_data$date_stop, 
                         "tr_type" = in_data$tr_type,
                         "number_inspections" = in_data$number_inspections,
                         "total_rides" = in_data$total_rides,
                         "faction" = in_data$number_inspections /in_data$total_rides,
                         "empirical_b" = 0)
  print("Current ID: ", out_data$ID)
  # Write the file to the local system
  write.table(out_data, file=fileName, append = TRUE, sep =",", col.names = FALSE)
}

loadData <- function(fileName) {
    # Read all the files into a list
    ldata <- read.csv(fileName, stringsAsFactors = FALSE, header = TRUE)
    return(ldata)
}


#Upfront data fetching from working directory
data_filename <- "data/conductor.csv"


if (dir.exists("data")) {
  if (!file.exists("data/conductor.rds")){
    cdata <- data.frame("ID", "date_start", "date_stop", "tr_type", "number_inspections", "total_rides", "fraction", "empirical_b")
    write.csv(cdata, file=data_filename)
  }
} else {
  dir.create("data")
  cdata <- data.frame("ID", "date_start", "date_stop", "tr_type", "number_inspections", "total_rides", "fraction", "empirical_b")
  write.csv(cdata, file=data_filename)
}

# Define the fields we want to save from the form - deprecated
#fields <- c("date_start","date_stop", "tr_type", "total_rides", "number_inspections")

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Bayesian Conductor"),
   # Sidebar with 
   sidebarLayout(
      sidebarPanel(
        h4("Insert your data"),
        p("Create an individual data record where you enter your experienced ticket inspections over
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
                plotOutput("dat_plot"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    newdata <- list(date_start= as.Date(input$date_range[1], origin = "1970-01-01"), 
                    date_stop= as.Date(input$date_range[2], origin = "1970-01-01"), 
                    tr_type= as.integer(input$tr_type), 
                    number_inspections= input$number_inspections, 
                    total_rides= input$total_rides)
    newdata
  })

  values <- reactiveValues(counter = 0, current_dat = NULL)
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$Submit, {
    values$counter <- values$counter + 1
    saveData(formData(), fileName = data_filename, dat_ID= values$counter)
    current_dat <- as.data.frame(loadData(fileName = data_filename))
  })
  
  output$type_selected <- renderText({paste("You have selected type: ", input$tr_type)})
  #output$date_range_sel <- renderText({paste(as.character(input$date_range), collapse=" to: ")})
  output$date_range_sel <- renderText({paste("dates from: ", as.Date(input$date_range[1], origin = "1970-01-01"), " until: ", as.Date(input$date_range[2], origin = "1970-01-01"))})
  output$total_rides <- renderText({paste("total number of train rides:", input$total_rides)})
  output$total_inspections <- renderText({paste("total number of ticket inspections: ", input$number_inspections)})
  #render Table
  output$responses <- DT::renderDataTable({
    input$Submit
    loadData(fileName = data_filename)
    })     
}

# Run the application 
shinyApp(ui = ui, server = server)

