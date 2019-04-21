# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(ggplot2)
library(DT)
library(gridExtra)
library(MASS)

#####
# My functions for this app #

save_data <- function(in_data, current_data, fileName, dat_ID) {
  out_data <- data.frame("ID" = dat_ID,
                         "date_start" = in_data$date_start, 
                         "date_stop" = in_data$date_stop, 
                         "tr_type" = in_data$tr_type,
                         "number_inspections" = in_data$number_inspections,
                         "total_rides" = in_data$total_rides,
                         "fraction" = in_data$number_inspections/
                                      in_data$total_rides,
                         "empirical_b" = update_bayesian(in_data$tr_type, 
                         in_data$number_inspections, in_data$total_rides))
  #combine the data frames
  if (!is.null(current_data)){
    latest_data <- rbind(current_data, out_data)
    write.csv(latest_data, fileName, row.names = FALSE)
  } else {
    # print("First save")
    # Write the file to the local system
    write.csv(out_data, fileName, row.names = FALSE)
  }
}

load_data <- function(fileName) {
    if (file.exists(fileName)){
      ldata <- as.data.frame(read.csv(fileName, 
                                      stringsAsFactors = FALSE, 
                                      header = TRUE))
      #recover correct date time
      ldata$date_start <- as.Date(ldata$date_start, origin = "1970-01-01")
      ldata$date_stop <- as.Date(ldata$date_stop, origin = "1970-01-01")
      return(ldata)
    } else {
      cat("No file: ", fileName)
    }
}

update_bayesian <- function(tr_type, sum_inspections, sum_total_rides){
  # take in data from data frame, 
  # calculate posterior expected value while using train specific priors
  # scale is == 1
  # TODO for later: rewrite this more elegantly 
  prior_alpha <- 0; prior_beta <- 0
  if (tr_type[1]==1){
    prior_alpha <- 15
    prior_beta <- 2
  }
  if (tr_type[1]==2){
    prior_alpha <- 8
    prior_beta <- 4
  }
  if (tr_type[1]==3){
    prior_alpha <- 4
    prior_beta <- 4
  }
  updated_alpha <- sum_inspections+prior_alpha
  updated_beta <- sum_total_rides+prior_beta-sum_inspections
  # calculate posterior expectation
  post_exp <- updated_alpha / (updated_alpha + updated_beta)
  # return posterior expectation
  return(post_exp)
}

#####

#intial working directory setup
data_filename <- "data/conductor.csv"
if (!dir.exists("data")) {
  dir.create("data")
}

#####

##### 
#define UI for application 
ui <- fluidPage(
   # application title
   titlePanel("Bayesian Conductor"),
   # sidebar with input fields and info text
   sidebarLayout(
      sidebarPanel(
        h4("Insert your data"),
        p("Create an individual data record where you enter your 
          experienced ticket inspections during
          your total number of train rides."),
        selectInput("tr_type", 
                    label="Select the train type you used: ", 
                    choices = list("ICE" = 1, "RE/RB" = 2,
                                   "S-Bahn" = 3), selected = 1),
        helpText("Note: Please provide a date range during which 
                 you took train rides,",
                 "a count of ticket inspections you experienced (see below),",
                 "and the total number of train rides you took during the 
                 reported date range."),
        dateRangeInput("date_range", h4("Date range")),
        sliderInput("total_rides", h4("Total number of train rides"),
                    min = 0, max = 100, value = 5),
        sliderInput("number_inspections", "Total number of ticket inspections 
                    for these rides",
                    min = 0, max = 100, value = 3),
        actionButton("Submit", "Submit your data"),
        br(),
        textOutput("user_message")
      ),
      
      mainPanel(
              tabsetPanel(
                
                tabPanel("Data table", 
                         DT::dataTableOutput("responses", width = 300), 
                         plotOutput("est_dbeta"), tags$hr(), 
                         position="above"), 
                
                tabPanel("Direct Estimate Plots", 
                         plotOutput("dat_dir_plot")), 
                
                tabPanel("Durable Estimate Plots", 
                         htmlOutput("warning"), 
                         plotOutput("dat_dur_plot")), 
                
                tabPanel("Summary / Maths of Estimation", 
                         verbatimTextOutput("summary"))
                
              )
            )
   )
)


#####
#define server logic
server <- function(input, output) {
  
  # set up reactive values, i.e. dynamic values
  values <- reactiveValues(counter = 0, 
                           current_dat = NULL)
  
  # whenever a field is filled, aggregate all form data
  formData <- reactive({
    newdata <- list(date_start= as.Date(input$date_range[1], 
                                        origin = "1970-01-01"), 
                    date_stop= as.Date(input$date_range[2], 
                                       origin = "1970-01-01"), 
                    tr_type= as.integer(input$tr_type), 
                    number_inspections= input$number_inspections, 
                    total_rides= input$total_rides)
    newdata #return
  })

  # when the Submit button is clicked, save the form data & put out a message
  observeEvent(input$Submit, {
    values$counter <- values$counter + 1
    save_data(formData(), 
              current_data =  values$current_dat, 
              fileName = data_filename, 
              dat_ID = values$counter)
    #get latest data after saving
    values$current_dat <- load_data(fileName = data_filename)
    output$user_message <- renderText("Thank you for your data!")
  })
  
  #render table
  output$responses <- DT::renderDataTable({
    #dynamically input this new data to the reactive environment
    values$current_dat <- load_data(data_filename)
    #load latest ID from data 
    if (is.null(values$current_dat)==FALSE){
      if (values$current_dat$ID[nrow(values$current_dat)]>1){
      values$counter <- values$current_dat$ID[nrow(values$current_dat)]
      }
    }
    #render current data
    input$Submit
    load_data(fileName = data_filename)
    })   
  
  #render plot of current overall posterior risk per type of train
  output$dat_dir_plot <- renderPlot({
    if (is.null(values$current_dat)==FALSE){
      if (values$counter>1){
        
        #collapse current data by train type into new data frame for plotting
        bayes_overall_dat <- aggregate(
                              values$current_dat[, names(values$current_dat) 
                              %in% 
                              c("number_inspections", "total_rides")], 
                              list(tr_type = values$current_dat$tr_type), 
                              sum)
        
        #recalculate the overall fraction and insert variable empirical_b
        bayes_overall_dat$fraction <- bayes_overall_dat$number_inspections / 
                                      bayes_overall_dat$total_rides
        
        bayes_overall_dat$empirical_b <- update_bayesian(
                                         as.numeric(bayes_overall_dat$tr_type), 
                                         bayes_overall_dat$number_inspections, 
                                         bayes_overall_dat$total_rides)
        #value label the data
        bayes_overall_dat$tr_type <- factor(bayes_overall_dat$tr_type,
                                            levels = c(1,2,3),
                                            labels = c("ICE", "RE / RB", "Sbahn"))
        #generate plot
        emp_plot <- ggplot(data = bayes_overall_dat, 
                           aes(x=tr_type, y=fraction)) + 
                  geom_bar(stat="identity", color="blue", fill="white") + 
                  theme_minimal() +  
                  xlab("Train Types") +
                  ylab("Risk of ticket inspection [%]") +
                  geom_text(aes(label=round(fraction, 2)), 
                            vjust=1.6, 
                            size=3.5, 
                            color="blue") +
                  ggtitle("Empirical risk of ticket inspection")
        
        post_plot <- ggplot(data = bayes_overall_dat, 
                            aes(x=tr_type, y=empirical_b)) + 
                geom_bar(stat="identity", color="red", fill="white") + 
                theme_minimal() +  
                xlab("Train Types") +
                geom_text(aes(label=round(empirical_b, 2)), 
                          vjust=1.6, 
                          size=3.5, 
                          color="red") + 
                ylab("Risk of ticket inspection [%]") + 
                ggtitle("Estimated risk of ticket inspection")
       
       #render plot
       grid.arrange(emp_plot, post_plot, nrow = 1)
      }
    }
  })
  
  #render plot of accumulated data if data per each train type >=500
  #render plot of current overall risk per type of train
  output$dat_dur_plot <- renderPlot({
    if (is.null(values$current_dat)==FALSE){
      if (values$counter>1){
        
        #collapse current data by train type into new data frame for plotting
        bayes_dur_dat <- aggregate(
                         values$current_dat[, names(values$current_dat) 
                         %in% 
                         c("number_inspections", "total_rides")], 
                         list(tr_type = values$current_dat$tr_type), 
                         sum)
        
        #recalculate the overall fraction and insert variable empirical_b
        bayes_dur_dat$fraction <- bayes_dur_dat$number_inspections / 
                                  bayes_dur_dat$total_rides
        
        bayes_dur_dat$empirical_b <- update_bayesian(
                                          as.numeric(bayes_dur_dat$tr_type), 
                                          bayes_dur_dat$number_inspections, 
                                          bayes_dur_dat$total_rides)
        
        #value label the data
        bayes_dur_dat$tr_type <- factor(bayes_dur_dat$tr_type,
                                            levels = c(1,2,3),
                                            labels = c("ICE", "RE / RB", "Sbahn"))
        
        if (sum(bayes_dur_dat$total_rides)>=1500){
          #generate plot
          emp_plot <- ggplot(data = bayes_overall_dat, 
                             aes(x=tr_type, y=fraction)) + 
            geom_bar(stat="identity", color="blue", fill="white") + 
            theme_minimal() +  
            xlab("Train Types") +
            ylab("Risk of ticket inspection [%]") +
            geom_text(aes(label=round(fraction, 2)), 
                      vjust=1.6, 
                      size=3.5, 
                      color="blue") +
            ggtitle("Empirical risk of ticket inspection")
          
          post_plot <- ggplot(data = bayes_overall_dat, 
                              aes(x=tr_type, y=empirical_b)) + 
            geom_bar(stat="identity", color="red", fill="white") + 
            theme_minimal() +  
            xlab("Train Types") +
            geom_text(aes(label=round(empirical_b, 2)), 
                      vjust=1.6, 
                      size=3.5, 
                      color="red") + 
            ylab("Risk of ticket inspection [%]") + 
            ggtitle("Estimated risk of ticket inspection")
          
          #render plot
          grid.arrange(emp_plot, post_plot, nrow = 1)
          
        } else {
          #if not enough data, output a message to user w. count of missing data
          output$warning <- renderUI({
            str1 <- "I cannot yet render a plot for you, 
                     insufficient data for realiable estimation!"
            str2 <- paste("(", 
                          (1500-sum(bayes_dur_dat$total_rides)),
                          " observations missing)")
            HTML(paste(str1, str2, sep = '<br/>'))
          })
        }
      }
    }
  })
}

#####
#Run the application 
shinyApp(ui = ui, server = server)