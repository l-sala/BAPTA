# ===================================================================
# Title: BAPTA: Action Potential Batch Analyser
#
# Purpose: This script allows the automated analysis of APs from adult, neonatal and hiPSC-derived cardiomyocytes.
# Author: Luca Sala, PhD
# ===================================================================

if (require(shiny) == F) {
  installed.packages("shiny")
  require(shiny)
} 

APD_values_input <- c(5,10,20,30,40,50,60,70,75,80,85,90,95)
saving_all_or_SS_input <- c("SS", "All")
data_pattern_input <- c(".abf", ".csv", ".txt")

#__Data examples__
Time_s <- c(0, 0.0002, 0.0004, 0.0006, "...")
Trace_1_mV  <- c(-73.36, -73.36,-73.46, -73.48, "...")
Trace_2_mV <- c(-76.53, -76.57, -76.53, -76.53, "...")
               
table_trigered <- data.frame(Time_s, Trace_1_mV , Trace_2_mV, Trace_n_mV = "...")
table_spontaneous <- data.frame(Time_s, "Trace_mV" = Trace_1_mV)
#___

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
  titlePanel(windowTitle = "BAPTA", 
             title=div(img(src="20220908_AP_logo.jpg", width = 587, height = 200))
    ),
 
    mainPanel(
    # Input: Select quotes ----
        radioButtons(inputId = "type_of_recording", 
                     label = "Were the cells paced or spontaneously beating?", 
                     choices = c("Spontaneously Beating" = "run_GF", 
                                 "Paced" = "run_TR"),
                     inline = TRUE,),
        
        checkboxGroupInput("APD_values", 
                           "Which Action Potential Durations you want? (APD90 is mandatory)", 
                           APD_values_input, 
                           selected = 90,
                           inline = TRUE,),
        
        radioButtons(inputId = "representatives", 
                 label = "Do you want to save representative traces?", 
                 choices = c("Yes" = T, 
                             "No" = F),
                 inline = TRUE,),
        
        numericInput("sweeps", "How many sweeps you want to average for steady state? (Default = 5)", 
                     value = 5, min = 1, max = 1000),
    
        numericInput("sweeps_SD", "How many sweeps you want to average for SD1 and SD2 calculations? (Default = 30)", 
                     value = 30, min = 1, max = 1000),
        
#        numericInput("high_pass", "Difene parametr of high pass filter? (Default = 70)", value = 70),
        
#        numericInput("low_pass", "Difene parametr of low pass filter? (Default = -100)", value = -100),
        
        selectInput(inputId = "data_pattern",
                    label = "Chose files format",
                    choices = data_pattern_input),
        
        conditionalPanel(condition = "input.data_pattern != '.abf'",
          selectInput(inputId = "time_parametr", 
                      label = "Time in seconds or miliseconds?",
                      choices = c("Seconds" = 1000, "Miliseconds" = 1)),
            
          conditionalPanel(condition = "input.type_of_recording == 'run_TR'",
            p("Files should contain more then 2 columns in following order:" ),
            p("Time (s or ms); Voltage 1 (mV);  Voltage 2 (mV); etc." ),
            fluidRow(
              column(12, tableOutput('table_TR'))
            ),
          ),
            
          conditionalPanel( condition = "input.type_of_recording == 'run_GF'",
            p("Files should contain 2 columns in following order:"),
            p("Time (s or ms); Voltage (mV)"),
            fluidRow(
              column(12, tableOutput('table_GF'))
            ),  
          ),
        ),
        
        conditionalPanel( condition = "input.type_of_recording == 'run_GF'",    
         numericInput("minpeakheight", "What is the minimum voltage threshold for automatic peak detection? (Default = -10 mV)", value = -10, min = -100, max = 100),
            radioButtons(inputId = "saving_all_or_SS", 
                        label = "Do you want to save/average all data or only the APs at the steady state?", 
                        choices = saving_all_or_SS_input,
                        inline = TRUE),
        ),
        
    actionButton("choice", "Run!",  class = "btn-success btn-lg"),
    actionButton("stop", "Stop", class = "btn-danger btn-lg")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$table_GF <- renderTable(table_spontaneous)
  output$table_TR <- renderTable(table_trigered)
  observeEvent(input$choice, {
               if(input$type_of_recording == "run_GF"){
                 showModal(modalDialog(
                     title = "Analysis Concluded",
                     easyClose = TRUE,
                     footer = NULL,
                     type_of_recording <<- input$type_of_recording,
                     APD_values <<- as.numeric(input$APD_values),
                     sweeps <<- input$sweeps,
                     sweeps_SD <<- input$sweeps_SD,
                     data_pattern <<- input$data_pattern,
                     minpeakheight <<- input$minpeakheight,
                     saving_all_or_SS <<- input$saving_all_or_SS,
                     representatives <<- input$representatives,
#                     high_pass <<- input$high_pass,
#                     low_pass <<- input$low_pass,
                     time_parametr <<- input$time_parametr,
                     source("scripts/AP_Gap_Free_Analysis.R"),
                   ))
                } else if(input$type_of_recording == "run_TR"){
                   
                   showModal(modalDialog(
                     title = "Analysis Concluded",
                     easyClose = TRUE,
                     footer = NULL,
                     type_of_recording <<- input$type_of_recording,
                     data_pattern <<- input$data_pattern,
                     APD_values <<- as.numeric(input$APD_values),
                     sweeps <<- input$sweeps,
                     sweeps_SD <<- input$sweeps_SD,
                     time_parametr <<- input$time_parametr,
                     representatives <<- input$representatives,
                     source("scripts/AP_Batch_Analysis.R")
                   ))
               }
  })
  
  observeEvent(input$stop, {
    stopApp(session$onSessionEnded(stopApp))
  })
  observe({
    if (input$stop > 0) stopApp()                             # stop shiny
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
