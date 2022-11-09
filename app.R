# ===================================================================
# Title: BAPTA: Action Potential Batch Analyser
#
# Purpose: This script allows the automated analysis of APs from adult, neonatal and hiPSC-derived cardiomyocytes.
# Author: Luca Sala, PhD
# ===================================================================


require(shiny)

APD_values_input <- c(5,10,20,30,40,50,60,70,75,80,85,90,95)
saving_all_or_SS_input <- c("SS", "All")
data_pattern_input <- c(".abf", ".csv", ".txt")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
  
    mainPanel(

    titlePanel(title=div(img(src="20220908_AP_logo.jpg", align = "left", width = 587, height = 200),
                         "Choose your variables")),

    # Input: Select quotes ----
        radioButtons(inputId = "type_of_recording", 
                     label = "Were the cells paced or spontaneously beating?", 
                     choices = c("Spontaneously Beating" = "run_GF", 
                                 "Paced" = "run_TR")),
        checkboxGroupInput("APD_values", 
                           "Which Action Potential Durations you want? (APD90 is mandatory)", 
                           APD_values_input, 
                           selected = 90,
                           ),
        numericInput("sweeps", "How many sweeps you want to average for steady state? (Default = 5)", value = 5, min = 1, max = 1000),
    
        numericInput("sweeps_SD", "How many sweeps you want to average for SD1 and SD2 calculations? (Default = 30)", value = 30, min = 1, max = 1000),
        
        p("Only for Spontaneously Beating", style ="color:red; font-size: 150%"),

        radioButtons(inputId = "data_pattern", 
                       label = "Chose files format", 
                       choices = data_pattern_input),
        
        numericInput("minpeakheight", "What is the minimum voltage threshold for automatic peak detection? (Default = -10 mV)", value = -10, min = -100, max = 100),
        
        radioButtons(inputId = "saving_all_or_SS", 
                     label = "Do you want to save/average all data or only the APs at the steady state?", 
                     choices = saving_all_or_SS_input),    
        
        p("Only for non .abf files", style ="color:red; font-size: 100%"),
        p("Fiels should contain 2 columns in following order: 1. Time (s or ms) 2. Voltage (mV)"),
        
        radioButtons(inputId = "sep", 
                 label = "What the field separator character?", 
                 choices = c("Space"="", "Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Dot" =".")),
        
        radioButtons(inputId = "dec", 
                 label = "What the character used in the file for decimal points?", 
                 choices = c("Dot" =".", "Comma" = ",")),
        
        radioButtons(inputId = "time_parametr", 
                       label = "Time in seconds or miliseconds?", 
                       choices = c("Seconds" = 1000, "Miliseconds" = 1)),
        
        numericInput("si", "What is the sampling interval in ms? (Default = 0.05 ms)", value = 0.05),
    
    actionButton("choice", "Run!",  class = "btn-success btn-lg"),
    actionButton("stop", "Stop", class = "btn-danger btn-lg")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$choice, {
               if(input$type_of_recording == "run_GF"){
                   showModal(modalDialog(
                     title = "Analysis Concluded 😃",
                     easyClose = TRUE,
                     footer = NULL,
                     APD_values <<- as.numeric(input$APD_values),
                     sweeps <<- input$sweeps,
                     sweeps_SD <<- input$sweeps_SD,
                     data_pattern <<- input$data_pattern,
                     minpeakheight <<- input$minpeakheight,
                     saving_all_or_SS <<- input$saving_all_or_SS,
                     sep <<- input$sep,
                     dec <<- input$dec,
                     time_parametr <<- input$time_parametr,
                     si <<- input$si,
                     source("scripts/AP_Gap_Free_Analysis.R")
                   ))
               } else if(input$type_of_recording == "run_TR"){
                   showModal(modalDialog(
                     title = "Analysis Concluded 😃",
                     easyClose = TRUE,
                     footer = NULL,
                     APD_values <<- as.numeric(input$APD_values),
                     sweeps <<- input$sweeps,
                     sweeps_SD <<- input$sweeps_SD,
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
