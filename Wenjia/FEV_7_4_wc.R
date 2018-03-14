library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyjs)
library(lme4) # to build linear mixed model  
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
library(MuMIn)
library(fBasics)
library(ROCR)
library(pROC)
library(ipw)
library(data.table)

setwd("~/Documents/RStudio projects1/20171111/")
source('FEV_functions.R')

ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),
  
  sidebarLayout(
    source('FEV_sidebarPanel.R'), #load left sidebar Panel that has the inputs
    mainPanel (
      tags$p("lmer summary:"),
      verbatimTextOutput("lmer_summary"),
      width = 5, class = 'rightAlign'
    )
  )
)

server <- function(input, output, session) {

  #wait for user to press the Browse button - then load user input values into GUI
  observeEvent(input$file1,{
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)

    #load the data frame from the csv file
    loadedInputs <- read.csv(inFile$datapath)

    #from the loaded file - get numeric values(INDEX 1-14) for the numericInput inputs
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #from the loaded file - get strings(index 15-18) for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })
  
  #when user presses 'Clear Inputs' button - set all inputs to NULL
  observeEvent(input$clear_inputs, {
    FEV_input_IDs <- FEV_input_labels()
    for (i in 1:length(FEV_input_IDs)) {
      session$sendInputMessage(FEV_input_IDs[i],  list(value = NULL) )
    }
  })
  

  #when user presses Save Inputs button - save user inputs to a csv file
  output$save_inputs <- downloadHandler(
    filename = function() {
      paste("FEV_input", ".csv", sep = "")
    },
    content = function(file) {
      ###########################################################################################
      #labels - 1st column in the data frame
      FEV_frame_labels <- FEV_input_labels() #use FEV_input_labels() function to create an array of labels
      #numerical values - 2nd column in the data frame
    
      
      #######################NOTE!!!!!!!!!!!!!!###########################################
      # We need to center & scale predictors as well (remember we need FEV1 at baseline) #
      # I provide centering algorithm here, but you could center them later as well      #
      ####################################################################################
        FEV_frame_num_values <- c(
                                input$fev1_0, # we need baseline FEV1 to do future prediction, if not entered, give options: 1.25, 2.25, 3.25, 4.25
                                input$trig,  #FEV_frame_num_values used to generate data frame column with numeric values only
                                input$hema,
                                input$alb,
                                input$glob,
                                input$alk_phos, 
                                input$white_bc,
                                input$qrs,
                                input$beer, #alcohol is a derived variable from beer, wine and highballs 
                                input$wine,
                                input$cocktail,
                                input$height,
                                input$smoke_year, #cum_smoke is a derived variable from smoke years and daily cigs
                                input$daily_cigs,
                                input$age,
                                # input$follow_up_baseline,
                                -999,# input$ba_use
                                -999,# input$dys_exer
                                -999,# input$noc_s
                                -999# input$sex
      )
      
      #non-numerical/character inputs - 3rd column in the data frame
      FEV_frame_char_values <- c("NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL",
                                 "NULL","NULL",
                                 # "NULL",
                                 input$ba_use,
                                 input$dys_exer,
                                 input$noc_s,
                                 input$sex
      )
      # FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels, FEV_input_vals=FEV_frame_values)
      FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                   FEV_input_num_vals=FEV_frame_num_values,
                                   FEV_input_char_vals = FEV_frame_char_values)
      ###########################################################################################
      write.csv(FEV_data_frame, file)
    }
  )

  #run code, that for every input checks if the value is na - NOTE: enable this when we put include reactive outputs
  # source('FEV_na_inputs_check.R')

  #make lmer summary non-reactive --> it is only calculated when the user presses "Run Linear mixed-effects models" button
  output$lmer_summary <- renderPrint({
    if (input$lmer_Submit_button == 0)
       return()
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    isolate({
      file_name=BINARY_CODE_FROM_INPUTS(input$fev1_0,
                                        input$age,
                                        # input$follow_up_baseline,
                                        input$trig,
                                        input$hema, 
                                        input$alb,
                                        input$glob,
                                        input$alk_phos,
                                        input$white_bc,
                                        input$qrs,
                                        input$beer, #again, alcohol index is a derived variable (see prediction)
                                        input$wine,
                                        input$cocktail,
                                        input$height,
                                        input$smoke_year, #cum_smoke is a derived variable (see prediction)
                                        input$daily_cigs, 
                                        input$sex,
                                        input$ba_use,
                                        input$dys_exer,
                                        input$noc_s
      )
      
      #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rdata"
      full_file_name = paste(paste(file_name,collapse=""),".rdata",sep="")

      #if RDATA file(for given inputs) exists, get lmer_summary from the rts file
      if(file.exists(full_file_name)){
        # ptm <- proc.time() #DEBUG - start timer to see how long it takes to load summary

        progress$set(message = "Extracting lmer summary from RDATA File", value = 1.00)
        load(full_file_name)
        summary_lmfin <- lmer_function_output_summary
        
        # file_exists_exec_time <- proc.time() - ptm
        # print('Execution time')
        # print(file_exists_exec_time) #DEBUG - print execution time
        
        summary_lmfin
      }
      else{ #If file does not exist, run model, save results and summary into .rdata file and 
        # ptm <- proc.time() #DEBUG - start timer to see how long it takes to run lmer, generate summary, and save file
        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('age',
                                # 'follow_up_baseline',
                                'trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
        BINARY_CODE_DATAFRAME <- data.frame(file_name, BINARY_INPUT_NAMES)
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('age',
                    # 'follow_up_baseline',
                    # NA,
                    'trig','hema','alb','glob','alk_phos','white_bc','qrs','alcohol','wine','cocktail','height','cum_smoke','sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('age',
                               # 'year',
                               'triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv','alcohol_indx','wine','cocktail','height2','cpackyr','sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('agecat',
                               # 'year2',
                               'triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year','alcohol_indx:year','wine:year','cocktail:year','height2:sex',NA,'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c(NA,
                               # '(year|RANDOMID)',
                               'triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)

        progress$set(message = "calculating lmer function", value = 1.00)
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)

        progress$set(message = "Extracting lmer summary", value = 1.00)
        lmer_function_output_summary <- summary(lmer_function_output)

        progress$set(message = "Saving RDATA file w/ model and summary", value = 1.00)
        save(lmer_function_output,lmer_function_output_summary,file=full_file_name)
        
        # file_exists_exec_time <- proc.time() - ptm
        # print(file_exists_exec_time) #DEBUG - print execution time
        lmer_function_output_summary
      }
    }) #end of isolate({...})
  }, width=400)#end of output$lmer_summary <- renderTable
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)