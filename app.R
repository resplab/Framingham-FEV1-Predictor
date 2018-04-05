#moved width outside the mainPanel
#edited main panel, particularly the Help Tab - got rid of the text that was simply typed into R file main panel
#added two libraries - rmarkdown and knitr
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(lme4) # to build linear mixed model
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
#library(MuMIn)
#library(fBasics)
#library(ROCR)
#library(pROC)
library(data.table)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)

#options(shiny.error = browser) #debug, amin

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"


source('./FEV_functions.R')

GLOBAL_lmer_model <- NULL
GLOBAL_lmer_model_summary <- NULL
GLOBAL_lmer_model_loaded_FLAG <- NULL
button_width <- 160

# lmer_function_output_summary <- NULL
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Individualized Framingham Lung Function Decline Predictor"),

  sidebarLayout(

    sidebarPanel(
                 helpText("Enter as many patient characteristics as possible.",
                          "When all fields are completed, a validated model will make predictions.",
                          "Fewer inputs will trigger the appropriate reduced model. See 'about' for more details."), 
                 numericInput('fev1_0', labelMandatory('FEV1 at baseline (L)'), value = NULL, min = 1, max = 5, step = 0.25),
                 numericInput("age", labelMandatory("Age (year)"), value = NULL, min = 20, max = 100, step = 1),
                 selectInput("sex", labelMandatory("Gender"),list('','female', 'male'),selected = NULL),
                 numericInput("height", labelMandatory("Height (cm)"),value = NULL, min = 100, max = 250,  step = 0.1),
                 icon("glass"),
                 a(id = "toggleLifeStyle", "LifeStyle", href = "#"),
                 shinyjs::hidden(
                   div(id = "LifeStyle",
                       numericInput("daily_cigs","cigarettes per day", value = NULL, min = 0, step = 1),
                       numericInput("smoke_year","Years smoking", value = NULL, min = 0, max = 50, step = 1),
                       numericInput("beer","Beer intake (cans or bottles/wk)", value = NULL, min = 0, max = 50, step = 1),
                       numericInput("wine","Wine intake (glasses/wk)", value = NULL, min = 0,step = 1),
                       numericInput("cocktail","Cocktail intake (drinks/wk)", value = NULL, min = 0, step = 1)
                   )
                 ),
                 br(), icon("stethoscope"),
                 a(id = "toggleSymptomsTreatments", "Symptoms & Treatments", href = "#"),
                 shinyjs::hidden(
                   div(id = "SymptomsTreatments",
                       numericInput("qrs","QRS interval (0.01 sec)",value = NULL, min = 2, max = 20, step = 1),
                       selectInput("ba_use", "Bronchodilator or inhaler", list('','Current use', 'Former use', 'No use'), selected = ''),
                       selectInput("dys_exer", "Dyspnea on exertion", list('','Yes, on walking up stairs or other vigorous excercise','Yes, on rapid walking or other moderderate exercise','On any slight exertion','No'), selected = ''),
                       selectInput("noc_s","Nocturnal symptoms",list('','Yes', 'No'),selected = '')
                   )
                 ),
                
                 br(), icon("tint"),"  ",
                 a(id = "toggleBloodTest", "Blood Test", href = "#"),
                 shinyjs::hidden(
                   div(id = "BloodTest",
                       numericInput("hema","Hematocrit (%)",value = NULL, min = 25, max = 62, step = 1),
                       numericInput("white_bc","White blood cells (10^9/L)", value = NULL, min = 25, max = 172, step = 1),
                       numericInput("trig","Triglycerides (mg/dl)",value = NULL, min = 1, max = 2000, step = 1),
                       numericInput("alb","Albumin (g/L)",value = NULL, min = 10, max = 100, step = 1),
                       numericInput("glob","Globulin (g/L)",value = NULL, min = 1, max = 100, step = 1),
                       numericInput("alk_phos","Alkaline Phosphotase (IU/L)",value = NULL, min = 1, max = 200, step = 1)
                 )
                   ),
                   br(), br(), icon("floppy-o"),"  ",
                   a(id = "toggleSaveLoad", "Save/Load Inputs", href = "#"),
                   shinyjs::hidden(
                     div(id = "SaveLoad",
                         downloadButton("save_inputs_button", "Save Inputs"),
                         fileInput("load_inputs_button","Choose CSV File to Load", accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), buttonLabel = "Load Inputs...")
                     )                 
      ),

      uiOutput('inputParam'),
      
      br(),
      br(),
      shinyjs::hidden(
        div(id = "FEV1_range",
            HTML(paste(tags$span(style="color:red", "FEV1 must be between 1L and 5L")))
        )
      ),
      shinyjs::hidden(
        div(id = "age_range",
            HTML(paste(tags$span(style="color:red", "age must be between 20 and 100")))
        )
      ),
      shinyjs::hidden(
        div(id = "height_range",
            HTML(paste(tags$span(style="color:red", "height must be between 100cm and 250cm")))
        )
      ),
      shinyjs::hidden(
        div(id = "qrs_range",
            HTML(paste(tags$span(style="color:red", "QRS (0.01s) out of range ")))
        )
      ),
      actionButton("submit", "Run the prediction model"),
      actionButton("clear_inputs_button", "Reset")
      ),
     

    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("FEV1",
                           plotlyOutput("plot_FEV1_decline"),
                           br(),
                           tableOutput("table_FEV1_decline")
            
                           ),
                  
                  tabPanel("FEV1 % Predicted",
                           plotlyOutput("plot_FEV1_percentpred"),
                           br(),
                           tableOutput("table_FEV1_percentpred")
                  ),
                  
                  tabPanel("COPD Risk",
                           plotlyOutput("COPD_risk")
                  ),

                  # tabPanel("Model Summary",
                  #          verbatimTextOutput("lmer_summary")),
                  
                  tabPanel("Disclaimer",  includeMarkdown("./disclaimer.rmd")),
                  tabPanel("About",  includeMarkdown("./about.rmd"))
                  #textOutput("binary") #for debug; monitoring binary value. Amin
      )
    )
  )
)

server <- function(input, output, session) {
  

  # Output Function Constants-------------------------------------------------------------------------------------------------
  
  coverageInterval <- "95% coverage interval"
  xlab="Time (years)"
  ylab="FEV1 (L)"
  errorLineColor <- "darkcyan"
  buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
  

  inputOption <- c("baseline information", 
                    "Risk Factors",
                    "Symptoms & Treatments",
                    "Blood Test")
  # Shinyjs-----------------------------------------------------------------------------------------------------------
  

  shinyjs::onclick("toggleLifeStyle",
                   shinyjs::toggle(id = "LifeStyle", anim = TRUE))    
  
  shinyjs::onclick("toggleSymptomsTreatments",
                   shinyjs::toggle(id = "SymptomsTreatments", anim = TRUE))    
  
  shinyjs::onclick("toggleBloodTest",
                   shinyjs::toggle(id = "BloodTest", anim = TRUE)) 
  
  shinyjs::onclick("toggleSaveLoad",
                   shinyjs::toggle(id = "SaveLoad", anim = TRUE)) 
  
  observe({
    if (is.na(input$fev1_0) || (input$fev1_0 == "") || is.na (input$age) || (input$age == "") || (is.null (input$sex) || (input$sex == ""))|| is.na (input$height) || (input$height == "")) {
      shinyjs::disable("submit")
    }else{
      shinyjs::enable("submit")
    }
  })  
  
  observe({
    if (!is.na(input$fev1_0) && (input$fev1_0!="")) {
      if ((input$fev1_0 < 1)  || (input$fev1_0 > 5))  {
        shinyjs::show (id = "FEV1_range", anim = TRUE)}
      else shinyjs::hide (id = "FEV1_range", anim = TRUE)
    }
  })    
  
  observe({
    if (!is.na(input$age) && (input$age!="")) {
      if ((input$age < 20)  || (input$age > 100))  {
           shinyjs::show (id = "age_range", anim = TRUE)}
      else shinyjs::hide (id = "age_range", anim = TRUE)
    }
  })  
  
  observe({
    if (!is.na(input$height) && (input$height!="")) {
      if ((input$height < 100)  || (input$height > 250))  {
        shinyjs::show (id = "height_range", anim = TRUE)}
      else shinyjs::hide (id = "height_range", anim = TRUE)
    }
  })  
  
  
  observe({
    if (!is.na(input$qrs) && (input$qrs!="")) {
      if ((input$qrs < 2)  || (input$qrs > 20))  {
        shinyjs::show (id = "height_qrs", anim = TRUE)}
      else shinyjs::hide (id = "height_qrs", anim = TRUE)
    }
  })  
  
  # Output Functions-----------------------------------------------------------------------------------------------------------
  
  output$inputParam<-renderUI({
    
    
  })
  

  #moved here to make it constanly recalculate binary code. Amin
 
  file_name <-  reactive(
                  file_name <- BINARY_CODE_FROM_INPUTS(input$fev1_0,
                              input$age,
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
                              input$noc_s)
    )

  output$binary <- renderText({ 
    file_name() 
   # input$ba_use #for debug
  })  

  observeEvent(input$prev_input_cat2, {
   updateTabsetPanel(session, "category", selected = "panel1")
  })

  observeEvent(input$prev_input_cat3, {
    updateTabsetPanel(session, "category", selected = "panel2")
  })  
  
  observeEvent(input$prev_input_cat4, {
    updateTabsetPanel(session, "category", selected = "panel3")
  })
  
  observeEvent(input$next_input_cat1, {
    updateTabsetPanel(session, "category", selected = "panel2")
  })
  
  observeEvent(input$next_input_cat2, {
    updateTabsetPanel(session, "category", selected = "panel3")
  })
  
  observeEvent(input$next_input_cat3, {
    updateTabsetPanel(session, "category", selected = "panel4")
  })
    
  #Browse button - prompts user to select input values file and loads it into GUI
  observeEvent(input$load_inputs_button,{
    inFile <- input$load_inputs_button
    if (is.null(inFile))
      return(NULL)

    #load the data frame from the csv file
    loadedInputs <- read.csv(inFile$datapath)

    #from the loaded file - get numeric values(INDEX 1-15) for the numericInput inputs
    for (i in 1:(length(loadedInputs$FEV_input_names)-4)) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_num_vals[(i)]) )
    }
    #from the loaded file - get strings(index 16-20) for selectInput inputs
    for (i in (length(loadedInputs$FEV_input_names)-3):(length(loadedInputs$FEV_input_names))) {
      session$sendInputMessage(loadedInputs$FEV_input_names[i],  list(value = loadedInputs$FEV_input_char_vals[(i)]) )
    }
  })

  #'Clear Inputs' button - set all inputs to NULL
  observeEvent(input$clear_inputs_button, {
    session$reload()
  })


  #Save Inputs button - prompts user to save inputs to a csv file
  output$save_inputs_button <- downloadHandler(
    filename = function() {
      paste("FEV_input-", Sys.Date(), ".csv", sep = "")
    },

    content = function(file) {
      # browser()
      #labels - 1st column in the data frame
      FEV_frame_labels <- FEV_input_labels()
      FEV_frame_num_values <- c(input$fev1_0, # we need baseline FEV1 to do future prediction, if not entered, give options: 1.25, 2.25, 3.25, 4.25
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
                                -999,# input$ba_use
                                -999,# input$dys_exer
                                -999,# input$noc_s
                                -999# input$sex
      )

      FEV_frame_char_values <- c("NULL","NULL","NULL","NULL","NULL",
                                 "NULL","NULL","NULL","NULL","NULL",
                                 "NULL","NULL","NULL","NULL","NULL",
                                 input$ba_use,
                                 input$dys_exer,
                                 input$noc_s,
                                 input$sex
      )

      FEV_data_frame <- data.frame(FEV_input_names=FEV_frame_labels,
                                   FEV_input_num_vals=FEV_frame_num_values,
                                   FEV_input_char_vals = FEV_frame_char_values)
      write.csv(FEV_data_frame, file)
    }
  )
  
  FEV1_plot <- reactive ({
    #req(GLOBAL_lmer_model_loaded_FLAG)
    
    predictors_data_frame <- predictors <- isolate(
      data.frame(
        input$fev1_0,
        input$age,
        input$trig,
        input$hema,
        input$alb,
        input$glob,
        input$alk_phos,
        input$white_bc,
        input$qrs,
        input$beer,
        input$wine,
        input$cocktail,
        input$height,
        input$smoke_year,
        input$daily_cigs,
        input$sex,
        input$ba_use,
        input$dys_exer,
        input$noc_s
      )
    )
    
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.fev1_0')] <- 'fev1_0'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.age')] <- 'age'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.trig')] <- 'triglycerides'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.hema')] <- 'hematocrit'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.alb')] <- 'albumin'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.glob')] <- 'globulin'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.alk_phos')] <- 'ALP'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.white_bc')] <- 'WBC'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.qrs')] <- 'QRS_intv'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.beer')] <- 'beer'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.wine')] <- 'wine'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.cocktail')] <- 'cocktail'
    
    # STEP1: create calculate alcohol_indx vector in the predictors dataframe and calculate it from 'beer', 'wine', 'cocktail'
    predictors$alcohol_indx<-((predictors$beer*0.444+predictors$cocktail*0.570+predictors$wine*0.400)-3.681324783)/4.781456965
    
    
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.height')] <- 'height'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.smoke_year')] <- 'smoke_year'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.daily_cigs')] <- 'daily_cigs'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.sex')] <- 'sex'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.ba_use')] <- 'broncho'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.dys_exer')] <- 'dyspnea_exc'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.noc_s')] <- 'night_sym'
    
    # STEP2: remove 'beer' from the predictors dataframe
    predictors[,'beer'] <- NULL
    
    
    prediction_results <<- make_predictions(GLOBAL_lmer_model, predictors)
    
    
    
    #Next line: save output for unit test(comment out next line under normal operation)
    write.csv(prediction_results,file="./FEV_make_predictions_output.CSV")
    
    #create prediction_results_QuitSmoke dataframe for scenario #1 (user quits smoking today)
    prediction_results_QuitSmoke <- subset.data.frame(prediction_results, prediction_results$smoking == 0)
    
    #create prediction_results_ContinueSmoke dataframe for scenario #2 (user continues to smoke)
    prediction_results_ContinueSmoke <- subset.data.frame(prediction_results, prediction_results$smoking == 1)
    
    #create prediction_results_toPlot dataframe
    #if "smoke_year" and "daily_cigs" inputs are both NA, then use prediction_results_QuitSmoke dataframe
    #if either "smoke_year" or "daily_cigs" is not NA, then use prediction_results_ContinueSmoke
    if(is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_QuitSmoke}
    if(!is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_ContinueSmoke}
    
    # save(prediction_results,prediction_results_QuitSmoke,prediction_results_ContinueSmoke,prediction_results_toPlot,file="~/RStudio projects/20171228/prediction_data_frames.RData")
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Time (year)",
      titlefont = f
    )
    y <- list(
      title = "FEV1 (mL)",
      titlefont = f
    )
    
   ggplotly(ggplot(prediction_results_toPlot, aes(year, predicted_FEV1)) + geom_line(aes(y = predicted_FEV1), color="black", linetype=1) +
               geom_ribbon(aes(ymin=lowerbound, ymax= upperbound), linetype=2, alpha=0.1) +
               geom_line(aes(y = lowerbound), color=errorLineColor, linetype=2) +
               geom_line(aes(y = upperbound), color=errorLineColor, linetype=2) +
               #annotate("text", 1, 0.5, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
               #annotate("text", 1.15, 0.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
               labs(x=xlab, y=ylab) +
               theme_bw()) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
     
  FEV1_percent_pred_plot <- reactive ({

    #create prediction_results_QuitSmoke dataframe for scenario #1 (user quits smoking today)
    prediction_results_QuitSmoke <- subset.data.frame(prediction_results, prediction_results$smoking == 0)
    
    #create prediction_results_ContinueSmoke dataframe for scenario #2 (user continues to smoke)
    prediction_results_ContinueSmoke <- subset.data.frame(prediction_results, prediction_results$smoking == 1)
    
    #create prediction_results_toPlot dataframe
    #if "smoke_year" and "daily_cigs" inputs are both NA, then use prediction_results_QuitSmoke dataframe
    #if either "smoke_year" or "daily_cigs" is not NA, then use prediction_results_ContinueSmoke
    if(is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_QuitSmoke}
    if(!is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_ContinueSmoke}
    
    # save(prediction_results,prediction_results_QuitSmoke,prediction_results_ContinueSmoke,prediction_results_toPlot,file="~/RStudio projects/20171228/prediction_data_frames.RData")
    
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Time (year)",
      titlefont = f
    )
    y <- list(
      title = "Percent predicted FEV1 (%)",
      titlefont = f
    )
    
    ggplotly(ggplot(prediction_results_toPlot, aes(year, percentpred)) + geom_line(aes(y = percentpred), color="black", linetype=1) +
               geom_ribbon(aes(ymin=percentpred_lower, ymax= percentpred_upper), linetype=2, alpha=0.1) +
               geom_line(aes(y = percentpred_lower), color=errorLineColor, linetype=2) +
               geom_line(aes(y = percentpred_upper), color=errorLineColor, linetype=2) +
               #annotate("text", 1, 25, label="Percent predicted FEV1", colour="black", size=4, hjust=0) +
               #annotate("text", 1.15, 15, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
               labs(x=xlab, y="FEV1 Percent predicted (%)") +
               theme_bw()) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
     
     observeEvent(input$submit, {
      # Create a Progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())


      #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rds"
      full_file_name = paste("./",paste(file_name(),collapse=""),".rds",sep="")
      # browser()

      #if file exists but model has not been loaded, load the model from the file
      if(file.exists(full_file_name) && is.null(GLOBAL_lmer_model_loaded_FLAG)){
        progress$set(message = "Model found. Loading the model...", value = 0.50)
        lmer_function_output <- readRDS(full_file_name)

        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE
        progress$set(message = "Extracting model parameters", value = 0.80)
        GLOBAL_lmer_model <<- lmer_function_output #could also be after lines 178-181
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)
        progress$set(message = "Plotting", value = 1.0)
  
        output$lmer_summary <- renderPrint({ GLOBAL_lmer_model_summary })
        
      }
      #if file exists and model has been loaded, then get the model from GLOBAL variable
      else if(file.exists(full_file_name) && !is.null(GLOBAL_lmer_model_loaded_FLAG)){
          output$lmer_summary <- renderPrint({GLOBAL_lmer_model_summary})
      }
      #if file does not exist, then calculate lmer model and create file
      else if(!file.exists(full_file_name)){
        # browser()
        
        #BINARY_CODE_DATAFRAME
        BINARY_INPUT_NAMES <- c('fev1_0','age','trig','hema','alb','glob','alk_phos','white_bc','qrs','beer','wine','cocktail','height','smoke_year','daily_cigs','sex','ba_use','dys_exer','noc_s')
        BINARY_CODE_DATAFRAME <- data.frame(file_name(), BINARY_INPUT_NAMES)
        #FACTOR_NAMES_DATAFRAME
        INPUTS <- c('fev1_0','age','trig','hema','alb','glob','alk_phos','white_bc','qrs',
                    'beer', # ?calculated from beer,wine,cocktail consumption?
                    'wine','cocktail','height',
                    'smoke_year','daily_cigs',
                    'sex','ba_use','dys_exer','noc_s')
        EQUATION_FACTORS1 <- c('year',#factors for fev1_0 unknown - ask Chen
                               'age','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv',
                               'alcohol_indx','wine','cocktail',
                               'height2',
                               'cpackyr','cpackyr',
                               'sex','broncho','dyspnea_exc','night_sym')
        EQUATION_FACTORS2 <- c('year2',#factors for fev1_0 unknown - ask Chen
                               'agecat','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year',
                               'alcohol_indx:year','wine:year','cocktail:year',
                               'height2:sex',
                               NA,NA, #factors for 'smoke_year','daily_cigs'
                               'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
        EQUATION_FACTORS3 <- c('(year|RANDOMID)',#factors for fev1_0 unknown - ask Chen
                               NA,'triglycerides:cpackyr',NA,'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
        FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
        progress$set(message = "Fitting new reduced model. This might take a few minutes", value = 0.30)
        
        #save data frames for unit tests - commented out under normal operation
        # saveRDS(BINARY_CODE_DATAFRAME,"FEV_calculate_lmer_fn_input1_BINARY_CODE_DATAFRAME.RDS")
        # saveRDS(FACTORS_NAMES_DATAFRAME,"FEV_calculate_lmer_fn_input2_FACTORS_NAMES_DATAFRAME.RDS")
        
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)

        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE

        progress$set(message = "Saving the model", value = 0.60)
        saveRDS(lmer_function_output,file=full_file_name)
        
        progress$set(message = "Loading the model, this might take a few minutes", value = 0.80)
        GLOBAL_lmer_model <<- lmer_function_output #most important has to be after line 214
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)
        
        progress$set(message = "Plotting...", value = 0.90)
        
          output$lmer_summary <- renderPrint({GLOBAL_lmer_model_summary})
          progress$set(message = "Done!", value = 1)
          
      }
       output$plot_FEV1_decline <- renderPlotly({
         print (FEV1_plot())
       })
       
       output$table_FEV1_decline<-renderTable({
         # rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
         #                  "Coefficient of Variation (CV) (%)")
         # colnames(aa1)<- years
         
         return(prediction_results)
       },
       include.rownames=T,
       caption="FEV1 Heterogeneity",
       caption.placement = getOption("xtable.caption.placement", "top"))
       output$plot_FEV1_percentpred <- renderPlotly({
         print (FEV1_percent_pred_plot())
       })
   
     
     output$table_FEV1_percentpred<-renderTable({
       # rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
       #                  "Coefficient of Variation (CV) (%)")
       # colnames(aa1)<- years
       
       return(prediction_results)
     },
     include.rownames=T,
     caption="FEV1 Heterogeneity",
     caption.placement = getOption("xtable.caption.placement", "top"))
     output$plot_FEV1_percentpred <- renderPlotly({
       print (FEV1_percent_pred_plot())
     })
}) 
 
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)
