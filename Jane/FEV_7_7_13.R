#moved width outside the mainPanel
#edited main panel, particularly the Help Tab - got rid of the text that was simply typed into R file main panel
#now, able to upload information from bookdown document into the Help Tab. Pieces of code were added only into this file.
#added two libraries - rmarkdown and knitr
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
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
library(rmarkdown) #for bookdown file
library(bookdown)
library(knitr) #for bookdown file
library(htmltools)

#options(shiny.error = browser) #debug, amin


source('./FEV_functions.R')

GLOBAL_lmer_model <- NULL
GLOBAL_lmer_model_summary <- NULL
GLOBAL_lmer_model_loaded_FLAG <- NULL
button_width <- 160

# lmer_function_output_summary <- NULL
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Individualized Prediction of Adulthood Lung Function Decline"),

  sidebarLayout(
    # source('FEV_sidebarPanel.R')


    sidebarPanel(
      # button_width <- 160,
      fluidRow(
        column(12,
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("age","Age (year)", value = 60,min = 30,max = 90,step = 1,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("fev1_0","FEV1 at baseline (L) custom",value = NULL,min = 0,max = 250,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("trig","Triglycerides (mg/dl)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("hema","Hematocrit (%)",value = NULL,min = 0,max = 100,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("alb","Albumin (mg/L)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("glob","Globulin (g/L)",value = NULL,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("alk_phos","Alkaline Phosphotase",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("white_bc","White blood cells(10^9/L)",value = NULL,step = 0.01,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("qrs","QRS interval (0.01 sec)",value = NULL,step = 0.01,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("beer","beer intake (cans or bottles/wk)",value = NULL,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("wine","Wine intake (glasses/wk)",value = NULL,min = 0,step = 1,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("cocktail","Cocktail intake (drinks/wk)",value = NULL,min = 0,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("height","Height (cm)",value = NULL,min = 55,step = 1,width = button_width))),
                 column(5,div(style = "font-size: 12px;",numericInput("smoke_year","Years smoking",value = NULL,min = 0,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",numericInput("daily_cigs","cig. per day",value = NULL,min = 0,step = 1,width = button_width)))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",selectInput("sex","sex",list('','female', 'male'),selected = ''))),
                 column(5,div(style = "font-size: 12px;",selectInput("ba_use","Bronchodilator or aerosol",list('','Current use', 'Former use', 'No use'),selected = '')))),
               fluidRow(
                 column(5,div(style = "font-size: 12px;",selectInput("dys_exer","Dyspnea on exertion",list('','On rigorous exercise','On moderate exercise','On slight exertion','No dyspnea on ex.'),selected = ''))),
                 column(5,div(style = "font-size: 12px;",selectInput("noc_s","Nocturnal symptoms",list('','Yes', 'Maybe', 'No'),selected = '')))),

               #action buttons
               fluidRow(column(5, div(style = "font-size: 12px;",downloadButton("save_inputs_button", "Save Inputs")))),
               fluidRow(column(10,div(style = "font-size: 12px;",fileInput("load_inputs_button","Choose CSV File to Load",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),buttonLabel = "Load Inputs...")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("lmer_Submit_button", "Run Linear mixed-effects models")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("clear_inputs_button", "Clear Inputs")))),
               fluidRow(column(5, div(style = "font-size: 12px;",actionButton("plot_FEV1_button", "Plot FEV1 decline")))))
      ),
      width=4
    )
    ,

    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Summary",
                           selectInput(
                             "lmer_summary_DropDownBox",
                             "Model Summary Selection",
                             list(
                               'Entire Summary',
                               'Coefficients',
                               'Residuals',
                               'Formula'),
                             selected = 'Entire Summary'),
                           verbatimTextOutput("lmer_summary")),
                  tabPanel("Plots",
                           tags$p("Plot graph of linear regression:"),
                           plotlyOutput("plot_FEV1_decline")
                  ),
                  # tabPanel("Help",
                  #          uiOutput('markdown')
                  #                   ),
                  tabPanel("Documentation",
                           # helpText(   a("Click Here to open FEV Documentation",href="file:///Users/sasha/Documents/RStudio projects1/26_12_2017/FEV_7_7_12/UserDocumentation/_book/index.html",target="_blank")
                           helpText(   a("Click Here to open FEV Documentation",href="file:///Users/sasha/Documents/RStudio projects1/26_12_2017/FEV_7_7_14/UserDocumentation/_book/index.html")
                           )
                  ),
                  # tabPanel("Resources",
                  #          br(),
                  #          fluidRow(
                  #            column(6, "Resources for Clinician",
                  #                   fluidRow(column(6, div(style = "font-size: 12px;",actionButton("article_1", "Article 1")))),
                  #                   fluidRow(column(6, div(style = "font-size: 12px;",actionButton("article_2", "Article 2"))))
                  #            ),
                  #            column(6, "Resources for User")
                  #          )
                  #          ),
                  tabPanel("Disclaimer", 
                  textOutput("binary") #for debug; monitoring binary value. Amin
                  )
      )
    )
  )
)

server <- function(input, output, session) {

  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('help_tab.Rmd', quiet = TRUE)))
  })

  #moved here to make it constanly recalculate binary code. Amin
 
  file_name <-  reactive(
                            # file_name <- paste(
                            # as.integer(!is.na(input$fev1_0)),
                            # as.integer(!is.na(input$age)),
                            # as.integer(!is.na(input$trig)),
                            # as.integer(!is.na(input$hema)),
                            # as.integer(!is.na(input$alb)),
                            # as.integer(!is.na(input$glob)),
                            # as.integer(!is.na(input$alk_phos)),
                            # as.integer(!is.na(input$white_bc)),
                            # as.integer(!is.na(input$qrs)),
                            # as.integer(!is.na(input$beer)), #again, alcohol index is a derived variable (see prediction)
                            # as.integer(!is.na(input$wine)),
                            # as.integer(!is.na(input$cocktail)),
                            # as.integer(!is.na(input$height)),
                            # as.integer(!is.na(input$smoke_year)), #cum_smoke is a derived variable (see prediction)
                            # as.integer(!is.na(input$daily_cigs)),
                            # as.integer(input$sex != ''),
                            # as.integer(input$ba_use != ''),
                            # as.integer(input$dys_exer != ''),
                            # as.integer(input$noc_s != ''))
    
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
    GLOBAL_lmer_model_loaded_FLAG <- NULL
    FEV_input_IDs <- FEV_input_labels()
    for (i in 1:length(FEV_input_IDs)) {
      session$sendInputMessage(FEV_input_IDs[i],  list(value = NULL) )
    }
   
  })


  #Save Inputs button - prompts user to save inputs to a csv file
  output$save_inputs_button <- downloadHandler(
    filename = function() {
      paste("FEV_input", ".csv", sep = "")
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

  #FEV_na_inputs_check.R checks for every inputs if the value is na - NOTE: enable this when introducing reactive outputs
  # source('FEV_na_inputs_check.R')

  

  #make lmer summary non-reactive --> it is only calculated when the user presses "Run Linear mixed-effects models" button
     observeEvent(input$lmer_Submit_button, {


      # Create a Progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())



      #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rds"
      full_file_name = paste(paste(file_name(),collapse=""),".rds",sep="")
      # browser()

      #if file exists but model has not been loaded, load the model from the file
      if(file.exists(full_file_name) && is.null(GLOBAL_lmer_model_loaded_FLAG)){
        progress$set(message = "Extracting lmer summary from RDS File", value = 1.00)
        lmer_function_output <- readRDS(full_file_name)

        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE

        GLOBAL_lmer_model <<- lmer_function_output #could also be after lines 178-181
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)

        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          output$lmer_summary <- renderPrint({ GLOBAL_lmer_model_summary })
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          output$lmer_summary <- renderPrint({ coef(GLOBAL_lmer_model_summary) })
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          output$lmer_summary <- renderPrint({ resid(GLOBAL_lmer_model_summary) })
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          output$lmer_summary <- renderPrint({ formula(GLOBAL_lmer_model_summary)})
        }
      }
      #if file exists and model has been loaded, then get the model from GLOBAL variable
      else if(file.exists(full_file_name) && !is.null(GLOBAL_lmer_model_loaded_FLAG)){
        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          output$lmer_summary <- renderPrint({GLOBAL_lmer_model_summary})
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          output$lmer_summary <- renderPrint({coef(GLOBAL_lmer_model_summary)})
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          output$lmer_summary <- renderPrint({resid(GLOBAL_lmer_model_summary)})
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          output$lmer_summary <- renderPrint({formula(GLOBAL_lmer_model_summary)})
        }
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
        progress$set(message = "calculating lmer function", value = 1.00)
        
        #save data frames for unit tests - commented out under normal operation
        # saveRDS(BINARY_CODE_DATAFRAME,"FEV_calculate_lmer_fn_input1_BINARY_CODE_DATAFRAME.RDS")
        # saveRDS(FACTORS_NAMES_DATAFRAME,"FEV_calculate_lmer_fn_input2_FACTORS_NAMES_DATAFRAME.RDS")
        
        lmer_function_output <- FEV_calculate_lmer_fn(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)

        progress$set(message = "Extracting lmer summary", value = 1.00)
        # lmer_function_output_summary <- summary(lmer_function_output)

        #set the model-loaded-flag to TRUE
        GLOBAL_lmer_model_loaded_FLAG <<- TRUE

        progress$set(message = "Saving RDATA file w/ model and summary", value = 1.00)
        saveRDS(lmer_function_output,file=full_file_name)

        GLOBAL_lmer_model <<- lmer_function_output #most important has to be after line 214
        GLOBAL_lmer_model_summary <<- summary(lmer_function_output)

        if(input$lmer_summary_DropDownBox == 'Entire Summary'){
          output$lmer_summary <- renderPrint({GLOBAL_lmer_model_summary})
        }
        else if(input$lmer_summary_DropDownBox == 'Coefficients'){
          output$lmer_summary <- renderPrint({coef(GLOBAL_lmer_model_summary)})
        }
        else if(input$lmer_summary_DropDownBox == 'Residuals'){
          output$lmer_summary <- renderPrint({resid(GLOBAL_lmer_model_summary)})
        }
        else if(input$lmer_summary_DropDownBox == 'Formula'){
          output$lmer_summary <- renderPrint({formula(GLOBAL_lmer_model_summary)})
        }
      }
    }) 





    # output$plot_FEV1_decline <- renderPlot({
    output$plot_FEV1_decline <- renderPlotly({
      # Take a dependency on input$plot_FEV1_button
      if (input$plot_FEV1_button == 0) #important to include because otherwise executes the rest of code even though user has not yet pressed the button
        return()

      # browser() #NOTE: browser() does not seem to work inside renderPlotyly()
      # # Create a Progress object
      # progress <- shiny::Progress$new()
      # on.exit(progress$close())

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
      
      #Next 2 lines: save inputs for unit test(comment out next 2 lines under normal operation)
      # saveRDS(GLOBAL_lmer_model,"~/RStudio projects/20171229/FEV_make_predictions_input1_lmfin.RDS")
      # write.csv(predictors,"~/RStudio projects/20171229/FEV_make_predictions_input2_predictors.CSV")

      prediction_results <- make_predictions(GLOBAL_lmer_model, predictors)
      
      
      
      #Next line: save output for unit test(comment out next line under normal operation)
       write.csv(prediction_results,file="./FEV_make_predictions_output.CSV")

      #create prediction_results_QuitSmoke dataframe for scenario #1 (user quits smoking today)
      prediction_results_QuitSmoke <- subset.data.frame(prediction_results, prediction_results$SmokeStatus == 0)
      
      #create prediction_results_ContinueSmoke dataframe for scenario #2 (user continues to smoke)
      prediction_results_ContinueSmoke <- subset.data.frame(prediction_results, prediction_results$SmokeStatus == 1)
      
      #create prediction_results_toPlot dataframe
      #if "smoke_year" and "daily_cigs" inputs are both NA, then use prediction_results_QuitSmoke dataframe
      #if either "smoke_year" or "daily_cigs" is not NA, then use prediction_results_ContinueSmoke
      if(is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_QuitSmoke}
      if(!is.na(input$daily_cigs)) {prediction_results_toPlot <- prediction_results_ContinueSmoke}
      
      # save(prediction_results,prediction_results_QuitSmoke,prediction_results_ContinueSmoke,prediction_results_toPlot,file="~/RStudio projects/20171228/prediction_data_frames.RData")
      
        ################PLOTLY CODE############################
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
      plot_ly(prediction_results_toPlot, x = ~year) %>%
        #add_markers(y = ~pred3) %>%
        add_lines(y= ~pred3, name = "FEV1 decline") %>%
        # add_markers(y = ~lower3) %>%
        # add_lines(y= ~lower3) %>%
        # add_markers(y = ~upper3) %>%
        # add_lines(y= ~upper3) %>%
        add_ribbons(x = ~year, ymin = prediction_results_toPlot$lower3, ymax = prediction_results_toPlot$upper3,      #responsible for 95% CI
                    color = I("red"), name = "95% confidence") %>%
        layout(title = "Individualized Prediction of Adulthood Lung Function Decline", xaxis = x, yaxis = y)


        ################END OF PLOTLY CODE########################

    })
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)
