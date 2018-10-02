library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(lme4) # to build linear mixed model
library(lmerTest) # for outputing test results from the mixed model
library(plyr) #for merging data
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

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page


source('./FEV_functions.R')

GLOBAL_fev1_lmer_model <- NULL
GLOBAL_fev1_fvc_lmer_model <- NULL
GLOBAL_fev1_lmer_model_summary <- NULL
GLOBAL_fev1_lmer_model_loaded_FLAG <- NULL
GLOBAL_prediction_results_fev1<- NULL
GLOBAL_prediction_results_fev1_fvc <-  NULL

button_width <- 160

# fev1_lmer_function_output_summary <- NULL
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsResetCode, functions = c("reset")),                      # Add the js code to the page
  shinyjs::inlineCSS(appCSS),
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  titlePanel("Lung Function Predictor for General Population"),
  
  sidebarLayout(
    
    sidebarPanel(
      #helpText("Enter as many patient characteristics as possible.",
       #        "When all fields are completed, a validated model will make predictions.",
        #       "Fewer inputs will trigger the appropriate reduced model. See 'about' for more details."), 
      numericInput('fev1_0', labelMandatory('FEV1 (L)'), value = NULL, min = 1, max = 5, step = 0.25),
      numericInput('fvc_0', labelMandatory('FVC (L)'), value = NULL, min = 1, max = 10, step = 0.25),
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
            numericInput("cocktail","Liquor/spirits intake (drinks/wk)", value = NULL, min = 0, step = 1)
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
            numericInput("WBC","White blood cells (10^9/L)", value = NULL, min = 25, max = 172, step = 1),
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
            fileInput("load_inputs_button","Choose CSV File to Load", accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), buttonLabel = "Load Inputs")
        )                 
      ),
      
      uiOutput('inputParam'),
      
      br(),
      br(),
      shinyjs::hidden(
        div(id = "COPD_detected",
            HTML(paste(tags$span(style="color:red", "Airflow obstruction (FEV1/FVC<0.7). The individual already satisfies the definition of airflow obstruction. Please use Individualized FEV1 prediction in COPD app instead.")))
        )
      ),
      shinyjs::hidden(
        div(id = "FEV1_range",
            HTML(paste(tags$span(style="color:red", "FEV1 must be between 1L and 5L")))
        )
      ),
      shinyjs::hidden(
        div(id = "FVC_range",
            HTML(paste(tags$span(style="color:red", "fvc_0 is out of range")))
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
      shinyjs::hidden(
        div(id = "alcoholIntake",
            HTML(paste(tags$span(style="color:red", "Please enter either all or none of alcohol intake variables")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "trigpkyr",
            HTML(paste(tags$span(style="color:red", "Please enter frequency and history of smoking")))
        )
      ),
      checkboxInput("termsCheck",HTML(paste("I agree to ", tags$span(style="color:tomato", tags$a(href="./disclaimer.html", target="_blank", "terms")), sep = "")), FALSE),
      actionButton("submit", "Run the prediction model"),
      actionButton("reset_button", "Start over")
    ),
    
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("FEV1",
                           shinyjs::hidden(div(id = "checkbox_FEV1", 
                                               checkboxInput("if_quit_FEV1", "Compare with smoking cessation", value = FALSE, width = NULL),
                                               checkboxInput("CI_FEV1_comparison", "Confidence intervals for scenario comparison", value = FALSE, width = 600))),
                           div(id = "background", includeMarkdown("./background.rmd")),
                           plotlyOutput("plot_FEV1_decline"),
                           br(),
                           tableOutput("table_FEV1_decline")
                           
                  ),
                  
                  tabPanel("FEV1 % Predicted",
                           shinyjs::hidden(div(id = "checkbox_FEV1_percentpred", 
                                               checkboxInput("if_quit_FEV1_percentpred", "Compare with smoking cessation", value = FALSE, width = NULL),
                                               checkboxInput("CI_FEV1_percpred", "Confidence intervals for scenario comparison", value = FALSE, width = 600))),
                           plotlyOutput("plot_FEV1_percentpred"),
                           br(),
                           tableOutput("table_FEV1_percentpred")
                  ),
                  
                  tabPanel("COPD Risk",
                           shinyjs::hidden(div(id = "checkbox_COPD_risk", 
                                               checkboxInput("if_quit_COPD_risk", "Compare with smoking cessation", value = FALSE, width = NULL))),
                           checkboxInput("CI_COPD_risk", "Show Bernoulli Confidence Interval", value = FALSE, width = NULL),
                           plotlyOutput("COPD_risk"),
                           br(),
                           tableOutput("table_COPD_risk")
                           
                  ),
                  
                  # tabPanel("Model Summary",
                  #           verbatimTextOutput("lmer_summary")),
                  
                  tabPanel("Terms",  includeMarkdown("./disclaimer.rmd")),
                  tabPanel("About",  includeMarkdown("./about.rmd"), 
                           imageOutput("logos"))
                  #textOutput("binary")) #for debug; monitoring binary value. Amin
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
  errorLineColorSmoker <- "salmon"
  errorLineColorNonSmoker <- "darkcyan"
  lineColorSmoker <- "red"
  lineColorNonSmoker <- "dodgerblue4"
  
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
    alcoholInputTest <- as.numeric(is.na(input$beer) + is.na(input$wine) + is.na(input$cocktail))
    pkyr <- as.numeric(is.na(input$daily_cigs) + is.na(input$smoke_year))

    if (!input$termsCheck || is.na(input$fev1_0) || (input$fev1_0 == "") || is.na(input$fvc_0) || (input$fvc_0 == "") || is.na (input$age) || (input$age == "") || is.null (input$sex) || (input$sex == "") || is.na (input$height) || input$height == "" || (alcoholInputTest > 0 && alcoholInputTest < 3) || (!is.na(input$trig) && input$trig>0 && pkyr == 2) || (input$fev1_0/input$fvc_0) <= 0.7) {
      shinyjs::disable("submit")
    }else {
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
      if (!is.na(input$fvc_0) && (input$fvc_0!="")) {
        if ((input$fvc_0 < 1)  || (input$fvc_0 > 8))  {
          shinyjs::show (id = "FVC_range", anim = TRUE)}
        else {
          shinyjs::hide (id = "FVC_range", anim = TRUE)
        }
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
    if (!is.na(input$fev1_0) && (input$fev1_0!="") && !is.na(input$fvc_0) && (input$fvc_0!="")) {
      if ((input$fev1_0/input$fvc_0) <= 0.7)  {
        shinyjs::show (id = "COPD_detected", anim = TRUE)}
      else {
        shinyjs::hide (id = "COPD_detected", anim = TRUE)}
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
  
  observe({
    alcoholInputTest <- as.numeric(is.na(input$beer) + is.na(input$wine) + is.na(input$cocktail))
    if (alcoholInputTest > 0 && alcoholInputTest < 3) {
        shinyjs::show (id = "alcoholIntake", anim = TRUE)
      }
      else {
        shinyjs::hide (id = "alcoholIntake", anim = TRUE)
      }
        
  })  
  
  observe({
    pkyr <- input$daily_cigs + input$smoke_year
    if (!is.na(input$trig) && is.na(pkyr)) {
      shinyjs::show (id = "trigpkyr", anim = TRUE)

    }
    else {
      shinyjs::hide (id = "trigpkyr", anim = TRUE)

    }
    
  })  
  
  # Output Functions-----------------------------------------------------------------------------------------------------------

  output$logos <- renderImage({
    width  <- session$clientData$output_logos_width
    height <- session$clientData$output_logos_height
    # Return a list containing the filename
    list(src = "./logos2.png",
         contentType = 'image/png',
         width = width,
         alt = "This is alternate text")
  }, deleteFile = FALSE)


  #moved here to make it constanly recalculate binary code. Amin
  
  file_name <-  reactive(
    file_name <- BINARY_CODE_FROM_INPUTS(input$fev1_0,
                                         input$fvc_0,
                                         input$age,
                                         input$trig,
                                         input$hema,
                                         input$alb,
                                         input$glob,
                                         input$alk_phos,
                                         input$WBC,
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
  observeEvent(input$reset_button, {
    shinyjs::js$reset()
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
                                input$fvc_0,
                                input$trig,  #FEV_frame_num_values used to generate data frame column with numeric values only
                                input$hema,
                                input$alb,
                                input$glob,
                                input$alk_phos,
                                input$WBC,
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
                                 "NULL","NULL","NULL","NULL","NULL", "NULL",
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
    #req(GLOBAL_fev1_lmer_model_loaded_FLAG)
    
    predictors_data_frame <- predictors <- isolate(
      data.frame(
        input$fev1_0,
        input$fvc_0,
        input$age,
        input$trig,
        input$hema,
        input$alb,
        input$glob,
        input$alk_phos,
        input$WBC,
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
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.fvc_0')] <- 'fvc_0'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.age')] <- 'age'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.trig')] <- 'triglycerides'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.hema')] <- 'hematocrit'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.alb')] <- 'albumin'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.glob')] <- 'globulin'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.alk_phos')] <- 'ALP'
    colnames(predictors)[which(colnames(predictors_data_frame) == 'input.WBC')] <- 'WBC'
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
    
    GLOBAL_prediction_results_fev1<<- make_predictions('fev1', GLOBAL_fev1_lmer_model, predictors)
    GLOBAL_prediction_results_fev1_fvc <<- make_predictions('fev1_fvc', GLOBAL_fev1_fvc_lmer_model, predictors)
    
    #Next line: save output for unit test(comment out next line under normal operation)
    # write.csv(GLOBAL_prediction_results_fev1,file="./FEV_make_predictions_output.CSV")
    # write.csv(GLOBAL_prediction_results_fev1_fvc,file="./FEV1_FVC_make_predictions_output.CSV")
    
    
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
    if (input$if_quit_FEV1) {  
      shinyjs::show ("CI_FEV1_comparison")
      p <- ggplot(GLOBAL_prediction_results_fev1, aes(year)) + geom_line(aes(y = predicted_FEV1_if_smoke), color=lineColorSmoker, linetype=1) +
        geom_line(aes(y = predicted_FEV1_if_quit), color=lineColorNonSmoker, linetype=1) +
        
        #annotate("text", 1, 0.5, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
        #annotate("text", 1.15, 0.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
        labs(x=xlab, y=ylab) +
        theme_bw() 
      if (input$CI_FEV1_comparison) {
        p <- p + geom_ribbon(aes(ymin=FEV1_lowerbound_CI_if_smoke, ymax= upperbound_CI_if_smoke), linetype=2, alpha=0.1, fill=lineColorSmoker) +
          geom_line(aes(y = FEV1_lowerbound_CI_if_smoke), color=errorLineColorSmoker, linetype=2) +
          geom_line(aes(y = upperbound_CI_if_smoke), color=errorLineColorSmoker, linetype=2) +
          geom_ribbon(aes(ymin=FEV1_lowerbound_CI_if_quit, ymax= upperbound_CI_if_quit), linetype=2, alpha=0.1) +
          geom_line(aes(y = FEV1_lowerbound_CI_if_quit), color=errorLineColorNonSmoker, linetype=2) +
          geom_line(aes(y = upperbound_CI_if_quit), color=errorLineColorNonSmoker, linetype=2) 
      }
    } else {
      shinyjs::hide ("CI_FEV1_comparison")
      p <- ggplot(GLOBAL_prediction_results_fev1, aes(year)) + geom_line(aes(y = predicted_FEV1), color=lineColorSmoker, linetype=1) +
        geom_ribbon(aes(ymin=lowerbound_PI, ymax= upperbound_PI), linetype=2, alpha=0.1, fill=lineColorSmoker) +
        geom_line(aes(y = lowerbound_PI), color=errorLineColorSmoker, linetype=2) +
        geom_line(aes(y = upperbound_PI), color=errorLineColorSmoker, linetype=2) +
        #annotate("text", 1, 0.5, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
        #annotate("text", 1.15, 0.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
        labs(x=xlab, y=ylab) +
        theme_bw()
    }
    ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  FEV1_percent_pred_plot <- reactive ({
    
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
    
    if (input$if_quit_FEV1_percentpred) {
      shinyjs::show("CI_FEV1_percpred")
      p <- ggplot(GLOBAL_prediction_results_fev1, aes(year)) + geom_line(aes(y = percentpred_if_smoke), color=lineColorSmoker, linetype=1) +
        geom_line(aes(y = percentpred_if_quit), color=lineColorNonSmoker, linetype=1) +
        #annotate("text", 1, 25, label="Percent predicted FEV1", colour="black", size=4, hjust=0) +
        #annotate("text", 1.15, 15, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
        labs(x=xlab, y="FEV1 Percent predicted (%)") +
        theme_bw() 
      if (input$CI_FEV1_percpred) {
        p <- p + geom_ribbon(aes(ymin=percentpred_FEV1_lowerbound_CI_if_smoke, ymax= percentpred_upperbound_CI_if_smoke), linetype=2, alpha=0.1, fill=lineColorSmoker) +
          geom_line(aes(y = percentpred_FEV1_lowerbound_CI_if_smoke), color=errorLineColorSmoker, linetype=2) +
          geom_line(aes(y = percentpred_upperbound_CI_if_smoke), color=errorLineColorSmoker, linetype=2) +
          geom_ribbon(aes(ymin=percentpred_FEV1_lowerbound_CI_if_quit, ymax= percentpred_upperbound_CI_if_quit), linetype=2, alpha=0.1) +
          geom_line(aes(y = percentpred_FEV1_lowerbound_CI_if_quit), color=errorLineColorNonSmoker, linetype=2) +
          geom_line(aes(y = percentpred_upperbound_CI_if_quit), color=errorLineColorNonSmoker, linetype=2) 
      }
      
    } else {
      shinyjs::hide("CI_FEV1_percpred")
      p <- ggplot(GLOBAL_prediction_results_fev1, aes(year)) + geom_line(aes(y = percentpred), color=lineColorSmoker, linetype=1) +
        geom_ribbon(aes(ymin=percentpred_lowerbound_PI, ymax=percentpred_upperbound_PI), linetype=2, alpha=0.1, fill=lineColorSmoker) +
        geom_line(aes(y = percentpred_lowerbound_PI), color=errorLineColorSmoker, linetype=2) +
        geom_line(aes(y = percentpred_upperbound_PI), color=errorLineColorSmoker, linetype=2) +
        theme_bw() 
    }
    ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  
  COPD_risk_plot <- reactive ({
    
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
      title = "COPD Risk (probablity %)",
      titlefont = f
    )
    
    if (input$daily_cigs == 0 && !is.na(input$daily_cigs)) {
      p <- ggplot(GLOBAL_prediction_results_fev1_fvc, aes(year)) + geom_line(aes(y = COPD_risk*100), color=lineColorNonSmoker, linetype=1) +
        labs(x=xlab, y="COPD Risk (%)") +
        theme_bw() 
      
      if (input$CI_COPD_risk) {
        p <- p + geom_ribbon(aes(ymin = COPD_risk_lowerbound*100, ymax = COPD_risk_upperbound*100), linetype=2, alpha=0.1, fill=lineColorNonSmoker) +
          geom_line(aes(y = COPD_risk_lowerbound*100), color = errorLineColorNonSmoker, linetype=2) +
          geom_line(aes(y = COPD_risk_upperbound*100), color = errorLineColorNonSmoker, linetype=2) 
      }
    }
    else if (input$if_quit_COPD_risk) { 
      p <- ggplot(GLOBAL_prediction_results_fev1_fvc, aes(year)) + geom_line(aes(y = COPD_risk_if_smoke*100), color=lineColorSmoker, linetype=1) +
        geom_line(aes(y = COPD_risk_if_quit*100), color=lineColorNonSmoker, linetype=1) +
        labs(x=xlab, y="COPD Risk (%)") +
        theme_bw() 
      
      if (input$CI_COPD_risk) {
        p <- p + geom_ribbon(aes(ymin = COPD_risk_lowerbound_if_smoke*100, ymax = COPD_risk_upperbound_if_smoke*100), linetype=2, alpha=0.1,  fill=lineColorSmoker) +
          geom_line(aes(y = COPD_risk_lowerbound_if_smoke*100), color = errorLineColorSmoker, linetype=2) +
          geom_line(aes(y = COPD_risk_upperbound_if_smoke*100), color = errorLineColorSmoker, linetype=2) +
          geom_ribbon(aes(ymin = COPD_risk_lowerbound_if_quit*100, ymax = COPD_risk_upperbound_if_quit*100), linetype=2, alpha=0.1,  fill=lineColorNonSmoker) +
          geom_line(aes(y = COPD_risk_lowerbound_if_quit*100), color = errorLineColorNonSmoker, linetype=2) +
          geom_line(aes(y = COPD_risk_upperbound_if_quit*100), color = errorLineColorNonSmoker, linetype=2)
      }
    } else {
      
      p <- ggplot(GLOBAL_prediction_results_fev1_fvc, aes(year)) + geom_line(aes(y = COPD_risk_if_smoke*100), color=lineColorSmoker, linetype=1) +
        labs(x=xlab, y="COPD Risk (%)") +
        theme_bw() 
      
      if (input$CI_COPD_risk) {
        p <- p + geom_ribbon(aes(ymin = COPD_risk_lowerbound_if_smoke*100, ymax = COPD_risk_upperbound_if_smoke*100), linetype=2, alpha=0.1, fill=lineColorSmoker) +
          geom_line(aes(y = COPD_risk_lowerbound_if_smoke*100), color = errorLineColorSmoker, linetype=2) +
          geom_line(aes(y = COPD_risk_upperbound_if_smoke*100), color = errorLineColorSmoker, linetype=2) 
      }
    }
    
    ggplotly(p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    
  })
  
  observeEvent(input$submit, {
    
    shinyjs::hide("background")
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    
    #use collapse="" to get rid of spaces between 1s and 0s; use sep="" to get rid of space betweeen file name and ".rds"
    fev1_full_file_name = paste("./",paste(file_name(), collapse=""), "-fev1", ".rds",sep="")
    fev1_fvc_full_file_name = paste("./",paste(file_name(), collapse=""),"-fev1_fvc", ".rds",sep="")
    
    # browser()
    
    #if file exists but model has not been loaded, load the model from the file
    if(file.exists(fev1_full_file_name) && is.null(GLOBAL_fev1_lmer_model_loaded_FLAG)){
      progress$set(message = "Model found. Loading the model...", value = 0.50)
      fev1_lmer_function_output <- readRDS(fev1_full_file_name)
      fev1_fvc_lmer_function_output <- readRDS(fev1_fvc_full_file_name)
      
      
      #set the model-loaded-flag to TRUE
      GLOBAL_fev1_lmer_model_loaded_FLAG <<- TRUE
      progress$set(message = "Extracting model parameters", value = 0.80)
      GLOBAL_fev1_lmer_model <<- fev1_lmer_function_output 
      GLOBAL_fev1_fvc_lmer_model <<- fev1_fvc_lmer_function_output 
      
      #GLOBAL_fev1_lmer_model_summary <<- summary(fev1_lmer_function_output) #model summary is disabled for now
      progress$set(message = "Plotting", value = 0.9)
      
      #output$lmer_summary <- renderPrint({ GLOBAL_fev1_lmer_model_summary }) #model summary is disabled for now
      
    }
    #if file exists and model has been loaded, then get the model from GLOBAL variable
    # else if(file.exists(fev1_full_file_name) && !is.null(GLOBAL_fev1_lmer_model_loaded_FLAG)){
    #     output$lmer_summary <- renderPrint({GLOBAL_fev1_lmer_model_summary})
    # }
    
    #if file does not exist, then calculate lmer model and create file
    else if(!file.exists(fev1_full_file_name)){
      # browser()
      
      #BINARY_CODE_DATAFRAME
      BINARY_INPUT_NAMES <- c('fev1_0', 'fvc_0', 'age','trig','hema','alb','glob','alk_phos','WBC','qrs','beer','wine','cocktail','height','smoke_year','daily_cigs','sex','ba_use','dys_exer','noc_s')
      BINARY_CODE_DATAFRAME <- data.frame(file_name(), BINARY_INPUT_NAMES)
      #FACTOR_NAMES_DATAFRAME
      INPUTS <- c('fev1_0', 'fvc_0', 'age','trig','hema','alb','glob','alk_phos','WBC','qrs',
                  'beer', # ?calculated from beer,wine,cocktail consumption?
                  'wine','cocktail','height',
                  'smoke_year','daily_cigs',
                  'sex','ba_use','dys_exer','noc_s')
      EQUATION_FACTORS1 <- c('year', NA,
                             'age','triglycerides','hematocrit','albumin','globulin','ALP','WBC','QRS_intv',
                             'alcohol_indx','wine','cocktail',
                             'height2',
                             'cpackyr', NA,
                             'sex','broncho','dyspnea_exc','night_sym')
      EQUATION_FACTORS2 <- c('year2', NA, 
                             'agecat','triglycerides:year','hematocrit:year','albumin:year','globulin:year','ALP:year','WBC:year','QRS_intv:year',
                             'alcohol_indx:year','wine:year','cocktail:year',
                             'height2:sex', NA, NA,
                             'sex:year','broncho:year','dyspnea_exc:year','night_sym:year')
      EQUATION_FACTORS3 <- c('(year|RANDOMID)',#factors for fev1_0 unknown - ask Chen
                             NA, NA, 'triglycerides:cpackyr', NA, 'albumin:sex',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA)
      FACTORS_NAMES_DATAFRAME <- data.frame(INPUTS, EQUATION_FACTORS1, EQUATION_FACTORS2, EQUATION_FACTORS3)
      print (FACTORS_NAMES_DATAFRAME)
      progress$set(message = "Fitting new reduced model. This might take a few minutes", value = 0.30)
      
      #save data frames for unit tests - commented out under normal operation
      # saveRDS(BINARY_CODE_DATAFRAME,"FEV_calculate_lmer_fn_input1_BINARY_CODE_DATAFRAME.RDS")
      # saveRDS(FACTORS_NAMES_DATAFRAME,"FEV_calculate_lmer_fn_input2_FACTORS_NAMES_DATAFRAME.RDS")
      
      fev1_lmer_function_output <- FEV_calculate_lmer_fn("fev1", BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
      fev1_fvc_lmer_function_output <- FEV_calculate_lmer_fn("fev1_fvc", BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
      
      #set the model-loaded-flag to TRUE
      GLOBAL_fev1_lmer_model_loaded_FLAG <<- TRUE
      
      progress$set(message = "Saving the model", value = 0.60)
      saveRDS(fev1_lmer_function_output, file=fev1_full_file_name)
      saveRDS(fev1_fvc_lmer_function_output, file=fev1_fvc_full_file_name)
      
      
      progress$set(message = "Loading the model, this might take a few minutes", value = 0.80)
      GLOBAL_fev1_lmer_model <<- fev1_lmer_function_output 
      GLOBAL_fev1_fvc_lmer_model <<- fev1_fvc_lmer_function_output
      
      #GLOBAL_fev1_lmer_model_summary <<- summary(fev1_lmer_function_output)  #summary tab is disabled for now
      
      progress$set(message = "Plotting...", value = 0.90)
      
      #output$lmer_summary <- renderPrint({GLOBAL_fev1_lmer_model_summary}) #summary tab is disabled for now
      
    }
    output$plot_FEV1_decline <- renderPlotly({
      print (FEV1_plot())
    })
    
    output$table_FEV1_decline<-renderTable({
      # rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
      #                  "Coefficient of Variation (CV) (%)")
      # colnames(aa1)<- years
      
      return(GLOBAL_prediction_results_fev1)
    },
    include.rownames=T,
    caption="FEV1 Projections",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$plot_FEV1_percentpred <- renderPlotly({
      print (FEV1_percent_pred_plot())
    })
    
    
    output$table_FEV1_percentpred<-renderTable({
      # rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
      #                  "Coefficient of Variation (CV) (%)")
      # colnames(aa1)<- years
      
      return(GLOBAL_prediction_results_fev1)
    },
    include.rownames=T,
    caption="FEV1 Percent Predicted Projections",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$plot_FEV1_percentpred <- renderPlotly({
      print (FEV1_percent_pred_plot())
    })
    
    output$COPD_risk <- renderPlotly({
      print (COPD_risk_plot())
    })
    
    output$table_COPD_risk<-renderTable({
      return(GLOBAL_prediction_results_fev1_fvc)
    },
    include.rownames=T,
    caption="FEV1/FVC Prediction",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    #disabling inputs
    shinyjs::disable("fev1_0") 
    shinyjs::disable("fvc_0") 
    shinyjs::disable("age") 
    shinyjs::disable("sex")  
    shinyjs::disable("height") 
    shinyjs::disable("daily_cigs") 
    shinyjs::disable("smoke_year") 
    shinyjs::disable("beer") 
    shinyjs::disable("wine") 
    shinyjs::disable("cocktail") 
    shinyjs::disable("qrs") 
    shinyjs::disable("ba_use") 
    shinyjs::disable("dys_exer") 
    shinyjs::disable("noc_s") 
    shinyjs::disable("hema") 
    shinyjs::disable("trig") 
    shinyjs::disable("WBC") 
    shinyjs::disable("alb") 
    shinyjs::disable("glob") 
    shinyjs::disable("alk_phos") 
    shinyjs::disable("submit") 
    
    
    if (input$daily_cigs>0 && !is.na(input$daily_cigs)) {
      shinyjs::toggle("checkbox_FEV1_percentpred")
      shinyjs::toggle("checkbox_COPD_risk")
      shinyjs::toggle("checkbox_FEV1")
    }
    
    progress$set(message = "Done!", value = 1)
    
    
  }) 
  
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)
