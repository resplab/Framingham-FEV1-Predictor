library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)
library(shinythemes)
#devtools::install_github("shiny", "rstudio")

source ('./FEV1_projection.R')
options(shiny.error = function() {
  stop("")
}) # removes red error message from Shiny webpage

# Define UI for dataset viewer application
ui <- fluidPage(theme = shinytheme("united"),
  tags$head(tags$style("#prob_decliner{color: black;
                        font-size: 16px;
           }"
                        )),
  titlePanel("Individualized FEV1 Predictor For Healthy Population"),

  sidebarLayout(


    sidebarPanel(

    selectInput('model', 'Please select your model', c("Basic model", "Minimal Model",
                                                       "Paper Model",
                                                       "Flexible Model - Enter as many characterisitcs as you wish")),

    uiOutput('inputParam'),

    br(),
    br()
    #submitButton("xx")
    ),


  # Show a summary of the dataset and an HTML table with the
  # requested number of observations. Note the use of the h4
  # function to provide an additional header above each output
  # section.
  mainPanel(

          tabsetPanel(
                  type = "tabs",
                  tabPanel("FEV1 Projection", plotlyOutput("figure"), br(), br(), textOutput("prob_decliner"),
                           br(), tableOutput("cv"), br(),
                           includeMarkdown("tablecaption.Rmd")),
                  tabPanel("GOLD Grade", br(), br(), plotlyOutput("severity"), tableOutput("sevTab")),
                  tabPanel("About",  includeMarkdown("about.Rmd"))
            )
  )

))



# Define server logic required to draw a histogram
server <- (function(input, output, session) {

  # Output Function Constants-------------------------------------------------------------------------------------------------

  buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
  interventionTitle <- paste0('If the patient is going to use a new intervention,\n',
                              'please indicate the effect of the new intervention\n',
                              'relative to his/her current therapy on initial\n',
                              'improvement in lung function (L).\n',
                              'If you only want to model the natural course of\n',
                              'disease progression irrespective of specific intervention,\n',
                              'please select 0 in here.')
  modelOptions <- c("Basic model", 
                    "Minimal Model",
                    "Paper Model",
                    "Flexible Model - Enter as many characterisitcs as you wish")
  years <- c('Year 1', 'Year 2', 'Year 3', 'Year 4', 'Year 5', 'Year 6', 'Year 7', 'Year 8',
            'Year 9', 'Year 10', 'Year 11')

  ######## Ouput Figure (FEV1 Projection Plot) -----------------------------------

  coverageInterval <- "95% coverage interval"
  xlab="Time (years)"
  ylab="FEV1 (L)"
  errorLineColor <- "darkcyan"

  # Output Functions-----------------------------------------------------------------------------------------------------------

	output$inputParam<-renderUI({

		if (input$model==modelOptions[1]) {

		  list(numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),

		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))

		} else if (input$model==modelOptions[2]){

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),
		       selectInput('sex', 'Sex', c('male', 'female')),

		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),

		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),

		       selectInput('smoking', 'Smoking status', c('Smoker', 'Sustained quitter')),

		       numericInput('fev1_0', 'FEV1 at baseline (L)', 2.75, min=1.25, max=3.55),

		       numericInput('oco', "O'Connor", -12.70, min=-300.00, max=2.00),

		       tags$div(title=interventionTitle,
		                numericInput('int_effect', 'Effect of Intervention on Lung Function (L)', 0, min=0, max=0.1)),
		       selectInput('tio', 'Is the patient being treated with tiotropium?', c('No', 'Yes')))

		} else if (input$model==modelOptions[3]) {

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),

		       selectInput('sex', 'Sex', c('male', 'female')),
		       
		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),
		       
		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),
		       
		       numericInput('packyears', 'Cumulative smoke pack-year', '', min=0, max=0),

		       numericInput('triglycerides', 'Triglycerides (mg/dl)', '', min=0, max=0),
		       
		       numericInput('hematocrit', 'Hematocrit (%)', '', min=0, max=0),

		       numericInput('albumin', 'Albumin (mg/L)', '', min=0, max=0),

		       numericInput('Globulin', 'Globulin (g/L)', '', min=0, max=0),

		       numericInput('AlkalinePhosphotase', 'Alkaline Phosphotase (units)', '', min=0, max=0),

		       numericInput('wbc', 'White blood cell count (10^9/L)', '', min=0, max=0),

		       numericInput('qrs', 'QRS interval (hundredth of second)', '', min=0, max=0),

		       numericInput('alcohol', 'Alcohol index (ozs/wk)', '', min=0, max=0),
		       
		       numericInput('wine', 'Wine intake (glasses/wk)', '', min=0, max=0),
		       
		       numericInput('cocktail', 'Cocktail Intake (drinks/wk)', '', min=0, max=0),
		       
		       numericInput('alcohol', 'Alcohol index (ozs/wk)', '', min=0, max=0),
		       
		       selectInput('bronchodilator', 'Bronchodilator Use?', c('current user', 'former user', 'no use')),

		       selectInput('dyspnea', 'Dyspnea on exertion', c('On rigorous exercise', 'On moderate exercise', 'On slight exertion', 'no dyspnea')),
  
		       selectInput('nocturnal', 'Nocturnal symptoms', c('Yes', 'Maybe', 'No')))
  
		} else if (input$model==modelOptions[4]) {

		  list(numericInput('age', 'Please select age', 55, min=40, max=90),
		       
		       selectInput('sex', 'Sex', c('male', 'female')),
		       
		       numericInput('weight', 'Weight (kg)', 75, min=40, max=120),
		       
		       numericInput('height', 'Height (m)', 1.7, min=1.2, max=2),
		       
		       numericInput('packyears', 'Cumulative smoke pack-year', '', min=0, max=0),
		       
		       numericInput('triglycerides', 'Triglycerides (mg/dl)', '', min=0, max=0),
		       
		       numericInput('hematocrit', 'Hematocrit (%)', '', min=0, max=0),
		       
		       numericInput('albumin', 'Albumin (mg/L)', '', min=0, max=0),
		       
		       numericInput('Globulin', 'Globulin (g/L)', '', min=0, max=0),
		       
		       numericInput('AlkalinePhosphotase', 'Alkaline Phosphotase (units)', '', min=0, max=0),
		       
		       numericInput('wbc', 'White blood cell count (10^9/L)', '', min=0, max=0),
		       
		       numericInput('qrs', 'QRS interval (hundredth of second)', '', min=0, max=0),
		       
		       numericInput('alcohol', 'Alcohol index (ozs/wk)', '', min=0, max=0),
		       
		       numericInput('wine', 'Wine intake (glasses/wk)', '', min=0, max=0),
		       
		       numericInput('cocktail', 'Cocktail Intake (drinks/wk)', '', min=0, max=0),
		       
		       numericInput('alcohol', 'Alcohol index (ozs/wk)', '', min=0, max=0),
		       
		       selectInput('bronchodilator', 'Bronchodilator Use?', c('current user', 'former user', 'no use')),
		       
		       selectInput('dyspnea', 'Dyspnea on exertion', c('On rigorous exercise', 'On moderate exercise', 'On slight exertion', 'no dyspnea')),
		       
		       selectInput('nocturnal', 'Nocturnal symptoms', c('Yes', 'Maybe', 'No')))

		}

	  })

  data <- reactive({

    if(!is.null(input$fev1_0) & input$model==modelOptions[1]) {

        fev1_projection(input$fev1_0, input$int_effect, input$tio)


    } else if(!is.null(input$age) & input$model==modelOptions[2]){

        fev1_projection2(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                             input$height, input$oco, input$tio)


    } else if(!is.null(input$age) & input$model==modelOptions[3]) {
        fev1_projection3(input$fev1_0, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                             input$height, input$tio)

    } else if(!is.null(input$fev1_prev) & input$model==modelOptions[4]){

        fev1_projection4(input$fev1_0, input$fev1_prev, input$int_effect, sex=input$sex, smoking=input$smoking, input$age, input$weight,
                               input$height, input$oco, input$tio)

    }

  })

	output$figure<-renderPlotly({

      df <- data()$df
      print(class(df))

			p <- ggplotly(ggplot(df, aes(Time, FEV1)) + geom_line(aes(y = FEV1), color="black", linetype=1) +
			                geom_ribbon(aes(ymin=FEV1_lower, ymax=FEV1_upper), linetype=2, alpha=0.1) +
			                geom_line(aes(y = FEV1_lower), color=errorLineColor, linetype=2) +
			                geom_line(aes(y = FEV1_upper), color=errorLineColor, linetype=2) +
			                annotate("text", 1, 3.52, label="Mean FEV1 decline", colour="black", size=4, hjust=0) +
			                annotate("text", 1.15, 3.4, label=coverageInterval, colour=errorLineColor, size=4, hjust=0) +
			                labs(x=xlab, y=ylab) +
			                theme_bw()) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
			print(p)
  
			p$x$data[[1]]$text <- paste0("Time (years): ", df$Time, "<br />", "FEV1 (L): ", round(df$FEV1,3),
			                             "<br />FEV1 lower (L): ", round(df$FEV1_lower,3), "<br />FEV1 upper (L): ",
			                             round(df$FEV1_upper,3))

			p$x$data[[3]]$hoverinfo="none"
			p$x$data[[4]]$hoverinfo="none"
			p


	})

	output$cv<-renderTable({

      aa1 <- data()$aa1
			rownames(aa1)<-c("Mean FEV1", "95% credible interval-upper bound", "95% credible interval-lower bound",
			                 "Coefficient of Variation (CV) (%)")
			colnames(aa1)<- years

			return(aa1)
	},

	  include.rownames=T,
	  caption="FEV1 Heterogeneity",
	  caption.placement = getOption("xtable.caption.placement", "top"))

	output$prob_decliner<-renderText({

      bb1 <- data()$bb1
			prob_text <- 'Probability of being a rapid decliner (i.e. more than 40 ml/yr) over the next 11 years: '
      bb1 <- paste0(prob_text, as.numeric(bb1), "%")
			print(bb1)
	})

	output$severity<-renderPlotly({

		  if(data()$options==1){
		    gender<-1
		    age_x<-55
		    height_x<-1.7
		  } else {
		    if (input$sex=="male"){
		      gender<-1
		    } else if (input$sex=="female"){
		      gender<-0
		    }
		    age_x <- input$age
		    height_x <- input$height
		  }

      if(data()$options==4){
        x<-c(-1:11)
        rnames <-	c('Previous','Baseline', years)
      } else {
			  x<-c(0:11)
			  rnames <- c('Baseline', years)
      }

	  print("testing")

			df <- data()$df
			fev1_avg <- df$FEV1
			fev1_low <- df$FEV1_lower
			fev1_up <- df$FEV1_upper



			fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
						(0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)
			print("testing2")

			p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
			p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
      p_severe<-100-p_mild-p_moderate


      u1<-matrix(0,nrow=length(x),ncol=4)
      u1[,1]<-x
      u1[,2]<-p_mild
      u1[,3]<-p_moderate
      u1[,4]<-p_severe
      colnames(u1)<-c("year","mild", "moderate", "severe")
      rownames(u1)<- rnames
      print(u1)
      data <- as.data.frame(u1)

			p <- plot_ly(data, x= ~year, y = ~mild, type='bar', name='Mild', marker = list(color = toRGB("#009E73"))) %>%
			  add_trace(y = ~moderate, name='Moderate', marker = list(color = toRGB("#E69F00"))) %>%
			  add_trace(y = ~severe, name='Severe', marker = list(color = toRGB("#D55E00"))) %>%
			  layout(yaxis=list(title='Probability (%)'), barmode='stack',
			         xaxis=list(title='Year', type='category', categoryorder='trace'),
			         title='Probability of the selected patient being at each GOLD grade',
			         hovermode='x') %>% config(displaylogo=F, doubleClick=F, displayModeBar=F, modeBarButtonsToRemove=buttonremove)  %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

    print(p)

	})


	output$sevTab<-renderTable({


	  if(data()$options==1){
	    gender<-1
	    age_x<-55
	    height_x<-1.7
	  } else {
	    if (input$sex=="male"){
	      gender<-1
	    } else if (input$sex=="female"){
	      gender<-0
	    }
	    age_x <- input$age
	    height_x <- input$height
	  }

	  if(data()$options==4){
	    x<-c(-1:11)
	    cnames <-	c('Previous','Baseline', years)
	  } else {
	    x<-c(0:11)
	    cnames <- c('Baseline', years)
	  }

	  df <- data()$df
	  fev1_avg <- df$FEV1
	  fev1_low <- df$FEV1_lower
	  fev1_up <- df$FEV1_upper

	  fev_pred<-(0.5536 + -0.01303*(age_x+x) + -0.000172*(age_x+x)^2 + 0.00014098*(height_x*100)^2)*gender +
	    (0.4333 + -0.00361*(age_x+x) + -0.000194*(age_x+x)^2 + 0.00011496*(height_x*100)^2)*(1-gender)

	  p_mild<-round((1-pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
	  p_moderate<-round((pnorm(0.8, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96)-pnorm(0.5, fev1_avg/fev_pred, (fev1_up/fev_pred-fev1_avg/fev_pred)/1.96))*100,0)
	  p_severe<-100-p_mild-p_moderate

	  u1<-matrix(0,nrow=3, ncol=length(x))
	  u1[1,]<-p_mild
	  u1[2,]<-p_moderate
	  u1[3,]<-p_severe

	  colnames(u1)<- cnames
	  rownames(u1)<-c("Probability of being mild", "Probability of being moderate", "Probability of being severe")
	  return(u1)

	},
	include.rownames=T)
})

shinyApp(ui = ui, server = server)


