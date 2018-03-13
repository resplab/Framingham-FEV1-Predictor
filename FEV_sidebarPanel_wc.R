  button_width <- 160
  
  sidebarPanel(
  fluidRow(
    column(12,
           
           ####inputs on ROW #1
           fluidRow(
             
             #row 1, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "age",
                          "Age (year)",
                          value = NULL,
                          min = 0,
                          max = 250,
                          step = 1,
                          width = button_width
                        )
                    )
             ),
        
             
          ######################Note!!!!!!!!!!!!!###################################################
          # You need baseline FEV1 to make final prediction                                        #
          # If Null, please provide 3 options - 1.25, 2.25, 3.25, so that the patient can compare  #
          ##########################################################################################
          
             #row 1, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "fev1_0",
                          "FEV1 at baseline (L)",
                          value = NULL,
                          min = 0,
                          max = 250,
                          step = 1,
                          width = button_width
                        )
                    )
             ),

             
           #   #row 1, right input
           #   column(5,
           #          div(style = "font-size: 12px;",
           #              numericInput(
           #                "follow_up_baseline",
           #                # "Follow-up since baseline (year)",
           #                "Follow-up",
           #                value = NULL,
           #                min = -100,
           #                max = 100,
           #                width = button_width
           #              )
           #          )
           #   )
           ),
           
           ####inputs on ROW #2
           fluidRow(
             
             #row 2, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "trig",
                          "Triglycerides (mg/dl)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             #row 2, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "hema",
                          "Hematocrit (%)",
                          value = NULL,
                          min = 0,
                          max = 100,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #3
           fluidRow(
             
             #row 3, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "alb",
                          "Albumin (mg/L)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             #row 3, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "glob",
                          "Globulin (g/L)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #4
           fluidRow(
             
             #row 4, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "alk_phos",
                          "Alkaline Phosphotase",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             #row 4, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "white_bc",
                          "White blood cells(10^9/L)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #5
           fluidRow(
             
             #row 5, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "qrs",
                          "QRS interval (0.01 sec)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             ############################Note!!!!!!!!!!!!!!!!!##################
             #            Alcohol index is a derived variable                  #
             # N = ((0.570XHIGHBALLS[drinks/wk])+(0.444XBEERS[cans or bottles/wk])+(0.400XWINE[glass/wk])) #
             # Please see my change below                                      #
             ###################################################################
             
             #row 5, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "beer",
                          "beer intake (cans or bottles/wk)",
                          value = NULL,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #6
           fluidRow(
             
             #row 6, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "wine",
                          "Wine intake (glasses/wk)",
                          value = NULL,
                          min = 0,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             #row 6, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "cocktail",
                          "Cocktail intake (drinks/wk)",
                          value = NULL,
                          min = 0,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #7
           fluidRow(
             
             #row 7, left input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "height",
                          "Height (cm)",
                          value = NULL,
                          min = 0,
                          step = 0.01,
                          width = button_width
                        )
                    )
             ),
             
             ################## Note !!!!!!!!!!!!!!! #####################
             # Cumulative smoke packyear is a derived variable           #
             # Should be divided into 3 questions                        #
             # 1. how many years have been actively smoking (smoke_year) #
             # 2. how many cigs per day on average (daily_cigs)          #
             # if never smoke, enter 0 for both questions                #
             # We need these two variable to calculate cum_smoke         #
             #############################################################
             #row 7, right input
             column(5,
                    div(style = "font-size: 12px;",
                        numericInput(
                          "cum_smoke",
                          "Cum. smoke pack-year",
                          value = NULL,
                          min = 0,
                          step = 0.01,
                          width = button_width
                        )
                    )
             )
           ),
           ####inputs on ROW #8
           fluidRow(
             
             #row 8, left input
             column(5,
                    div(style = "font-size: 12px;",
                        selectInput(
                          "sex",
                          "sex",
                          # list('female', 'male'),
                          # selected = 'male')
                          list('Not Selected','female', 'male'),
                          selected = 'Not Selected')
                    )
             ),
             
             #row 8, right input
             column(5,
                    div(style = "font-size: 12px;",
                        selectInput(
                          "ba_use",
                          "Bronchodilator or aerosol",
                          # list('Current use', 'Former use', 'No use'),
                          # selected = 'No use'
                          list('Not Selected','Current use', 'Former use', 'No use'),
                          selected = 'Not Selected'
                        )
                    )
             )
           ),
           
           ####inputs on ROW #9
           fluidRow(
             
             #row 9, left input
             column(5,
                    div(style = "font-size: 12px;",
                        selectInput(
                          "dys_exer",
                          "Dyspnea on exertion",
                          # list(
                          #   'On rigorous exercise',
                          #   'On moderate exercise',
                          #   'On slight exertion',
                          #   'No dyspnea on ex.'
                          # ),
                          # selected = 'No dyspnea on ex.'
                          list(
                            'Not Selected',
                            'On rigorous exercise',
                            'On moderate exercise',
                            'On slight exertion',
                            'No dyspnea on ex.'
                          ),
                          selected = 'Not Selected'
                        )
                    )
             ),
             
             #row 9, right input
             column(5,
                    div(style = "font-size: 12px;",
                        selectInput(
                          "noc_s",
                          "Nocturnal symptoms",
                          # list('Yes', 'Maybe', 'No'),
                          # selected = 'No'
                          list('Not Selected','Yes', 'Maybe', 'No'),
                          selected = 'Not Selected'
                        )
                    )
             )
           ),
           ####inputs on ROW #10
           fluidRow(
             
             #row 10, left input
             column(5,
                    div(style = "font-size: 12px;",
                        # actionButton("save_inputs", "Save Inputs")
                        downloadButton("save_inputs", "Save Inputs")
                    )
             )
             # ,
             # hr()
           ),
           fluidRow(
             #row 10, right input
             column(10,
                    # div(style = "font-size: 12px;",
                    #     actionButton("load_inputs", "Load Inputs")
                    # )
                    div(style = "font-size: 12px;",
                        fileInput("file1","Choose CSV File to Load",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"),
                                  buttonLabel = "Load Inputs..."
                        )
                    )
             )
             # ,
             # hr()
           ),
           ####inputs on ROW #11
           fluidRow(
             
             #row 11, left input
             column(5,
                    div(style = "font-size: 12px;",
                        actionButton("lmer_Submit_button", "Run Linear mixed-effects models")
                    )
             ),
             hr()
           ),
           ####empty row - separates rows 10 and 11
           # fluidRow(
           #   column(5,div(style = "font-size: 12px;"," ")), #empty row, left input
           #   column(5,div(style = "font-size: 12px;"," "))  #empty row, right input
           # ),
           
           fluidRow(
             column(5,
                    div(style = "font-size: 12px;",
                        actionButton("clear_inputs", "Clear Inputs")
                    )
             )
             # ,
             # hr()
           )
           # ,
   
           
           
           # ,
           
           # ####inputs on ROW #11
           # fluidRow(
           #   
           #   #row 11, left input
           #   column(5,
           #          div(style = "font-size: 12px;",
           #              actionButton("lmer_Submit_button", "Run Linear mixed-effects models")
           #          )
           #   )
           # )
    )
  ), width=6
)
  