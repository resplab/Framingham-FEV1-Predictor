# #CHANGE NOTES:
# # 2017-10-08: added 'Not Selected' options for:
# #             NSB_CHECK(), NSB_CHECK_RC(), BA_USE_CHECK(), BA_USE_CHECK_RC(), DYS_EXER_CHECK(), DYS_EXER_CHECK_RC()
# #             updated BINARY_CODE_FROM_INPUTS() - changed sex, ba_use, dys_exer, noc_s from is.null(...) to if(...)
#
# #List of function used in FEV program:
# #AGE_CHECK(age) - checks patient's age falls into one of the two age categories: 35-49 y or 50-64 y.
# #BA_USE_CHECK(ba_use) - produces boolean, indicating the use of bronchodilator or aerosol by patient (for baseline)
# #BA_USE_CHECK_RC(ba_use) - produces boolean, indicating the use of bronchodilator or aerosol by patient (for FEV rate of change)
# #NSB_CHECK(noc_s) - produces boolean, indicating patient's nocturnal symptoms (for baseline)
# #NSB_CHECK_RC(noc_s) - produces boolean, indicating patient's nocturnal symptoms (for FEV rate of change)
# #DYS_EXER_CHECK(dys_exer) - dyspnea on exertion/varying levels of exercise (for baseline)
# #DYS_EXER_CHECK_RC(dys_exer) - dyspnea on exertion/varying levels of exercise (for FEV rate of change)
# #SEX_CHECK(sex, dys_exer) - checks if female & dyspnea on exertion/varying levels of exercise
# #AS(sex, alb) - effect of female on level of albumin (Albumin*sex)
# #HS(sex, height) - effect of female on height (Height square * sex)
# #SEX_FM(sex) - effect on sex on baseline
# #SEX_FM_RC(sex) - effect of sex on FEV rate of change
# #ACE(age) - effect of age on baseline (2 components: Age, y effect & Age category)
# #ACE_RC(age) - effect of age on FEV rate of change (2 components: Age, y effect & Age category)
# #BUE(ba_use, ba_use_bool) - bronchodilator or aerosol use by patient (for baseline)
# #BUE_RC(ba_use, ba_use_bool_rc) - bronchodilator or aerosol use by patient (for rate of change)
# #NSB(noc_s, noc_s_bool) - nocturnal symptoms' effect (on baseline)
# #NSB_RC(noc_s, noc_s_bool_rc) - nocturnal symptoms' effect (on rate of change)
# #FEV <- function (trig,...,sex) - baseline forced expiratory volume (FEV); intercept of linear regression equation
# #FEV_RC <- function (follow_up_baseline,...,sex) - rate of FEV change; slope of linear regression equation
# #BINARY_CODE_FROM_INPUTS(...) - binary code that tracks doctor's inputs (NULL or not NULL) & produces the name of model (i.e. binary code)
# #FEV_input_labels - vector of all input names (total: 17 inputs)
# #buildformula_factors - list of non-null inputs in the web application
# #FEV_calculate_coefficients - produces dataframe of names and values of coefficients of the lmer function
# #FEV_calculate_lmer_fn - creates linear mixed-effects model based on user inputs
#

# #BINARY_CODE_FROM_INPUTS(...) - binary code that tracks doctor's inputs (NULL or not NULL) & produces the name of model (i.e. binary code)
# #FEV_input_labels - vector of all input names (total: 17 inputs)
# #buildformula_factors - list of non-null inputs in the web application
# #FEV_calculate_coefficients - produces dataframe of names and values of coefficients of the lmer function
# #FEV_calculate_lmer_fn - creates linear mixed-effects model based on user inputs
# make_predictions - creates predictions and CI which are then used by plotly 

#function for forcing probabilities between zero and one 
asProbability <- function(p) {
  if (p > 1) {result = 1}
  else if (p < 0) {result = 0}
  else {result = p}
  
  return (result)
}

#function for generating binary code
BINARY_CODE_FROM_INPUTS <- function(
  fev1_0,
  age,
  # follow_up_baseline,
  trig,
  hema,
  alb,
  glob,
  alk_phos,
  wbc,
  qrs,
  beer,
  wine,
  cocktail,
  height,
  smoke_year,
  daily_cigs,
  sex, #selectInput
  ba_use,#selectInput
  dys_exer,#selectInput
  noc_s#selectInput
) {
  if(is.na(fev1_0))   {fev1_0 = 0} else {fev1_0 = 1}
  if(is.na(age)) {age = 0} else {age = 1}
  # if(is.na(follow_up_baseline)) {follow_up_baseline = 0} else {follow_up_baseline = 1}
  if(is.na(trig))     {trig = 0} else {trig = 1}
  if(is.na(hema))     {hema = 0} else {hema = 1}
  if(is.na(alb))      {alb = 0} else {alb = 1}
  if(is.na(glob))     {glob = 0} else {glob = 1}
  if(is.na(alk_phos)) {alk_phos = 0} else {alk_phos = 1}
  if(is.na(wbc)) {wbc = 0} else {wbc = 1}
  if(is.na(qrs))      {qrs = 0} else {qrs = 1}
  if(is.na(beer))     {beer = 0} else {beer = 1}
  if(is.na(wine))     {wine = 0} else {wine = 1}
  if(is.na(cocktail)) {cocktail = 0} else {cocktail = 1}
  if(is.na(height))   {height = 0} else {height = 1}
  if(is.na(smoke_year)) {smoke_year = 0} else {smoke_year = 1}
  if(is.na(daily_cigs)) {daily_cigs = 0} else {daily_cigs = 1}
  if(sex == '') {sex = 0} else {sex = 1}
  if(ba_use == '') {ba_use = 0} else {ba_use = 1}
  if(dys_exer == '') {dys_exer = 0} else {dys_exer = 1}
  if(noc_s == '') {noc_s = 0} else {noc_s = 1}
  bc <- c(fev1_0,
          age,
          # follow_up_baseline,
          trig,
          hema,
          alb,
          glob,
          alk_phos,
          wbc,
          qrs,
          beer,
          wine,
          cocktail,
          height,
          smoke_year,
          daily_cigs,
          sex,
          ba_use,
          dys_exer,
          noc_s)
  return(bc)
}

FEV_input_labels <- function() {
  c('fev1_0',
    'trig',
    'hema',
    'alb',
    'glob',
    'alk_phos',
    'wbc',
    'qrs',
    'beer',
    'wine',
    'cocktail',
    'height',
    'smoke_year',
    'daily_cigs',
    'age',
    'ba_use',
    'dys_exer',
    'noc_s',
    'sex'
  )
}

listoffactors = NULL #initialize listoffactors

buildformula_factors <- function(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME){
  if(!is.null(listoffactors)){listoffactors <- NULL}

  for(i in 1:nrow(BINARY_CODE_DATAFRAME)){
    if (BINARY_CODE_DATAFRAME$file_name[i] == 1){     #if INPUT value is not null
      listoffactors <- c(listoffactors,unlist((apply(FACTOR_NAMES_DATAFRAME[,(2:ncol(FACTOR_NAMES_DATAFRAME))], 1, function(x) unname(x[!is.na(x)])))[i]))
    }
  }
  unique_listoffactors <- unique(listoffactors)
  return(unique_listoffactors)
}


FEV_calculate_lmer_fn<- function(responseVar, BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME,updateProgress = NULL){
  #####################################
  #STEP0: Prepare the data(Chen's code)
  #####################################

  data_rf4 <- readRDS("./data_rf4.rds") #load reduced size file
  
  #-------------------------------------------#
  #   Running the Random effects model	    #
  #-------------------------------------------#
  # Note: the model is based on framingham data data_rf4 (centered/scaled, with a censoring variable)
  #Step 1: calculate stablized inverse probability weights of dropping out to the regression model;
  tstarting_time<-ipw::tstartfun(RANDOMID, visit, data_rf4)		#Preparing the data for calculation of inverse probability weight of being censored
  # Calculate inverse probability weight of being censored, which is a stablized inverse probability weight
  ipw<- ipw::ipwtm(exposure = status, family = "binomial",link="logit",numerator=~1,
              denominator=~age+agecat+sex+triglycerides+hematocrit+albumin+globulin+ALP+wine+cocktail+WBC
              +QRS_intv+alcohol_indx+height2+broncho+dyspnea_exc+night_sym,
              id = RANDOMID, tstart = tstarting_time, timevar = visit, type = "first",
              data = data_rf4)

  data_rf4<-cbind.data.frame(data_rf4,ipw$ipw.weights) #add censoring variable to data
  colnames(data_rf4)[27]<-'sw'		#change the name of the weight to "sw" - stablized weights

  ########################################################
  #STEP1: Generate BINARY_CODE_DATAFRAME from the filename - NO, just pass BINARY_CODE_DATAFRAME to the function
  ########################################################

  #STEP2: Create inside this func. or outside this function, the FACTOR_NAMES_DATAFRAME - NO, just pass FACTOR_NAMES_DATAFRAME to the function

  #STEP3: Use buildformula_factors(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME) to build the equation
  formula_factors <- buildformula_factors(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
  #STEP4: use reformulate to build the full equation(can combine steps 3 and 4)
  # formula_factors <- c(formula_factors, "year", "year2", "(year|RANDOMID)") #NOTE: "year", "year2", "(year|RANDOMID)" are now all mapped to fev1_0 input
  full_formula <- reformulate(formula_factors,response=responseVar)
  #STEP5: Use lmfin to compute the coefficients
  lmfin <- lmer(full_formula,data_rf4,weights=sw, REML=FALSE)
  return(lmfin)
}


make_predictions <- function(respVar, lmfin, predictors) {

  #Add a RANDOMID to make predictions
  predictors$RANDOMID<-1
  
  if (respVar == 'fev1_fvc') {
    predictors$fev1_fvc_0 <- 1.0
  }

  # Create age category
  predictors$agecat[predictors$age>=65]<- 3
  # Extrapolationg 

  predictors$agecat[predictors$age<65 & predictors$age>=50]<-3
  predictors$agecat[predictors$age<50 & predictors$age>=35]<-2
  predictors$agecat[predictors$age<35 & predictors$age>=20]<-1
  predictors$agecat <- as.factor(predictors$agecat)

  # Create sex category
  predictors$sexcat[predictors$sex=='male'] <- 1
  predictors$sexcat[predictors$sex=='female'] <- 2
  # predictors$sexcat<-as.factor(predictors$sexcat)
  predictors$sex <- as.factor(predictors$sexcat)

  #Wenjia: this is the category that we use
  #Create broncho category
  predictors$bronchocat[predictors$broncho=='Current use'] <- 1
  predictors$bronchocat[predictors$broncho=='Former use'] <- 2
  predictors$bronchocat[predictors$broncho=='No use'] <- 0
  predictors$broncho <- as.factor(predictors$bronchocat)


  #Wenjia: this is the coding of categories in the Framingham data
  # Create dyspnea category
  predictors$dyspnea_exc_cat[predictors$dyspnea_exc=='Yes, on walking up stairs or other vigorous excercise'] <- 1
  predictors$dyspnea_exc_cat[predictors$dyspnea_exc=='Yes, on rapid walking or other moderderate exercise'] <- 2
  predictors$dyspnea_exc_cat[predictors$dyspnea_exc=='On any slight exertion'] <- 3
  predictors$dyspnea_exc_cat[predictors$dyspnea_exc=='No'] <- 0
  predictors$dyspnea_exc <- as.factor(predictors$dyspnea_exc_cat)

  # Wenjia: this is the coding of categories in Framingham
  # Create night symptoms category
  predictors$night_sym_cat[predictors$night_sym=='Yes'] <- 4
  predictors$night_sym_cat[predictors$night_sym=='Maybe'] <- 5
  predictors$night_sym_cat[predictors$night_sym=='No'] <- 3
  predictors$night_sym <- as.factor(predictors$night_sym_cat)


  # Center input predictors
  predictors$fev1_0[!is.na(predictors$fev1_0)]<-(predictors$fev1_0-2.979447188)/0.794445308 #WC: I centered all continuous predictors except for cum_smoke and year
  if (respVar == 'fev1_fvc') predictors$fev1_fvc_0[!is.na(predictors$fev1_fvc_0)]<-(predictors$fev1_fvc_0-0.7904786)/0.09908784  
  predictors$triglycerides[!is.na(predictors$triglycerides)]<-(predictors$triglycerides-93.02984434)/79.628844
  predictors$hematocrit[!is.na(predictors$hematocrit)]<-(predictors$hematocrit-42.83871875)/3.770632403
  predictors$albumin[!is.na(predictors$albumin)]<-(predictors$albumin-46.7343477)/3.259360147
  predictors$globulin[!is.na(predictors$globulin)]<-(predictors$globulin-25.90719409)/3.530116396
  predictors$ALP[!is.na(predictors$ALP)]<-(predictors$ALP-56.64908166)/16.30523751
  predictors$WBC[!is.na(predictors$WBC)]<-(predictors$WBC-61.5919838)/16.32819272
  predictors$QRS_intv[!is.na(predictors$QRS_intv)]<-(predictors$QRS_intv-7.903884425)/0.784763186
  predictors$alcohol_indx[!is.na(predictors$alcohol_indx)]<-(predictors$alcohol_indx-3.681324783)/4.781456965
  #this is CENTERED alcohol index = ((0.570XHIGHBALLS[drinks/wk])+(0.444XBEERS[cans or bottles/wk])+(0.400XWINE[glass/wk]))
  predictors$wine[!is.na(predictors$wine)]<-(predictors$wine-1.532559397)/3.13716088
  predictors$cocktail[!is.na(predictors$cocktail)]<-(predictors$cocktail-2.749524582)/5.049623158
  predictors$height2[!is.na(predictors$height)]<-(predictors$height^2-28422.20329)/3185.597537 #this is centered height square
  # predictors$height2[!is.na(predictors$height2)]<-(predictors$height^2-28422.20329)/3185.597537 #this is centered height square
  predictors$cpackyr<-round((predictors$smoke_year*predictors$daily_cigs)/20,0) #this is a derived variable, need two variables, dont need to center
  predictors$age<-(predictors$age-36.61082037)/9.249913362 #assuming nobody will miss age entry

  #make sure all categorical variables are factors
  predictors$sex<-as.factor(predictors$sex)

  #generate year for prediction
  year<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  year2<-year^2
  year<-cbind(year,year2)

  data_pred<-merge(predictors,year,all=TRUE)
  #Now I will create two scenario for 20-year prediction of FEV1 decline
  #Scenario 1: quit smoke today (smk=0)
  #Scenario 2: continue to smoke at current speed (smk=1)
  smk<-c(0,1)
  data_pred<-merge(data_pred,smk,all=TRUE) #From here on, for quitting smoke and continue to smoke, each scenario has 20 years of follow-up

  #Make sure non-smoker's baseline CUM_SMoke=0

  # data_pred <- rename(data_pred, replace=c("y"="smk")) #JK: added
  #because we sometimes get teh following Warning: Error in : Expressions are currently not supported in `rename()
  data_pred <- plyr:::rename(data_pred, replace=c("y"="smk")) #JK: added
  
  data_pred$cpackyr[data_pred$smk==0]<-data_pred$cpackyr[data_pred$smk==0]
  data_pred$cpackyr[data_pred$smk==1]<-data_pred$cpackyr[data_pred$smk==1]+data_pred$daily_cigs[data_pred$smk==1]*data_pred$year[data_pred$smk==1]/20 #If continue to smoke, calculate cumulative pack-years over time
  #Note: for smoke=0, pack-years will continue all the time

  #12/12/2017
  # Warning message:
  #   In data_pred$cum_smoke[data_pred$smk == 1] <- data_pred$cum_smoke +  :
  #   number of items to replace is not a multiple of replacement length


  ##############################################################
  #  When data is ready, prediction begins here                #
  ##############################################################
  #Obtain fixed Coefficients;
  # lmfin <- lmer_object
  beta<-fixef(lmfin) #JK: extracting fixed-effects estimates
  vcov<-vcov(lmfin) #JK: returns the variance-covariance matrix of the main parameters of a fitted model object
  vbeta<-diag(vcov(lmfin)) #JK: extract or replace the diagnonal of a variance-covariance matrix, or contruct a diagonal variance-covariance matrix

  #vARIANCE-COVARIANCE coefficients
  vc<-as.data.frame(VarCorr(lmfin)) #JK: functions to check if an object is a data frame, or coerce it if possible;
  #JK: VarCorr() calculates the estimated variances, SD and correlations between the random-effects terms in a mixed-effects model.
  #JK: vc produces a table with 4 rows; in the next four rows of code, extract elements in the 4th column and assign them to variables - v.int, v.yr, cov.int.yr, v.err
  v.int<-vc$vcov[1]
  v.yr<-vc$vcov[2]
  cov.int.yr<-vc$vcov[3]
  v.err<-vc$vcov[4]

  # Prediction;
  
  pred<-lme4:::predict.merMod(object=lmfin,newdata=data_pred,re.form=NA, allow.new.levels=TRUE) #JK:predict is a generic function for predictions from the results of various model fitting functions.
  
  data_pred2<-cbind(data_pred,pred)

  #get predicted fev1 at baseline for calculation (pfev0)
  if (respVar == 'fev1') { 
    pfev0<-subset(data_pred2,year==0 & smk==0,select=c(RANDOMID,pred))
   # print(pfev0)  #debug amin
    colnames(pfev0)[2]<-"pfev0"
    data_pred2<-join(data_pred2,pfev0,by='RANDOMID', type='right',match='all')
    
  } else if (respVar == 'fev1_fvc') {
    #print(data_pred2)  #debug amin
    pfev_fvc0<-subset(data_pred2,year==0 & smk==0,select=c(RANDOMID,pred))
    
    #print(pfev_fvc0)  #debug amin
    colnames(pfev_fvc0)[2]<-"pfev_fvc0"
    data_pred2<-join(data_pred2,pfev_fvc0,by='RANDOMID', type='right',match='all')
  }

  #Calculation the bivariate correlation between baseline and future FEV1 value, used for predition interval
  cov11<-v.int+2*data_pred2$year*cov.int.yr+data_pred2$year2*v.yr+v.err
  cov12<-v.int+data_pred2$year*cov.int.yr
  cov22<-v.int+v.err

  #Calculation the bivariate correlation between baseline and future FEV1 value, used for confidence interval
  rand_eff <- ranef(lmfin)
  rand_eff <- as.data.frame(rand_eff)
  rand_int <- subset(rand_eff, term == "(Intercept)")[,4]
  rand_yr  <- subset(rand_eff, term == "year")[,4]
  
  v.int.rand <- var(rand_int) 
  v.yr.rand <- var(rand_yr)
  cov.int.yr.rand <- cov(rand_int, rand_yr)
  
  #TODO verify variances for random effect confidence intervals
  cov11.rand <-v.int.rand+2*data_pred2$year*cov.int.yr.rand+data_pred2$year2*v.yr.rand#+v.err
  cov12.rand <-v.int.rand+data_pred2$year*cov.int.yr.rand
  cov22.rand <-v.int.rand#+v.err
  
  print("v.yr")
  print(v.yr)
  print("v.yr.rand")
  print(v.yr.rand)
  print("v.int")
  print(v.int)
  print("v.int.rand")
  print(v.int.rand)
  print("v.err")
  print (v.err)
  
  
  data_pred2<-cbind(data_pred2, cov11, cov12, cov22, cov11.rand, cov12.rand, cov22.rand )
  
  #relate baseline fev1 to future fev1 to make final prediction
  if (respVar == 'fev1') { 
    pred2<-data_pred2$pred+data_pred2$cov12*(data_pred2$fev1_0-data_pred2$pfev0)/data_pred2$cov22
    se2<-sqrt(data_pred2$cov11-data_pred2$cov12*data_pred2$cov12/data_pred2$cov22)
    se2.rand <- sqrt(data_pred2$cov11.rand-data_pred2$cov12.rand*data_pred2$cov12.rand/data_pred2$cov22.rand)
    
    #VERY IMPORTANT!!!!  - back-transform PREDICTION into original scale
    #pred3<-pred2*y.sd+y.mean
    pred3<-pred2*0.794445308+2.979447188
    #se3<-se2*y.sd
    se3<-se2*0.794445308
    se3.rand <- se2.rand*0.794445308
    
    
    lower3<-pred3-1.960463534*se3 #lower 95% prediction interval
    upper3<-pred3+1.960463534*se3 #upper 95% prediction interval
  
    lower3.rand<-pred3-1.960463534*se3.rand #lower 95% confidence interval
    upper3.rand<-pred3+1.960463534*se3.rand #upper 95% confidence interval
  
    data_pred_fin<-cbind(data_pred2$year, data_pred2$smk, data_pred2$cpackyr,data_pred2$fev1_0,pred3,se3, lower3, upper3, se3.rand, lower3.rand, upper3.rand)
    data_pred_fin <- as.data.frame(data_pred_fin)
    colnames(data_pred_fin)<-c("year","smoking","cpackyr","fev1_0","predicted_FEV1","predicted_SE_PI", "lowerbound_PI", "upperbound_PI", "predicted_SE_CI", "lowerbound_CI", "upperbound_CI")
    # Note: We used baseline FEV1 to predict future FEV1, so baseline FEV1 should be set to original value, se should be 0
    data_pred_fin$predicted_FEV1[data_pred_fin$year==0]<-data_pred_fin$fev1_0[data_pred_fin$year==0]*0.794445308+2.979447188 #backtransformed
    data_pred_fin$predicted_SE_PI[data_pred_fin$year==0]<-0
    data_pred_fin$predicted_SE_CI[data_pred_fin$year==0]<-0
    data_pred_fin$lowerbound_PI[data_pred_fin$year==0]<-data_pred_fin$predicted_FEV1[data_pred_fin$year==0]
    data_pred_fin$upperbound_PI[data_pred_fin$year==0]<-data_pred_fin$predicted_FEV1[data_pred_fin$year==0]
    data_pred_fin$lowerbound_CI[data_pred_fin$year==0]<-data_pred_fin$predicted_FEV1[data_pred_fin$year==0]
    data_pred_fin$upperbound_CI[data_pred_fin$year==0]<-data_pred_fin$predicted_FEV1[data_pred_fin$year==0]
    
    #removin untransfromed FEV10 from the dataframme
    data_pred_fin = subset(data_pred_fin, select = -c(fev1_0) )
    
    #calculating %predicted FEV1, sex == 1 male. sex == 1 female. Following the NHANES-III algorithm, using 25y/o white Caucasian as reference,for people aged 20 years and above
    if   (predictors$sex == 1) { 
    data_pred_fin$percentpred <- 100 * data_pred_fin$predicted_FEV1 / ((0.5536+(-0.01303)*25+(-0.000172)*25*25+0.00014098*predictors$height*predictors$height))
    data_pred_fin$percentpred_upperbound_PI <- 100 * data_pred_fin$upperbound_PI / ((0.5536+(-0.01303)*25+(-0.000172)*25*25+0.00014098*predictors$height*predictors$height))
    data_pred_fin$percentpred_lowerbound_PI <- 100 * data_pred_fin$lowerbound_PI / ((0.5536+(-0.01303)*25+(-0.000172)*25*25+0.00014098*predictors$height*predictors$height))
    data_pred_fin$percentpred_upperbound_CI <- 100 * data_pred_fin$upperbound_CI / ((0.5536+(-0.01303)*25+(-0.000172)*25*25+0.00014098*predictors$height*predictors$height))
    data_pred_fin$percentpred_lowerbound_CI <- 100 * data_pred_fin$lowerbound_CI / ((0.5536+(-0.01303)*25+(-0.000172)*25*25+0.00014098*predictors$height*predictors$height))
    
    }
    
    if   (predictors$sex == 2) { 
      data_pred_fin$percentpred <- 100 * data_pred_fin$predicted_FEV1 / ((0.4333+(-0.00361)*25+(-0.000194)*25*25+0.00011496*predictors$height*predictors$height))
      data_pred_fin$percentpred_upperbound_PI <- 100 * data_pred_fin$upperbound_PI / ((0.4333+(-0.00361)*25+(-0.000194)*25*25+0.00011496*predictors$height*predictors$height))
      data_pred_fin$percentpred_lowerbound_PI <- 100 * data_pred_fin$lowerbound_PI / ((0.4333+(-0.00361)*25+(-0.000194)*25*25+0.00011496*predictors$height*predictors$height))
      data_pred_fin$percentpred_upperbound_CI <- 100 * data_pred_fin$upperbound_CI / ((0.4333+(-0.00361)*25+(-0.000194)*25*25+0.00011496*predictors$height*predictors$height))
      data_pred_fin$percentpred_lowerbound_CI <- 100 * data_pred_fin$lowerbound_CI / ((0.4333+(-0.00361)*25+(-0.000194)*25*25+0.00011496*predictors$height*predictors$height))
      
      }

      #reducing rows in data_pred_fin
      data_if_smoke <- subset(data_pred_fin, smoking == 1)
      data_if_quit <- subset(data_pred_fin, smoking == 0)
      data_pred_fin <- subset(data_pred_fin, smoking == 0)
      data_pred_fin <- subset(data_pred_fin, select = -c(smoking, cpackyr,  upperbound_CI, lowerbound_CI, percentpred, percentpred_upperbound_CI, percentpred_lowerbound_CI))
      
      #adding coloumns for smoking vs. quitting scenario
      data_pred_fin$predicted_FEV1_if_smoke <- data_if_smoke$predicted_FEV1
      data_pred_fin$FEV1_lowerbound_CI_if_smoke <- data_if_smoke$lowerbound_CI
      data_pred_fin$upperbound_CI_if_smoke <- data_if_smoke$upperbound_CI
      
      
      data_pred_fin$predicted_FEV1_if_quit <- data_if_quit$predicted_FEV1
      data_pred_fin$FEV1_lowerbound_CI_if_quit <- data_if_quit$lowerbound_CI
      data_pred_fin$upperbound_CI_if_quit <- data_if_quit$upperbound_CI
      
      data_pred_fin$cpackyr_if_smoke <- data_if_smoke$cpackyr
      data_pred_fin$cpackyr_if_quit <- data_if_quit$cpackyr
      
      
      
      #same for percentpred
      data_pred_fin$percentpred_if_smoke <- data_if_smoke$percentpred
      data_pred_fin$percentpred_FEV1_lowerbound_CI_if_smoke <- data_if_smoke$percentpred_lowerbound_CI
      data_pred_fin$percentpred_upperbound_CI_if_smoke <- data_if_smoke$percentpred_upperbound_CI
      
      
      data_pred_fin$percentpred_if_quit <- data_if_quit$percentpred
      data_pred_fin$percentpred_FEV1_lowerbound_CI_if_quit <- data_if_quit$percentpred_lowerbound_CI
      data_pred_fin$percentpred_upperbound_CI_if_quit <- data_if_quit$percentpred_upperbound_CI
      
  } else if (respVar == 'fev1_fvc') {
    pred2<-data_pred2$pred+data_pred2$cov12*(data_pred2$fev1_fvc_0-data_pred2$pfev_fvc0)/data_pred2$cov22

    se2<-sqrt(data_pred2$cov11-data_pred2$cov12*data_pred2$cov12/data_pred2$cov22)
    
    prob<-pnorm(((0.7-0.7904786)/0.09908784-pred2)/se2)
    
    data_pred_fin<-cbind(data_pred2$year, data_pred2$smk, data_pred2$cpackyr, data_pred2$fev1_fvc_0, prob)
    data_pred_fin <- as.data.frame (data_pred_fin)
    colnames(data_pred_fin)<-c("year","smoking","cpackyr","fev1_fvc_0","COPD_risk")

    #reducing rows in data_pred_fin
    data_if_smoke <- subset(data_pred_fin, smoking == 1)
    data_if_quit <- subset(data_pred_fin, smoking == 0)
    
    data_pred_fin <- subset(data_pred_fin, smoking == 0)
    data_pred_fin <- subset(data_pred_fin, select = -c(smoking, cpackyr, COPD_risk))
    
    # adding coloumns for smoking vs. quitting scenario
    # we assume a bernulli distribution for uncertainty around probablity. SE = p (1 - p)
    data_pred_fin$COPD_risk_if_smoke <- (data_if_smoke$COPD_risk)
    SE_COPD_risk_if_smoke <- sqrt(data_if_smoke$COPD_risk * (1 - data_if_smoke$COPD_risk) / 13567) #13567 is n
    data_pred_fin$COPD_risk_lowerbound_if_smoke <- (data_if_smoke$COPD_risk - 1.96 *  SE_COPD_risk_if_smoke) 
    data_pred_fin$COPD_risk_upperbound_if_smoke <- (data_if_smoke$COPD_risk + 1.96 *  SE_COPD_risk_if_smoke) 
    
    
    data_pred_fin$COPD_risk_if_quit <- (data_if_quit$COPD_risk)
    SE_COPD_risk_if_quit <- sqrt (data_if_quit$COPD_risk * (1 - data_if_quit$COPD_risk) / 13567)
    data_pred_fin$COPD_risk_lowerbound_if_quit <- (data_if_quit$COPD_risk - 1.96 * SE_COPD_risk_if_quit)
    data_pred_fin$COPD_risk_upperbound_if_quit <- (data_if_quit$COPD_risk + 1.96 * SE_COPD_risk_if_quit)
    
    data_pred_fin$cpackyr_if_smoke <- data_if_smoke$cpackyr
    data_pred_fin$cpackyr_if_quit <- data_if_quit$cpackyr
    
    data_pred_fin <- as.data.frame (data_pred_fin)
    
    
  }
      
  return(data_pred_fin)
}



################################################################################################################
#############################FUNCTIONS BELOW ARE NOT CURRENTLY USED#############################################
################################################################################################################
##############NOTE !!!!!!!!!!!!!!!!!!!!!###################
# These coefficients are for transformed datasets         #
# They can not directly multiply with untransformed data  #
# Need to first transform you input                       #
###########################################################

FEV_calculate_coefficients<- function(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME){
  #####################################
  #STEP0: Prepare the data(Chen's code)
  #####################################
  
  data_rf4 <- readRDS("./data_rf4.rds") #load reduced size file
  
  
  #-------------------------------------------#
  #   Running the Random effects model	    #
  #-------------------------------------------#
  # Note: the model is based on framingham data data_rf4 (centered/scaled, with a censoring variable)
  #Step 1: calculate stablized inverse probability weights of dropping out to the regression model;
  tstarting_time<-ipw::tstartfun(RANDOMID, visit, data_rf4)		#Preparing the data for calculation of inverse probability weight of being censored
  # Calculate inverse probability weight of being censored, which is a stablized inverse probability weight
  ipw<- ipw::ipwtm(exposure = status, family = "binomial",link="logit",numerator=~1,
              denominator=~age+agecat+sex+triglycerides+hematocrit+albumin+globulin+ALP+wine+cocktail+WBC
              +QRS_intv+alcohol_indx+height2+broncho+dyspnea_exc+night_sym,
              id = RANDOMID, tstart = tstarting_time, timevar = visit, type = "first",
              data = data_rf4)
  
  data_rf4<-cbind.data.frame(data_rf4,ipw$ipw.weights) #add censoring variable to data
  colnames(data_rf4)[27]<-'sw'		#change the name of the weight to "sw" - stablized weights
  
  ########################################################
  #STEP1: Generate BINARY_CODE_DATAFRAME from the filename - NO, just pass BINARY_CODE_DATAFRAME to the function
  ########################################################
  
  
  #STEP2: Create inside this func. or outside this function, the FACTOR_NAMES_DATAFRAME - NO, just pass FACTOR_NAMES_DATAFRAME to the function
  
  #STEP3: Use buildformula_factors(BINARY_CODE_DATAFRAME,FACTOR_NAMES_DATAFRAME) to build the equation
  formula_factors <- buildformula_factors(BINARY_CODE_DATAFRAME,FACTORS_NAMES_DATAFRAME)
  
  #STEP4: use reformulate to build the full equation(can combine steps 3 and 4)
  full_formula <- reformulate(formula_factors,response="fev1")
  #STEP5: Use lmfin to compute the coefficients
  lmfin <- lmer(full_formula,data_rf4,weights=sw, REML=FALSE)
  #STEP6: Use extract_lmer_coefficients(lmfin) to extract coefficients
  # FEV_coeff_DATA_FRAME <- extract_lmer_coefficients(lmfin)
  coeff_lmfin <- coeffs(lmfin)
  coefficients_DATA_FRAME <- as.data.frame(coeff_lmfin)
  
  final_FEV_coeff_data_frame<- data.frame(FEV_coeff_names=row.names(coefficients_DATA_FRAME),
                                          FEV_coeff_vals=coefficients_DATA_FRAME$coeff_lmfin)
  
  return(final_FEV_coeff_data_frame) #return names and values of the coefficients
}

