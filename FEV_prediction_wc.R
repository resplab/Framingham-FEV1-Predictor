make_predictions<-function(lmer_object, predictors){
  
  attach(full_file_name) 
  #Is this the prediction data? Please use the prediction data, it should be one row of entry with some NULL variables. 
  
  #Add a RANDOMID to make predictions
   full_file_name$RANDOMID<-1
  
  # Create age category
   full_file_name$agecat[full_file_name$age>=65]<-4
   full_file_name$agecat[full_file_name$age<65 &full_file_name$age>=50]<-3
   full_file_name$agecat[full_file_name$age<50 &full_file_name$age>=35]<-2
   full_file_name$agecat[full_file_name$age<35 &full_file_name$age>=20]<-1
   full_file_name$agecat<-as.factor( full_file_name$agecat)
   
  # Center input predictors 
   full_file_name$fev1_0[is.na!(full_file_name$fev1_0)]<-full_file_name$fev1_0-2.979447188)/0.794445308 #WC: I centered all continuous predictors except for cum_smoke and year
   full_file_name$trig[is.na!(full_file_name$trig)]<-(full_file_name$trig-93.02984434)/79.628844  
   full_file_name$hema[is.na!(full_file_name$hema)]<-(full_file_name$hema-42.83871875)/3.770632403
   full_file_name$alb[is.na!(full_file_name$alb)]<-(full_file_name$alb-46.7343477)/3.259360147
   full_file_name$glob[is.na!(full_file_name$glob)]<-(full_file_name$glob-25.90719409)/3.530116396
   full_file_name$alk_phos[is.na!(full_file_name$alk_phos)]<-(full_file_name$alk_phos-56.64908166)/16.30523751 
   full_file_name$white_bc[is.na!(full_file_name$white_bc)]<-(full_file_name$white_bc-61.5919838)/16.32819272
   full_file_name$qrs[is.na!(full_file_name$qrs)]<-(full_file_name$qrs-7.903884425)/0.784763186
   full_file_name$alcohol<-((full_file_name$beer*0.444+full_file_name$cocktail*0.570+full_file_name$wine*0.400)-3.681324783)/4.781456965
               #this is CENTERED alcohol index = ((0.570XHIGHBALLS[drinks/wk])+(0.444XBEERS[cans or bottles/wk])+(0.400XWINE[glass/wk])) 
   full_file_name$wine[is.na!(<-(full_file_name$wine-1.532559397)/3.13716088
   full_file_name$cocktail[is.na!(<-(full_file_name$cocktail-2.749524582)/5.049623158
   full_file_name$height2[is.na!(<-(full_file_name$height^2-28422.20329)/3185.597537 #this is centered height square
   full_file_name$cum_smoke<-round((full_file_name$smoke_year*full_file_name$daily_cigs)/20,0) #this is a derived variable, need two variables, dont need to center 
   full_file_name$age<-(full_file_name$age-36.61082037)/9.249913362 
   
   #make sure all categorical variables are factors 
   full_file_name$sex<-as.factor(full_file_name$sex) 
   
   #generate year for prediction
   year<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
   year2<-year^2
   year<-cbind(year,year2)
   
   data_pred<-merge(full_file_name,year,all=TRUE)	
   
   #Now I will create two scenario for 20-year prediction of FEV1 decline 
   #Scenario 1: quit smoke today (smk=0)
   #Scenario 2: continue to smoke at current speed (smk=1)
   smk<-c(0,1)
   data_pred<-merge(data_pred,smk,all=TRUE) #From here on, for quitting smoke and continue to smoke, each scenario has 20 years of follow-up
                
   #Make sure non-smoker's baseline CUM_SMoke=0
   data_pred$cum_smoke[data_pred$smk==1]<-data_pred$cum_smoke+data_pred$daily_cigs*data_pred$year/20 #If continue to smoke, calculate cumulative pack-years over time
   #Note: for smoke=0, pack-years will continue all the time
  
   
   ##############################################################
   #  When data is ready, prediction begins here                #
   ############################################################## 
   #Obtain fixed Coefficients; 
  beta<-fixef(lmfin)
  vcov<-vcov(lmfin)
  vbeta<-diag(vcov(lmfin))

  #vARIANCE-COVARIANCE coefficients 
  vc<-as.data.frame(VarCorr(lmfin))
  vc
  v.int<-vc$vcov[1]
  v.yr<-vc$vcov[2]
  cov.int.yr<-vc$vcov[3]
  v.err<-vc$vcov[4]

  # Prediction; 
  pred<-predict(lmfin,data_pred,re.form=NA, allow.new.levels=TRUE)
    #Note!!!: I wonder if you could rename these data variables to in line with the data_rf4's variable names, otherwise it will not work
  data_pred2<-cbind(data_pred,pred)
  
  #get predicted fev1 at baseline for calculation (pfev0)
  pfev0<-subset(data_pred2,year==0,select=c(RANDOMID,pred)) 
  colnames(pfev0)[2]<-"pfev0"
  data_pred2<-join(data_pred2,pfev0,by='RANDOMID', type='right',match='all')
  
  #Calculation the bivariate correlation between baseline and future FEV1 value 
  cov11<-v.int+2*data_pred2$year*cov.int.yr+data_pred2$year2*v.yr+v.err
  cov12<-v.int+data_pred2$year*cov.int.yr
  cov22<-v.int+v.err

  data_pred2<-cbind(data_pred2,cov11,cov12)
  data_pred2<-merge(data_pred2,cov22,all=TRUE) #please make sure cov22's variable name is accurate in the prediction dataset


  #relate baseline fev1 to future fev1 to make final prediction 
  pred2<-data_pred2$pred+data_pred2$cov12*(data_pred2$fev1_0-data_pred2$pfev0)/data_pred2$cov22
  se2<-sqrt(data_pred2$cov11-data_pred2$cov12*data_pred2$cov12/data_pred2$cov22)

  #VERY IMPORTANT!!!!  - back-transform PREDICTION into original scale 
  #pred3<-pred2*y.sd+y.mean
   pred3<-pred2*0.794445308+2.979447188	
  #se3<-se2*y.sd
   se3<-se2*0.794445308
   lower3<-pred3-1.960463534*se3 #lower 95% prediction interval
   upper3<-pred3+1.960463534*se3 #upper 95% prediction interval 
   

   data_pred_fin<-cbind(data_pred2$year, data_pred2$smk, data_pred2$cum_smoke,data_pred2$fev1_0,pred3,se3,lower3,upper3)
   # Note: We uses baseline FEV1 to predict future FEV1, so baseline FEV1 should be set to original value, se should be 0
   data_pred_fin$pred3[data_pred_fin$year==0]<-data_pred_fin$fev1_0 
   data_pred_fin$se3[data_pred_fin$year==0]<-0
   data_pred_fin$lower3[data_pred_fin$year==0]<-data_pred_fin$pred3
   data_pred_fin$upper3[data_pred_fin$year==0]<-data_pred_fin$pred3
}
