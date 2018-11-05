###=======================================
### Model testing script
### Based on earlier versions that were done prior
### to seeing the new data. This one uses the "actual"
### data from the 2018 forecasting work (i.e. 2017-based)
###========================================
rm(list=ls())

library(glmnet)
library(randomForest)
library(xgboost)
library(pROC)
#devtools::install_github('lukesonnet/KRLS')
#library(KRLS2)
library(dplyr)

load("prepared_data_final_15Oct2018.RData")
dat=as.data.frame(dat)
table(dat$year)

###===========================================
### Main training and testing function
###===========================================
# Write a general function that will take in a training set and test set, 
# do all the training and prediction, so that it can be called by a cross-validation
# routine or for full blown prediction.

trainandtest = function(yXtrain, Xtest, predictornames, outcomename){

  #yX will be a dataset whose first column is the outcome to be predicted. 
  yXtrain=yXtrain[,c(outcomename,predictornames)]
  yXtrain=na.omit(yXtrain)
  ytrain=yXtrain[,1]
  Xtrain=yXtrain[,-1]

  Xtest=na.omit(Xtest)

  #For algorithms that like a formula as input:
  rhs=paste0(predictornames,collapse = "+")
  f1=paste0(outcomename,"~",rhs)
  
  ### GLM (logit)
  print("Running GLM")
  glm.train=glm(f1, data=yXtrain, family="binomial")
  glm.train.predictions=glm.train$fitted.values
  glm.test.predictions=predict(glm.train, newdata = Xtest, type="response")
  
  ### Elastic net at lambda.min and at lambda.1se
  print("Running elastic net")
  elastic.cv.out=cv.glmnet(y=as.numeric(ytrain),  x=as.matrix(Xtrain), 
                           alpha=.5, family="binomial")
  ### fitted elastic-logit:
  elastic.1se.out=coef(elastic.cv.out, s=elastic.cv.out$lambda.1se)
  elastic.min.out=coef(elastic.cv.out, s=elastic.cv.out$lambda.min)
  
  #plot(elastic.cv.out)
  elastic.train.1se.predictions = predict.cv.glmnet(elastic.cv.out, newx=as.matrix(Xtrain), s="lambda.1se", type="response")
  elastic.test.1se.predictions = predict.cv.glmnet(elastic.cv.out, newx=as.matrix(Xtest), s="lambda.1se", type="response")

  elastic.train.min.predictions = predict.cv.glmnet(elastic.cv.out, newx=as.matrix(Xtrain), s="lambda.min", type="response")
  elastic.test.min.predictions = predict.cv.glmnet(elastic.cv.out, newx=as.matrix(Xtest), s="lambda.min", type="response")
  

  ### Lasso at lambda.min and at lambda.1se
  print("Running LASSO")
  lasso.cv.out=cv.glmnet(y=as.numeric(ytrain),  x=as.matrix(Xtrain), 
                           alpha=1, family="binomial")
  ### fitted elastic-logit:
  lasso.1se.out=coef(elastic.cv.out, s=lasso.cv.out$lambda.1se)
  lasso.min.out=coef(elastic.cv.out, s=lasso.cv.out$lambda.min)
  
  #plot(elastic.cv.out)
  lasso.train.1se.predictions = predict.cv.glmnet(lasso.cv.out, newx=as.matrix(Xtrain), s="lambda.1se", type="response")
  lasso.test.1se.predictions = predict.cv.glmnet(lasso.cv.out, newx=as.matrix(Xtest), s="lambda.1se", type="response")
  
  lasso.train.min.predictions = predict.cv.glmnet(lasso.cv.out, newx=as.matrix(Xtrain), s="lambda.min", type="response")
  lasso.test.min.predictions = predict.cv.glmnet(lasso.cv.out, newx=as.matrix(Xtest), s="lambda.min", type="response")
  
  ### Randomforest
  print("Running RF")
  # Reducing ntree from 1000 (previously used) to
  # 500 for speed
  rf.train.out = randomForest(y=as.factor(ytrain), x=Xtrain, ntree=500,mtry=3)
  rf.train.predictions = predict(rf.train.out, type = "prob", newdata = Xtrain, na.action = na.exclude)[,2]
  rf.test.predictions = predict(rf.train.out, type = "prob", newdata = Xtest, na.action = na.exclude)[,2]
  
  ### xgboost
  print("Running XGBoost")
  xgb.out = xgboost(data = data.matrix(Xtrain), 
                    label = ytrain, nrounds = 10, objective = "binary:logistic")
  xgb.train.predictions = predict(xgb.out, data.matrix(Xtrain))
  xgb.test.predictions = predict(xgb.out, data.matrix(Xtest))
  
  
  ### KRLogit and KRLS disabled to save running time.
  
  ### KRlogit 
#  print("Running KRLogit")
#  krlogit.out = KRLS2::krls(y=ytrain, X=Xtrain, loss = "logistic", epsilon = .01, lambda=1e-4,b=1*35)
#  krlogit.out = KRLS2::krls(y=ytrain, X=Xtrain, loss = "logistic", epsilon = .01)
#  krlogit.train.predictions = krlogit.out$fitted
#  krlogit.test.predictions = predict(krlogit.out, newdata = Xtest)$fit
  
  ### KRLS 
  #print("Running KRLS")
  #krls.out = KRLS2::krls(y=ytrain, 
  #                       X=Xtrain, loss = "leastsquares", epsilon = .001, lambda=1)
  #krls.train.predictions = krls.out$fitted
  #krls.test.predictions = predict(krls.out, newdata = Xtest)$fit
  
  ### Compile results
  trainresults=cbind(ytrain, glm.train.predictions, elastic.train.1se.predictions,
                     elastic.train.min.predictions, lasso.train.1se.predictions,
                     lasso.train.min.predictions,
                     rf.train.predictions, xgb.train.predictions)
                      #, krlogit.train.predictions)#, krls.train.predictions)

  colnames(trainresults)=c("ytrain","glm","elastic.1se","elastic.min",
                           "lasso.1se","lasso.min","rf","xgb") #,"krlogit")#,"krls")
  
  testresults=cbind(glm.test.predictions, elastic.test.1se.predictions, elastic.test.min.predictions,
                    lasso.test.1se.predictions, lasso.test.min.predictions,
                    rf.test.predictions, xgb.test.predictions)
                    #, krlogit.test.predictions)#, krls.test.predictions)

  colnames(testresults)=c("glm","elastic.1se","elastic.min","lasso.1se","lasso.min","rf","xgb") #,"krlogit")#,"krls")
  
  R=list()
  R$trainresults=trainresults
  R$testresults=testresults
  #R$krlogit.out=krlogit.out
  R$elastic.1se.out=elastic.1se.out
  return(R)
}

###===============================================================================
### Function to do various performance metrics: AUC, discrim-factor, discrim-add
###===============================================================================

fitmeasures = function(y,yhat, threshold=0.05){
  roc.out=roc(response=as.numeric(y), predictor=yhat)
  auc=roc.out$auc

  factorincrease=mean(yhat[y==1])/mean(yhat[y==0])
  
  addincrease=mean(yhat[y==1])-mean(yhat[y==0])
  
  #Accuracy if we make positive predictions for those at some risk level 
  pospredict = as.numeric(yhat>=threshold)
  accuracy = mean(pospredict==y)
  FPtoTP = sum(pospredict==1 & y==0)/sum(pospredict==1 & y==1)
  missrate = sum(y==1 & pospredict==0)/sum(y==1)
  R=c(auc,factorincrease,addincrease, accuracy, FPtoTP, missrate)
  names(R) =c("auc","factorincrease","addincrease", "accuracy","FPtoTP","missrate")
  return(R)
}


###===============================================================
### Doing actual analyses:
###==============================================================


outcomenames=c("anymk.start.1", "anymk.start.2window")
# Drop 3-year-window, since we don't use it and it causes
# additional missingness.

# Predictornames -- those actually used in 2018 modeling (2017-based forecasts)
predictornames = c("anymk.ongoing","anymk.ever",
                   "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", #americas, "reg.amr" left out as baseline.
                   "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
                   "ios.iccpr1","includesnonstate",
                   #"polity2", #"polity2_sq",
                   "durable.ln","minorityrule", "elf.ethnic", "battledeaths.ln",
                   "candidaterestriction", "partyban","judicialreform",
                   "religiousfreedom", "pol_killing_approved",
                   "freemove_men4","freemove_women4", "freediscussion",
                   "social_inequality","even_civilrights","repress_civilsoc","social_power_dist", 
                   #"elc.eleth2",  #testing to see if it helps given "minorityrule" from vdem
                   #"barrierstoparties",
                   "tradeshare.ln.combined",   #consider dropping due to missingness
                   "coup.try.5yr",
                   # "mev.regac.ln",  #out of service
                   #"polity2.fl.1",
                   "polity2.fl.2","polity2.fl.3")  #these lasts two currently as carry-forwards

# For posterity -- predictornames in use at etime of original model testing and choosing. 
#predictornames = c("anymk.ongoing","anymk.ever",
#          "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", #americas, "reg.amr" left out as baseline.
#          "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
#          "ios.iccpr1","includesnonstate",
#          "durable.ln","minorityrule", "elf.ethnic", "battledeaths.ln",
#          "candidaterestriction", "partyban","judicialreform",
#          "religiousfreedom", "pol_killing_approved",
#          "freemove_men4","freemove_women4", "freediscussion",
#          "social_inequality","even_civilrights","repress_civilsoc","social_power_dist", 
#          "tradeshare.ln.combined",   #consider dropping due to missingness
#          "cou.tries5d",
#          "polity2.fl.2","polity2.fl.3")  #these lasts two currently as carry-forwards

table(dat$anymk.start.1, dat$year)
table(dat$anymk.start.2window, dat$year)

# Clean dataset, no missingness.
yXall=na.omit(dat[,c(outcomenames,predictornames, "country_name", "year")])

###========================================================
### Forecaster's LOO, used for primary testing.
###=========================================================

yXall=na.omit(dat[,c("sftgcode","year",outcomenames,predictornames, "country_name")])
yXtrain=yXall 
yXtrain=na.omit(yXtrain)
ytrain=yXtrain[,outcomenames]
Xtrain=yXtrain[,predictornames]
ytest1.glm=ytest1.elastic.1se=ytest1.elastic.min=ytest1.lasso.1se=ytest1.lasso.min=ytest1.rf=ytest1.xgb=c()
ytest2.glm=ytest2.elastic.1se=ytest2.elastic.min=ytest2.lasso.1se=ytest2.lasso.min=ytest2.rf=ytest2.xgb=c()
#ytest3.glm=ytest3.elastic.1se=ytest3.elastic.min=ytest3.lasso.1se=ytest3.lasso.min=ytest3.rf=ytest3.xgb=c()
ytest.allyears1=c()
ytest.allyears2=c()
#ytest.allyears3=c()

# Testing: prediction starts from 2 years later, so that the window
# Can "clear".  Would need to skip 3 is doing the three year window as well.
# Discontinuing the three year window anyhow. 

for (thisyear in 1991:2011){   # Used 1991 to 2011 in original testing, replicate here
  print(thisyear)
  yXtrain=yXall[yXall$year<=thisyear,]
  yXtest=yXall[yXall$year==(thisyear+2),]
  
  Xtest=yXtest[,predictornames]
  ytest1=yXtest[,outcomenames[1]]
  ytest2=yXtest[,outcomenames[2]]
  #ytest3=yXtest[,outcomenames[3]]
  
  ytest.allyears1=c(ytest.allyears1,ytest1)
  ytest.allyears2=c(ytest.allyears2,ytest2)
  #ytest.allyears3=c(ytest.allyears3,ytest3)
  
  traintest.thisyear1 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename=outcomenames[1])
  traintest.thisyear2 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename=outcomenames[2])
  #traintest.thisyear3 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename=outcomenames[3])
  
  ytest1.glm=c(ytest1.glm,traintest.thisyear1$testresults[,"glm"])  
  ytest1.elastic.1se=c(ytest1.elastic.1se,traintest.thisyear1$testresults[,"elastic.1se"])  
  ytest1.elastic.min=c(ytest1.elastic.min,traintest.thisyear1$testresults[,"elastic.min"])  
  ytest1.lasso.1se=c(ytest1.lasso.1se,traintest.thisyear1$testresults[,"lasso.1se"])  
  ytest1.lasso.min=c(ytest1.lasso.min,traintest.thisyear1$testresults[,"lasso.min"])  
  ytest1.rf=c(ytest1.rf,traintest.thisyear1$testresults[,"rf"])  
  ytest1.xgb=c(ytest1.xgb,traintest.thisyear1$testresults[,"xgb"])  
  
  ytest2.glm=c(ytest2.glm,traintest.thisyear2$testresults[,"glm"])  
  ytest2.elastic.1se=c(ytest2.elastic.1se,traintest.thisyear2$testresults[,"elastic.1se"])  
  ytest2.elastic.min=c(ytest2.elastic.min,traintest.thisyear2$testresults[,"elastic.min"])  
  ytest2.lasso.1se=c(ytest2.lasso.1se,traintest.thisyear2$testresults[,"lasso.1se"])  
  ytest2.lasso.min=c(ytest2.lasso.min,traintest.thisyear2$testresults[,"lasso.min"])  
  ytest2.rf=c(ytest2.rf,traintest.thisyear2$testresults[,"rf"])  
  ytest2.xgb=c(ytest2.xgb,traintest.thisyear2$testresults[,"xgb"])  
  
  # ytest3.glm=c(ytest3.glm,traintest.thisyear3$testresults[,"glm"])  
  # ytest3.elastic.1se=c(ytest3.elastic.1se,traintest.thisyear3$testresults[,"elastic.1se"])  
  # ytest3.elastic.min=c(ytest3.elastic.min,traintest.thisyear3$testresults[,"elastic.min"])  
  # ytest3.lasso.1se=c(ytest3.lasso.1se,traintest.thisyear3$testresults[,"lasso.1se"])  
  # ytest3.lasso.min=c(ytest3.lasso.min,traintest.thisyear3$testresults[,"lasso.min"])  
  # ytest3.rf=c(ytest3.rf,traintest.thisyear3$testresults[,"rf"])  
  # ytest3.xgb=c(ytest3.xgb,traintest.thisyear3$testresults[,"xgb"])  
}

threshold=0.05

results2.foos.glm = fitmeasures(y=ytest.allyears2, yhat=ytest2.glm, threshold=threshold)
results2.foos.elastic.1se = fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.1se, threshold=threshold)
results2.foos.elastic.min = fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold)
results2.foos.lasso.1se = fitmeasures(y=ytest.allyears2, yhat=ytest2.lasso.1se, threshold=threshold)
results2.foos.lasso.min = fitmeasures(y=ytest.allyears2, yhat=ytest2.lasso.min, threshold=threshold)
results2.foos.rf = fitmeasures(y=ytest.allyears2, yhat=ytest2.rf, threshold=threshold)
results2.foos.xgb = fitmeasures(y=ytest.allyears2, yhat=ytest2.xgb, threshold=threshold)
results.foos.2yr = rbind(results2.foos.glm, results2.foos.elastic.1se, 
                         results2.foos.elastic.min, results2.foos.lasso.1se, 
                         results2.foos.lasso.min, results2.foos.rf,                    results2.foos.xgb)
signif(results.foos.2yr[,-4],3)
signif(results2.foos.elastic.min[-4],3)


threshold.seq=c(0.015,.02, .025, .03, .04, .05)

a=fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[1])
a=rbind(a, fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[2]))
a=rbind(a, fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[3]))
a=rbind(a, fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[4]))
a=rbind(a, fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[5]))
a=rbind(a, fitmeasures(y=ytest.allyears2, yhat=ytest2.elastic.min, threshold=threshold.seq[6]))

a=cbind(threshold.seq, a)
round(a[,-5],3)


results1.foos.glm = fitmeasures(y=ytest.allyears1, yhat=ytest1.glm, threshold=threshold)
results1.foos.elastic.1se = fitmeasures(y=ytest.allyears1, yhat=ytest1.elastic.1se, threshold=threshold)
results1.foos.elastic.min = fitmeasures(y=ytest.allyears1, yhat=ytest1.elastic.min, threshold=threshold)
results1.foos.lasso.1se = fitmeasures(y=ytest.allyears1, yhat=ytest1.lasso.1se, threshold=threshold)
results1.foos.lasso.min = fitmeasures(y=ytest.allyears1, yhat=ytest1.lasso.min, threshold=threshold)
results1.foos.rf = fitmeasures(y=ytest.allyears1, yhat=ytest1.rf, threshold=threshold)
results1.foos.xgb = fitmeasures(y=ytest.allyears1, yhat=ytest1.xgb, threshold=threshold)
results.foos.1yr = rbind(results1.foos.glm, results1.foos.elastic.1se, 
                         results1.foos.elastic.min, results1.foos.lasso.1se, 
                         results1.foos.lasso.min, results1.foos.rf, 
                         results1.foos.xgb)
signif(results.foos.1yr[,-4],3)


# results.foos.3yr = rbind(results3.foos.glm, results3.foos.elastic.1se, 
#                          results3.foos.elastic.min, results3.foos.lasso.1se, 
#                          results3.foos.lasso.min, results3.foos.rf, 
#                          results3.foos.xgb)
# colnames(results.foos.3yr)=c("auc","factorincrease","addincrease", "accuracy05","FPtoTP05","missrate")



#signif(results.foos.3yr,3)


###==========================================
### 5-Fold CV
###==========================================
 
outcomename="anymk.start.2window"  # For 2-year window

### Setup cross-validation
set.seed(54321) #new seeed from what was used in training-only version)
cvfold=sample(x = seq(1:5),replace = TRUE, size = nrow(yXall))

# Fold 1
yXtrain=yXall[cvfold!=1,]
ytrainfold1=yXtrain[,outcomename]
yXtest=yXall[cvfold==1,]
Xtest=yXtest[,predictornames]
ytestfold1=yXtest[,outcomename]
fold1 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames,outcomename =outcomename)

# Fold 2
yXtrain=yXall[cvfold!=2,]
ytrainfold2=yXtrain[,outcomename]
yXtest=yXall[cvfold==2,]
Xtest=yXtest[,predictornames]
ytestfold2=yXtest[,outcomename]
fold2 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames,outcomename =outcomename)

# Fold 3
yXtrain=yXall[cvfold!=3,]
ytrainfold3=yXtrain[,outcomename]
yXtest=yXall[cvfold==3,]
Xtest=yXtest[,predictornames]
ytestfold3=yXtest[,outcomename]
fold3 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames,outcomename =outcomename)

# Fold 4
yXtrain=yXall[cvfold!=4,]
ytrainfold4=yXtrain[,outcomename]
yXtest=yXall[cvfold==4,]
Xtest=yXtest[,predictornames]
ytestfold4=yXtest[,outcomename]
fold4 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames,outcomename =outcomename)

# Fold 5
yXtrain=yXall[cvfold!=5,]
ytrainfold5=yXtrain[,outcomename]
yXtest=yXall[cvfold==5,]
Xtest=yXtest[,predictornames]
ytestfold5=yXtest[,outcomename]
fold5 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames,outcomename =outcomename)

#All test-results
alltest=rbind(fold1$testresults, fold2$testresults,fold3$testresults, fold4$testresults, fold5$testresults)
alltrain=rbind(fold1$trainresults, fold2$trainresults,fold3$trainresults, fold4$trainresults, fold5$trainresults)
ytestall=c(ytestfold1, ytestfold2, ytestfold3, ytestfold4, ytestfold5)
ytrainall=c(ytrainfold1, ytrainfold2, ytrainfold3, ytrainfold4, ytrainfold5)

#out-of-sample
glm.fit.full=fitmeasures(y=ytestall, yhat=alltest[,"glm"])
rf.fit.full=fitmeasures(y=ytestall, yhat=alltest[,"rf"])
elastic.1se.fit.full=fitmeasures(y=ytestall, yhat=alltest[,"elastic.1se"])
elastic.min.fit.full=fitmeasures(y=ytestall, yhat=alltest[,"elastic.min"])
xgb.fit.full=fitmeasures(y=ytestall, yhat=alltest[,"xgb"])

results.cv=rbind(glm.fit.full, elastic.1se.fit.full,elastic.min.fit.full,
                 rf.fit.full, xgb.fit.full)
colnames(results.cv)=c("auc","factorincrease","addincrease", 
                      "accuracy", "FPtoTP","missrate")

signif(results.cv,3)


###========================================================
### Train up through 2001, test on rest. 
###========================================================

# With one year lead:
yXall=na.omit(dat[,c("sftgcode","year","anymk.start.1",predictornames, "country_name","year")])
yXtrain=yXall[yXall$year<=2001,]
yXtest=yXall[yXall$year>2001,]
ytrain=yXtrain[,outcomenames[1]]
Xtest=yXtest[,predictornames]
ytest=yXtest[,outcomenames[1]]

traintest2001 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename=outcomename)

# takes a while
#(krlogit2001.summary=summary(traintest2001$krlogit.out))
#round(krlogit2001.summary$sumavgderiv, 3)

glm.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"glm"])
elastic.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.1se"])
elastic.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.min"])
lasso.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.1se"])
lasso.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.min"])
rf.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"rf"])
xgb.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"xgb"])
#krlogit.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"krlogit"])
#ave.fit = fitmeasures(y=ytest, yhat=test.ave)

results.prepost2001=rbind(glm.fit,rf.fit, elastic.1se.fit, elastic.min.fit, lasso.1se.fit, lasso.min.fit, xgb.fit) # krlogit.fit, ave.fit) #krls.fit, ave.fit)
colnames(results.prepost2001)=c("auc","factorincrease","addincrease")
signif(results.prepost2001,3)

yXtest.yhat.1=cbind(yXtest, traintest2001$testresults)


### Repeat with 2 year window:
# With one year lead:
yXall=na.omit(dat[,c("sftgcode","year","anymk.start.2window",predictornames, "country_name","year")])
yXtrain=yXall[yXall$year<=2001,]
yXtest=yXall[yXall$year>2001,]
ytrain=yXtrain[,"anymk.start.2window"]
Xtest=yXtest[,predictornames]
ytest=yXtest[,"anymk.start.2window"]

traintest2001 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename="anymk.start.2window")

threshold=0.1
glm.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"glm"], threshold = threshold)
elastic.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.1se"], threshold = threshold)
elastic.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.min"], threshold = threshold)
lasso.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.1se"], threshold = threshold)
lasso.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.min"], threshold = threshold)
rf.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"rf"], threshold = threshold)
xgb.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"xgb"], threshold = threshold)
#krlogit.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"krlogit"])
#ave.fit = fitmeasures(y=ytest, yhat=test.ave)

results.prepost2001.2yr=rbind(glm.fit,rf.fit, elastic.1se.fit, elastic.min.fit, lasso.1se.fit, lasso.min.fit, xgb.fit) # krlogit.fit, ave.fit) #krls.fit, ave.fit)
#colnames(results.prepost2001.2yr)=c("auc","factorincrease","addincrease")
signif(results.prepost2001.2yr,3)

yXtest.yhat.2=cbind(yXtest, traintest2001$testresults)

### Repeat with 3 year window:
# With one year lead:
yXall=na.omit(dat[,c("sftgcode","year","anymk.start.3window",predictornames, "country_name","year")])
yXtrain=yXall[yXall$year<=2001,]
yXtest=yXall[yXall$year>2001,]
ytrain=yXtrain[,"anymk.start.3window"]
Xtest=yXtest[,predictornames]
ytest=yXtest[,"anymk.start.3window"]

traintest2001 = trainandtest(yXtrain = yXtrain, Xtest = Xtest, predictornames = predictornames, outcomename="anymk.start.3window")

glm.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"glm"])
elastic.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.1se"])
elastic.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"elastic.min"])
lasso.1se.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.1se"])
lasso.min.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"lasso.min"])
rf.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"rf"])
xgb.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"xgb"])
#krlogit.fit = fitmeasures(y=ytest, yhat=traintest2001$testresults[,"krlogit"])
#ave.fit = fitmeasures(y=ytest, yhat=test.ave)

results.prepost2001.3yr=rbind(glm.fit,rf.fit, elastic.1se.fit, elastic.min.fit, lasso.1se.fit, lasso.min.fit, xgb.fit) # krlogit.fit, ave.fit) #krls.fit, ave.fit)
colnames(results.prepost2001.3yr)=c("auc","factorincrease","addincrease")
signif(results.prepost2001.3yr,3)

yXtest.yhat.3=cbind(yXtest, traintest2001$testresults)

### For various interpretations, run on full data:
yXall=na.omit(dat[,c("sftgcode","year",outcomename,predictornames, "country_name")])
yXtrain=yXall #n=yXall[,c(outcomename,predictornames)]
yXtrain=na.omit(yXtrain)
ytrain=yXtrain[,outcomename]
Xtrain=yXtrain[,predictornames]

krlogit.full.out = KRLS2::krls(y=ytrain, X=Xtrain, loss = "logistic", epsilon = .001)
krlogit.full.predictions = krlogit.full.out$fitted
(krlogit.full.summary=summary(krlogit.full.out))
round(krlogit.full.summary$sumavgderiv, 4)

### For various interpretations, see how variables selected by logit or elastic net 
### vary across years. 

alpha=.5
yXtrain2013=filter(yXtrain, year<=2013)
ytrain2013=yXtrain2013[,outcomename]
Xtrain2013=yXtrain2013[,predictornames]

elastic2013.cv.out=cv.glmnet(y=as.numeric(ytrain2013),  x=as.matrix(Xtrain2013[,predictornames]), 
                             alpha=alpha, family="binomial")

(coefs2013=coef(elastic2013.cv.out, s=elastic2013.cv.out$lambda.min))

elastic.predictions.2013 = signif(predict.cv.glmnet(elastic2013.cv.out, 
        newx=as.matrix(Xtrain2013[,predictornames]), s="lambda.min", type="response"),4)

yXtrain2013$risk=as.numeric(elastic.predictions.2013)
sortedrisk2001_2013model = yXtrain2013 %>% filter(year==2001) %>% select(country_name, risk) %>% arrange(desc(risk))
sortedrisk2001_2013model[1:10,]
colnames(sortedrisk2001_2013model)=c("country","risk")

### 2014 trained
yXtrain2014=filter(yXtrain, year<=2014)
ytrain2014=yXtrain2014[,outcomename]
Xtrain2014=yXtrain2014[,predictornames]

elastic2014.cv.out=cv.glmnet(y=as.numeric(ytrain2014),  
              x=as.matrix(Xtrain2014[,predictornames]), 
                             alpha=alpha, family="binomial")

(coefs2014=coef(elastic2014.cv.out, s=elastic2014.cv.out$lambda.min))

elastic.predictions.2014 = signif(predict.cv.glmnet(elastic2014.cv.out, 
                                             newx=as.matrix(Xtrain2014[,predictornames]), s="lambda.min", type="response"),4)
yXtrain2014$risk=as.numeric(elastic.predictions.2014)
sortedrisk2001_2014model = yXtrain2014 %>% filter(year==2001) %>% select(country_name, risk) %>% arrange(desc(risk))
sortedrisk2001_2014model[1:10,]
colnames(sortedrisk2001_2014model)=c("country","risk")

# For a preview, do 2015 predictions based on model trained up through 2014.
yXtrain2015=filter(yXtrain, year<=2015)
Xtrain2015=yXtrain2015[,predictornames]
elastic.predictions.2015from2014 = signif(predict.cv.glmnet(elastic2014.cv.out, 
                 newx=as.matrix(Xtrain2015[,predictornames]), s="lambda.min", type="response"),4)
yXtrain2015$risk=as.numeric(elastic.predictions.2015)
sortedrisk2015from2014 = yXtrain2015 %>% filter(year==2015) %>% select(country_name, risk) %>% arrange(desc(risk))
colnames(sortedrisk2015from2014)=c("country","risk")
View(sortedrisk2015from2014)

### 2015 trained
yXtrain2015=filter(yXtrain, year<=2015)
ytrain2015=yXtrain2015[,outcomename]
Xtrain2015=yXtrain2015[,predictornames]

elastic2015.cv.out=cv.glmnet(y=as.numeric(ytrain2015),  
                             x=as.matrix(Xtrain2015[,predictornames]), 
                             alpha=.5, family="binomial")

(coefs2015=coef(elastic2015.cv.out, s=elastic2015.cv.out$lambda.1se))
(coefs2015.min=coef(elastic2015.cv.out, s=elastic2015.cv.out$lambda.min))

cbind(coefs2015,coefs2015.min)

elastic.predictions.2015 = signif(predict.cv.glmnet(elastic2015.cv.out, 
                           newx=as.matrix(Xtrain2015[,predictornames]), s="lambda.min", type="response"),4)

yXtrain2015$risk=as.numeric(elastic.predictions.2015)
sortedrisk2001_2015model = yXtrain2015 %>% filter(year==2001) %>% select(country_name, risk) %>% arrange(desc(risk))
sortedrisk2001_2015model[1:10,]
colnames(sortedrisk2001_2015model)=c("country","risk")

sortedrisk2015_2015model = yXtrain2015 %>% filter(year==2015) %>% select(country_name, risk) %>% arrange(desc(risk))
colnames(sortedrisk2015_2015model)=c("country","risk")
View(sortedrisk2015_2015model)

cbind(sortedrisk2001_2013model, 
      sortedrisk2001_2014model, 
      sortedrisk2001_2015model)[1:20,]


### without any regularization, i.e. just logit
coefs2015_noreg=coef(elastic2015.cv.out, s=0)

elastic_coef_matrix= signif(cbind(coefs2013, coefs2014, coefs2015, coefs2015_noreg),4)
colnames(elastic_coef_matrix) = c("train2013","train2014","train2015","train2015_noreg")
elastic_coef_matrix

### And on just post-2001
predictornames2=setdiff(predictornames, "includesnonstate")
yXtrain=yXall[yXall$year>2001,c(outcomename,predictornames2)]
yXtrain=na.omit(yXtrain)
apply(yXtrain[,predictornames2], 2, var)

ytrain=yXtrain[,1]
Xtrain=yXtrain[,-1]
krlogit.2001.out = KRLS2::krls(y=ytrain, X=Xtrain, loss = "logistic", epsilon = .001, b=33)
krlogit.2001.predictions = krlogit.2001.out$fitted
(krlogit.2001.summary=summary(krlogit.2001.out))
round(krlogit.2001.summary$sumavgderiv, 4)
