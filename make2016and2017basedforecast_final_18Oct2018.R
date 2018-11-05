###========================================================
### Simplified script just to do forecasts from 2017 predictors. 
### Also backs up to do the 2016-based forecasts, but from the dataset
### constructed for 2017. 
### 9 July 2018
### 18 October 2018 -- re-run with new data incorporating 2017 updated polity/durable. 
###========================================================

library(glmnet)
library(dplyr)
set.seed(12345)
load("prepared_data_final_15Oct2018.RData")
dat=as.data.frame(dat)

###===============================================================
### Doing actual analyses:
###==============================================================
outcomenames=c("anymk.start.1", "anymk.start.2window")

# Note change to anymk.ongoing and anymk.ever (from mkl.ongoing and mkl.ever)
predictornames = c("anymk.ongoing","anymk.ever",
          "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", #americas, "reg.amr" left out as baseline.
          "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
          "ios.iccpr1","includesnonstate",
          "durable.ln","minorityrule", "elf.ethnic", "battledeaths.ln",
          "candidaterestriction", "partyban","judicialreform",
          "religiousfreedom", "pol_killing_approved",
          "freemove_men4","freemove_women4", "freediscussion",
          "social_inequality","even_civilrights","repress_civilsoc","social_power_dist", 
          "tradeshare.ln.combined", 
          "coup.try.5yr",
          "polity2.fl.2","polity2.fl.3")

table(dat$anymk.start.1, dat$year)
table(dat$anymk.start.2window, dat$year)

# Exploring and checking
#a=dat[,c(predictornames, "year","country_name")]
#View(a[a$year>=2016,c("country_name","year","durable.ln")])
#dat %>% filter(country_name=="Afghanistan" & year>=2015)%>% select(c("year",predictornames))%>%View()

###===========================================
###  Run! 2017-based
###===========================================
foo=dat[,c("sftgcode","year", outcomenames, predictornames, "country_name","year")]

### Train on all, though 2016 drops 
yXtrain2017=na.omit(dat[,c("sftgcode","year", outcomenames, predictornames, "country_name","year")])
Xtrain2017=yXtrain2017[,predictornames]
ytrain2017_1yr=yXtrain2017[,outcomenames[1]] #single leaded year outcome
ytrain2017_2yr=yXtrain2017[,outcomenames[2]] # two year window outcome
table(yXtrain2017$year)

# Prediction time data:
Xtest2017=na.omit(dat[dat$year==2017,c("sftgcode","COWcode",predictornames,"country_name","year")])
testcountries2017.clean = Xtest2017$country_name
table(Xtest2017$year)

# Single leaded year model
alpha=.5
elastic.cv.2017.1yr=cv.glmnet(y=as.numeric(ytrain2017_1yr),  
                          x=as.matrix(Xtrain2017[,predictornames]), 
                             alpha=alpha, family="binomial")

plot(elastic.cv.2017.1yr)
(coefsfull.2017.1yr=coef(elastic.cv.2017.1yr, s="lambda.min"))

elastic.predictions.2017.1yr = signif(predict.cv.glmnet(elastic.cv.2017.1yr, 
        newx=as.matrix(Xtest2017[,predictornames]), s="lambda.min", type="response"),4)

Xtest2017$risk2017.1yr=as.numeric(elastic.predictions.2017.1yr)

# Two-year leaded window model
elastic.cv.2017.2yr=cv.glmnet(y=as.numeric(ytrain2017_2yr),  
                          x=as.matrix(Xtrain2017[,predictornames]), 
                          alpha=alpha, family="binomial")

plot(elastic.cv.2017.2yr)
(coefsfull.2017.2yr=coef(elastic.cv.2017.2yr, s="lambda.min"))

coefsboth2017 = cbind(coefsfull.2017.1yr,coefsfull.2017.2yr)
round(coefsboth2017,4)

elastic.predictions.2017.2yr = signif(predict.cv.glmnet(elastic.cv.2017.2yr, 
                                                newx=as.matrix(Xtest2017[,predictornames]), s="lambda.min", type="response"),4)

Xtest2017$risk2017.2yr=as.numeric(elastic.predictions.2017.2yr)

everything2017 = Xtest2017 %>% 
  select(country_name, sftgcode, COWcode, risk2017.1yr, risk2017.2yr, predictornames) %>% 
  arrange(desc(risk2017.2yr))

colnames(everything2017)=c("country","SFTGcode","COW","risk_in_2018",
                           "risk_2018_19",predictornames)
write.csv(x = everything2017, file="sortedrisk_2017based_1and2yr_15Oct2018.csv" )


###=========================================
### Repeat for the 2016-based forecasts
### Almost the same but drops 2015 from training.
###==========================================
### Train on all, though drop 2015. 
yXtrain2016=na.omit(dat[,c("sftgcode","year", outcomenames, predictornames, "country_name","year")])
yXtrain2016 = yXtrain2016 %>% filter(year<=2014)
Xtrain2016=yXtrain2016[,predictornames]
ytrain2016_1yr=yXtrain2016[,outcomenames[1]] #single leaded year outcome
ytrain2016_2yr=yXtrain2016[,outcomenames[2]] # two year window outcome
table(yXtrain2016$year)

# Prediction time data:
Xtest2016=na.omit(dat[dat$year==2016,c("sftgcode","COWcode",predictornames,"country_name","year")])
testcountries2016.clean = Xtest2016$country_name

table(Xtest2016$year)

# Single leaded year model
alpha=.5
elastic.cv.out1=cv.glmnet(y=as.numeric(ytrain2016_1yr),  
                          x=as.matrix(Xtrain2016[,predictornames]), 
                          alpha=alpha, family="binomial")

plot(elastic.cv.out1)
(coefsfull.2016.1yr=coef(elastic.cv.out1, s="lambda.min"))

elastic.predictions1 = signif(predict.cv.glmnet(elastic.cv.out1, 
                                                newx=as.matrix(Xtest2016[,predictornames]), s="lambda.min", type="response"),4)

Xtest2016$risk1=as.numeric(elastic.predictions1)

# Two-year leaded window model
elastic.cv.out2=cv.glmnet(y=as.numeric(ytrain2016_2yr),  
                          x=as.matrix(Xtrain2016[,predictornames]), 
                          alpha=alpha, family="binomial")

plot(elastic.cv.out2)
(coefsfull.2016.2yr=coef(elastic.cv.out2, s="lambda.min"))

coefsboth.2016 = cbind(coefsfull.2016.1yr,coefsfull.2016.2yr)
round(coefsboth.2016,4)

elastic.predictions2 = signif(predict.cv.glmnet(elastic.cv.out2, 
                                                newx=as.matrix(Xtest2016[,predictornames]), s="lambda.min", type="response"),4)

Xtest2016$risk2=as.numeric(elastic.predictions2)

everything2016 = Xtest2016 %>% 
  select(country_name, sftgcode, COWcode, risk1, risk2, predictornames) %>% 
  arrange(desc(risk2))
colnames(everything2016)=c("country","SFTGcode","COW","risk_in_2017",
                           "risk_2017_18",predictornames)
View(everything2016)

write.csv(x = everything2016, file="sortedrisk_2016based_1and2yr_15Oct2018.csv" )

save.image(file = "WorkspaceImageAfterModeling_15Oct2018.RData")
