
#Script to create the survival model with deep features and the AUC and NRI comparison with REGICOR risk function

rm(list=ls())
library(gdata)
library(readr)
library(chron)
library(car)
library(Epi)
library(nricens)
library(survival)
library(gtsummary) 
library(survIDINRI) 


load("Mydata.RData")#load data called 'subdat'

###
### Create the Model 0: The Survival model with REGICOR Risk variables (REGICOR risk fucntion)
###
mod0 <- coxph(Surv(censurdays, event) ~ sex + age + coltot +hdl + sbp +dbp + diab +smoke, data = subdat)

# censurdays: days until the last contact 
# event: dicotomic variable (1= event, 0 =no event)
# sex: 1=Men, otherwise Women 
# age: in years
# coltot: Total cholesterol in mg/dL
# hdl: HDL cholesterol in mg/dL
# sbp: Systolic blood preassure in mmHg
# dbp: Diastolic blood preassure in mmHg
# diab: Diabetes (1=yes, 0=no)
# smoke: Former smoker <1 year or current (1=yes, 0=no)


###
### Create the Model 1: The Survival model with REGICOR Risk variables and deep CNN-Mask features extracted 
###     from a segmentation model which produces a mask of the Carotid Intima Media region
###
mod1 <- update(mod0, .~. +X1pca85_imgs_c+X2pca85_imgs_c+X1pca70_imgs_b+X2pca70_imgs_b)

# 4 deep CNN-Mask features from the semantic segmentation model and transformed by PCA:
# X1pca85_imgs_c, X2pca85_imgs_c: Deep features from Common Carotid Artery and transformed with PCA applying 85% of variance.
# X1pca70_imgs_b, X2pca70_imgs_b: Deep features from Bulb Artery and transformed with PCA applying 70% of variance.


####
#### AUC comparison
####

#### AUC Model 0
concorResu0 <- concordance(mod0, timewt="n")
AUC0 <- concorResu0$concordance
AUCsd0 <- sqrt(concorResu0$var)
IC95low0<-round(AUC0 - qnorm(1 - 0.05/2) * AUCsd0, 4)
IC95upp0<-round(AUC0 + qnorm(1 - 0.05/2) * AUCsd0, 4)


### AUC Model 1
concorResu1 <- concordance(mod1, timewt="n")
AUC1 <- concorResu1$concordance
AUCsd1 <- sqrt(concorResu1$var)
IC95low1<-round(AUC1 - qnorm(1 - 0.05/2) * AUCsd1, 4)
IC95upp1<-round(AUC1 + qnorm(1 - 0.05/2) * AUCsd1, 4)


inc_AUC<-round(AUC1-AUC0,4)# The increment of AUC using Model 1 compared to Model 0

ctest <- concordance(mod0regi,mod1)
contr <- c(-1, 1)
dtest <- contr %*% coef(ctest)
dvar <- contr %*% vcov(ctest) %*% contr
x<-c(contrast=dtest, sd=sqrt(dvar), z=dtest/sqrt(dvar))
pvalAUC<-round(1-pnorm(x[3]),3)*2 # The significance level in the increment of AUC with Model 1 compared to Model 0


####
#### NRI calculation
####

moment0 <- 3000 # time (in days) for the preditcted risk
# 'moment0' is the number of the days of the follow-up (it depends on your 'censurdays' variable)
# For example, in a 10 years follow-up  it should be moment0<-365.25*10
# Reduce the number of moment0 if no individual reaches the end of the follow-up, it may result in an error.

p.std = get.risk.coxph(mod0, t0=moment0)
p.new = get.risk.coxph(mod1, t0=moment0)

set.seed(11111) # set a seed for the 'nricens' fucntion

### Categorical Net Reclassification Improvement (NRI)
nri_cut<-nricens(time = subdat$censurdays, event = subdat$event, p.std = p.std, p.new = p.new,
                 t0 = moment0, cut = c(0.05,0.1,0.15),  niter = 1000)#
# 0.05,0.1,0.15 correspond to the categories [0,5%), [5%,10%), [10%,15%), >=15%


### Total NRI
nri1<-round(nri_cut$nri[1,1],4)*100 #Ttoal NRI value
IC95_nriLow1<-round(nri_cut$nri[1,2],4) # Low value of the Confidence Interval for total NRI
IC95_nriUp1<-round(nri_cut$nri[1,3],4)  # High value of the Confidence Interval for total NRI

### NRI only for cases (events)
nri2<-round(nri_cut$nri[2,1],4)*100  #NRI value in cases 
IC95_nriLow2<-round(nri_cut$nri[2,2],4) # Low value of the Confidence Interval for NRI cases
IC95_nriUp2<-round(nri_cut$nri[2,3],4)  # High value of the Confidence Interval for  NRI cases

### NRi only for controls (no events)
nri3<-round(nri_cut$nri[3,1],4)*100  #NRI value in controls 
IC95_nriLow3<-round(nri_cut$nri[3,2],4) # Low value of the Confidence Interval for NRI control
IC95_nriUp3<-round(nri_cut$nri[3,3],4)  # High value of the Confidence Interval for NRI control