# causal_analysis week 4
# IPTW

# from the video
library(tableone)
library(ipw)
library(sandwich)  # for robust variance estimation
library(survey)

# =========================================
# read in data
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))
View(rhc)
# create new dataset, convert characters to numeric
# fit propensity score model
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75, 
             family=binomial(link="logit"), data=lalonde)
ps<- predict(psmodel, type="response")
# look at the coefficients
# look at plot of propensity score for treatment & control
# do IPTW
weight<-ifelse(treatment==2, 1/(ps), 1/(1-pz))
# apply weights to data
weighteddata <- svydesign(ids = ~1, data=mydata, weights = ~weight)
# weighted table1
weightedtable <- svyCreateTableOne(vars=xvars, strata = "treatment",
                                   data=weighteddata, test=FALSE)
# show table with SMD
print(weightedtable, smd=TRUE)
# check balance
# this is based on weighted data, so standard deviations dont matter as much
# can also create weighted mean directly:
# to get a weighted mean for a single covariate directly:
weighted_age = mean(weight[treatment==1]*age[treatment==1])/(mean(weight[treatment==1]))
# the svy command does this more readily

# MSMs
# get causal relative risk, using log linking. Weighted GLM
glm.obj <- glm(died~treatment, weights=weight, family=binomial(link=log))
# summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))  #e vcovHC from sandwich
# get point estimate and CI for relative risk (need to exponentiate)
causalrr<-exp(betaiptw[2])  # relative risk
lcl<-exp(betaiptw[2]-1.96*SE[2])
ucl<-exp(betaiptw[2]+1.96*SE[2])
c(lcl, causalrr, ucl)
# value greater than 1 is indicating risk of death with heart catheterization

# get causal risk difference (identity link)
glm.obj <- glm(died~treatment, weights=weight, family=binomial(link="identity"))
# summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))  #e vcovHC from sandwich
causalrr<-exp(betaiptw[2])  # relative risk
lcl<-exp(betaiptw[2]-1.96*SE[2])
ucl<-exp(betaiptw[2]+1.96*SE[2])
c(lcl, causalrr, ucl)
# since risk difference, a value greater than 0 is saying risk of death given treatment

# We can do same thing with ipw pcakge
weightmodel<-ipwpoint(exposure=treatment, family="binomial", link="logit",
                      denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan
                      +Coma+lungcan+MOSF+sepsis, data=mydata)
# numeric summary of weights
summary(weightmodel$ipw.weights)
# plot of weights
ipwplot(weights=weightmodel$ipw.weights, logscale=FALSE, main="weights", xlim=c(0,22))
# Fit Marginal Structural Model (MSM)
msm<- svyglm(died ~ treatment, design=svydesign(~1, weights=~wt, data=mydata))
coef(msm)
confint(msm)
# this is a little easier, fewer steps than above

# truncating the weights
# say want to truncate weights at 10
truncweight <-replace(weight, weight>10, 10)
glm.obj <- glm(died~treatment, weights=weight, family=binomial(link="identity"))

# or specify truncation with ipw package
# trunc uses percentiles
weightmodel<-ipwpoint(exposure=treatment, family="binomial", link="logit",
                      denominator=~age+female+meanbp1+ARF+CHF+Cirr+colcan
                      +Coma+lungcan+MOSF+sepsis, data=mydata, trunc=0.01)
summary(weightmodel$weights.trun)
# plot of weights
ipwplot(weights=weightmodel$weights.trun, logscale=FALSE, main="weights", xlim=c(0,22))
# Fit Marginal Structural Model (MSM)
mydata$wt<-weightmodel$weights.trun
msm<- svyglm(died ~ treatment, design=svydesign(~1, weights=~wt, data=mydata))
coef(msm)
confint(msm)


