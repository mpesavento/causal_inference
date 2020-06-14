# week 4 - IPTW causal analysis

library(tableone)
library(Matching)
library(MatchIt)
library(ipw)
library(survey)
library(ggplot2)

# load the data
data(lalonde)
View(lalonde)

xvars = c("age", "educ", "black", "hispan", "married", 
          "nodegree", "re74", "re75")
outcome = "re78"  # post intervention income

# fit propensity score model, using logistic regression with outcome as treatment
# Include the 8 confounding variables in the model as predictors, 
# with no interaction terms or non-linear terms (such as squared terms). 
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75, 
             family=binomial(link="logit"), data=lalonde)
ps<- predict(psmodel, type="response")  # gets same as psmodel$fitted.values?

# plot propensity score histograms
pscore_df<-data.frame(
  treat=ifelse(lalonde$treat==1, "1", "0"),
  pscore=ps
)
# NOTE: why is this still not showing colors???
ggplot(pscore_df, aes(x=pscore)) +
  geom_histogram(aes(color=treat, fill=treat),
                 position="identity", bins=20, alpha=0.4) +
  scale_color_manual(values=c("blue", "red")) +
  scale_fill_manual(values=c("blue", "red"))

# apply weights
weight<-ifelse(lalonde$treat==1, 1/(ps), 1/(1-ps))
# apply weights to data
weighteddata <- svydesign(ids = ~1, data=lalonde, weights = ~weight)
# weighted table1
weightedtable <- svyCreateTableOne(vars=xvars, strata="treat",
                                   data=weighteddata, test=FALSE)
# show table with SMD
print(weightedtable, smd=TRUE)


# get causal risk difference
weightmodel<-ipwpoint(exposure=treat, family="binomial", link="logit",
                      denominator=~age+educ+black+hispan+married+nodegree+re74+re75,
                      data=lalonde)
summary(weightmodel$ipw.weights)
# plot of weights
ipwplot(weights=weightmodel$ipw.weights, logscale=FALSE, main="weights", xlim=c(0,41))
# Fit Marginal Structural Model (MSM)
msm<- svyglm(died ~ treatment, design=svydesign(~1, weights=~wt, data=mydata))
coef(msm)
confint(msm)


# =======
# Q1 - minimum and maximum propensity weights
min(weight)
max(weight)
# 1.009, 40.07729

# Q2 - standardized differences for each confounder on weighted population
# what is standardized difference for nodegree?
print(weightedtable, smd=TRUE)
# 0.11


# Q3 - Using IPTW, find the estimate and 95% confidence interval for the average causal effect. 
# This can be obtained from svyglm
coef(msm)
# treat, 224.676
confint(msm)
# treat, -1559.321, 2008.673


# Q4 - truncate the weights at the 1st and 99th percentile, using trunc=0.01 in svyglm

weightmodel<-ipwpoint(exposure=treat, family="binomial", link="logit",
                      denominator=~age+educ+black+hispan+married+nodegree+re74+re75,
                      data=lalonde, trunc=0.01)
lalonde$wt.trunc<-weightmodel$weights.trun
ipwplot(weights=weightmodel$weights.trun, logscale=FALSE, main="weights", xlim=c(0,41))

msm<- svyglm(re78 ~ treat, design=svydesign(~1, weights=~wt.trunc, data=lalonde))
coef(msm)
# treat, 486.933
confint(msm)
# treat, -1090.639, 2064.506


