# causal_analysis week 3

library(tableone)
library(Matching)
library(MatchIt)

# load the data
data(lalonde)
View(lalonde)

# view column information
str(lalonde)

xvars = c("age", "educ", "black", "hispan", "married", 
          "nodegree", "re74", "re75")
outcome = "re78"  # post intervention income

# create a Table 1, prematching
# more info here: https://cran.r-project.org/web/packages/tableone/vignettes/smd.html
# SMD = standardized differences
table1<- CreateTableOne(vars=xvars, strata="treat", data=lalonde, test=FALSE)
print(table1, smd=TRUE)

#Q1
# standardized difference for married, prematching

#Q2
6984.17 - 6349.14


# greedy match, Mahalanobis distance
greedymatch<-Match(Tr=lalonde$treat, M=1, X=lalonde[xvars])
matched<-lalonde[unlist(greedymatch[c("index.treated", "index.control")]), ]

matchedtable1<-CreateTableOne(vars=xvars, strata="treat", data=matched, test=FALSE)
print(matchedtable1, smd=TRUE)


# causal risk difference from paired t-test
#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

# pairwise difference
diff_y<- y_trt - y_con
# paired t-test
t.test(diff_y)


# McNemar test
# can't do this, cause our outcome is continuous, not binomial
#table(y_trt, y_con)
# manually create matrix from the output of the table?
#mcnemar.test(matrix(c(994, 493, 394, 305), 2, 2)

# if want a causal risk ratio or causal odds ratio, 
# use GEE with log or logit link (respectively)

##################
# fit propensity score model
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75, 
             family=binomial(), data=lalonde)

#show coefficients, etc
summary(psmodel)
# create propensity score
pscore<-psmodel$fitted.values

# plot propensity score histograms
pscore_df<-data.frame(
  treat=lalonde$treat,
  pscore=pscore
)
ggplot(pscore_df, aes(x=pscore)) +
  geom_histogram(aes(color=treat, fill=treat),
                 position="identity", bins=20, alpha=0.4)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
  

# Q3
min(pscore)
max(pscore)

# using matchit
m.out <- matchit(treat~age+educ+black+hispan+married+nodegree+re74+re75+re78,
                 data=lalonde, method="nearest")
summary(m.out)

#propensity score plots
plot(m.out, type="jitter")
plot(m.out, type="hist")

###########################
# matching without caliper
# do greedy matching
set.seed(931139)

# Q4, Q5
# match on propensity score, not logit
psmatch<-Match(Tr=lalonde$treat, M=1, X=pscore, replace=FALSE)
matched<-lalonde[unlist(psmatch[c("index.treated", "index.control")]),]

matchedtable1<-CreateTableOne(vars=xvars, strata="treat", data=matched, test=FALSE)
print(matchedtable1, smd=TRUE)

# with caliper, without logit
set.seed(931139)
psmatch_cal<-Match(Tr=lalonde$treat, M=1, X=pscore, replace=FALSE, caliper=0.1)
matched_cal<-lalonde[unlist(psmatch_cal[c("index.treated", "index.control")]),]

matchedtable1_cal<-CreateTableOne(vars=xvars, strata="treat", data=matched_cal, test=FALSE)
print(matchedtable1_cal, smd=TRUE)

# Q6 how many matched pairs
# Q7 mean of real earnings in 1978 for treated vs control

y_trt<-matched_cal$re78[matched_cal$treat==1]
y_con<-matched_cal$re78[matched_cal$treat==0]

# marginal means within population
m_trt<-mean(y_trt)
m_con<-mean(y_con)
m_trt - m_con

# pairwise difference
diff_y<- y_trt - y_con
# paired t-test
t.test(diff_y)



#################
# use logit

# match on logit(ps)
psmatch<-Match(Tr=lalonde$treat, M=1, X=logit(pscore), replace=FALSE)
matched<-lalonde[unlist(psmatch[c("index.treated", "index.control")]),]

psmatchedtab1<-CreateTableOne(vars=xvars, strata="treat", data=matched, test=FALSE)
print(pasmatchedtab1, smd=TRUE)


# PS matching with caliper
psmatch<-Match(Tr=lalonde$treat, M=1, X=logit(pscore), replace=FALSE, caliper=0.1)
matched<-lalonde[unlist(psmatch[c("index.treated", "index.control")]),]
psmatchedtab1<-CreateTableOne(vars=xvars, strata="treat", data=matched, test=FALSE)
print(pasmatchedtab1, smd=TRUE)

# outcome analysis
# causal risk difference from paired t-test
#outcome analysis
y_trt<-matched$outcome[matched$treat==1]
y_con<-matched$outcome[matched$treat==0]

# pairwise difference
diff_y<- y_trt - y_con
# paired t-test
t.test(diff_y)


