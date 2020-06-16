# week_5_ex
# Card. D "Using Geographic variation in college proximity to estimate the return to schooling"
# how much would you expect the number of years of education on affecting education
# proposed instrument = proximity to 4 year college


library(ivpack)

# read dataset
data(card.data)

# IV is nearc4 (near 4 year college)
# outcome is lwage (log of wage)
# 'treatment' is educ (number of years of education)

#summary stats
mean(card.data$nearc4)
par(mfrow=c(1,2))
hist(card.data$lwage)
hist(card.data$educ)

# make education binary
educ12<-card.data$educ>12
# estimate proportion of 'compliers'
propcomp = mean(educ12[card.data$nearc4==1]) - mean(educ12[card.data$nearc4==0])
propcomp
# 0.1219

# intention to treat effect
# causal effect of encouragement
itt<- mean(card.data$lwage[card.data$nearc4==1]) - mean(card.data$lwage[card.data$nearc4==0])
itt
# 0.1559

# complier average causal effect
cace<-itt/propcomp
cace
# 1.2786
# larger than itt, which we expect with no defiers assumption
# defier: lives near 4 year college and doesnt get education; OR
# doesnt live near 4 year college and gets education

## ============================

# estimate the same kind of effect using 2 stage least squares (2SLS)

# stage 1: regress A on Z
s1<-lm(educ12~card.data$nearc4)
# get predicted value of A given Z for each subject
predtx <- predict(s1, type="response")
table(predtx)
# predtx
# 0.422152560083588  0.54408183146614 
# 957              2053 

# stage 2: regress Y on predicted value of A
lm(card.data$lwage~predtx)
# Coefficients:
# (Intercept)   predtx  
# 5.616        1.279  


# two stage least squares using ivpack
ivmodel=ivreg(lwage ~ educ12, ~ nearc4, x=TRUE, data=card.data)
robust.se(ivmodel)

## ============================
# controlling for covariates

ivmodel=ivreg(lwage ~ educ12 + exper + reg661 + reg662 + reg663 + reg664 +reg665 + reg666 + reg667 + reg668,
              ~ nearc4 + exper + reg661 + reg662 + reg663 + reg664 +reg665 + reg666 + reg667 + reg668,
              x=TRUE, data=card.data)
robust.se(ivmodel)
