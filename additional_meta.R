##jeff script for meta
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/510-523 Projects/jeff meta analysis")

##import file
allvalues = read.csv("revised_meta_table.csv")

##import the libraries
library(metafor)
library(meta)
library(pwr)

##set up for the meta test
allvalues$correl = rep(0, nrow(allvalues))

##edit number 5 to have valid values (i.e. not the t-test values)
allvalues$sdpre[5] = .01 * sqrt(139)
allvalues$meanpost[5] = 0
allvalues$sdpost[5] = 0

##run effects to get variance
saveinfo = with(allvalues, escalc("SMCC", m1i = meanpre, m2i = meanpost, 
                       sd1i = sdpre, sd2i = sdpost, ni = n,
                       ri = correl))
allvalues$variance = saveinfo$vi
allvalues$se_est = sqrt(saveinfo$vi)

##model all values and groups
allgroups = metagen(effect.size, se_est, data = allvalues)
summary(allgroups)

qoldata = subset(allvalues, type == "QOL")
qolgroups = metagen(effect.size, se_est, data = qoldata)
summary(qolgroups)

ptgdata = subset(allvalues, type == "PTG")
ptggroups = metagen(effect.size, se_est, data = ptgdata)
summary(ptggroups)

##test for influence
fixedmodel = rma(yi=effect.size, vi=variance, data = allvalues, method="FE")
randommodel = rma(yi=effect.size, vi=variance, data = allvalues, method="DL")
inffixed =influence(fixedmodel)
infrandom =influence(randommodel)
inffixed
infrandom

##model without point 10
allgroups = metagen(effect.size, se_est, data = allvalues[ -10, ])
summary(allgroups)

qoldata = subset(allvalues[-10, ], type == "QOL")
qolgroups = metagen(effect.size, se_est, data = qoldata)
summary(qolgroups)

ptgdata = subset(allvalues[-10, ], type == "PTG")
ptggroups = metagen(effect.size, se_est, data = ptgdata)
summary(ptggroups)

##get normal CIs
##p for each experiment if they had done this test
##number sig/nonsign
#CI's based on normal approximation
options(scipen=999)
CIs = data.frame(ci(TE = allvalues$effect.size, seTE = sqrt(allvalues$variance)))
CIs
nonsig = sum(CIs$p > .05)
(nrow(allvalues) - nonsig)/nrow(allvalues) * 100 ##percent significant findings
 
####power
#individual power based on individual ES
powerstuff = pwr.t.test(n = allvalues$n, d = allvalues$effect.size, sig.level = 0.05, type = c("paired"))
allvalues$power = powerstuff$power
mean(allvalues$power)
sd(allvalues$power)
#count of powers>.8
length(which(allvalues$power>=.8))/nrow(allvalues) *100 # studies with .8 power or above

#based on effect of .13
powerstuff2 = pwr.t.test(n = allvalues$n, d = .13, sig.level = 0.05, type = c("paired"))
allvalues$power2 = powerstuff2$power
mean(allvalues$power2)
sd(allvalues$power2)
