
library(foreign) 
library(Hmisc)
library(dplyr)
library(tidyverse)

library(lavaan)

rm(list=ls())  #alles löschen

# Insert path where dataset is stored
load("/ready.RDa")



## divide age by 100 for more similar scales between variables
df1$age <- df1$age/100

##Measurement model latent factors health consciousness and political trust#####
  
  model0 <- '
  #measurement model
  h_cons =~ h_worry  + media_di + h_imp
  pol_trust =~ otrust_gov + otrust_hth + otrust_med + otrust_pol #+ otrust_parl 
  
  otrust_med ~~ otrust_pol
  otrust_gov ~~ otrust_hth
  
  '
  
  fit_m0 <- sem(model0, df1, meanstructure=T, missing="fiml")
  
  summary(fit_m0, standardized=TRUE, fit.measures = T, rsquare=T)
  
  

  
  
model1 <- '
#measurement model
h_cons =~ h_worry  + media_di + h_imp
pol_trust =~ otrust_gov + otrust_hth + otrust_med + otrust_pol #+ otrust_parl 

#Covariances
educ ~~ sex + mig + age + inc
sex ~~ mig + age + inc 
mig ~~ age + inc 
age ~~ inc 

pol_trust ~~ h_cons + vaxx

#Error Correlations
fear_infect ~~ fear_contr
h_worry ~~ fear_infect 
otrust_med ~~ otrust_pol
otrust_gov ~~  otrust_hth

#regressions
vaxx ~ a1*fear_infect + b1*fear_contr + e1*educ + f1*sex + g1*inc + h1*age
fear_infect ~ a2*pol_trust  + c2*h_cons + d2*educ + e2*sex + f2*inc + g2*age  
fear_contr ~  a3*pol_trust  + c3*h_cons + d3*educ + e3*sex + f3*inc + g3*age 
pol_trust ~  a4*mig + b4*rel + c4*educ + d4*sex + e4*inc + f4*age
h_cons ~ a6*mig + b6*rel + c6*educ + d6*sex + e6*inc + f6*age
rel ~ a7*mig + c7*educ + d7*sex + e7*inc + f7*age

#total effect of migration

m1:= a7*b6*c2*a1 #mig-rel-hcon-inf-vaxx 
m2:= a7*b6*c3*b1 #mig-rel-hcon-contr-vaxx 
m3:= a7*b4*a2*a1 #mig-rel-trust-inf-vaxx 
m4:= a7*b4*a3*b1 #mig-rel-trust-contr-vaxx 

totmig:= m1+m2+m3+m4

'

fit_m1 <- sem(model1, df1, meanstructure=T, missing="fiml")

summary(fit_m1, standardized=TRUE, fit.measures = T, rsquare=T)
modindices(fit_m1, sort.=T)


model2 <- '
#measurement model
h_cons =~ h_worry  + media_di + h_imp
pol_trust =~ otrust_gov + otrust_hth + otrust_med + otrust_pol #+ otrust_parl 

#Covariances
educ ~~ sex + mig + age + inc
sex ~~ mig + age + inc 
mig ~~ age + inc 
age ~~ inc 

#Error Correlations
fear_infect ~~ fear_contr
h_worry ~~ fear_infect 

otrust_med ~~ otrust_pol
otrust_gov ~~  otrust_hth

h_cons ~~ pol_trust + vaxx

#regressions
vaxx ~ a1*fear_infect + b1*fear_contr  + i1*mig + x1*rel + e1*educ + f1*sex + g1*inc + h1*age + k1*pol_trust 
fear_infect ~ a2*pol_trust  + c2*h_cons + d2*educ + e2*sex + f2*inc + g2*age  
fear_contr ~  a3*pol_trust  + c3*h_cons + d3*educ + e3*sex + f3*inc + g3*age + x3*rel
pol_trust ~  a4*mig + b4*rel + c4*educ + d4*sex + e4*inc + f4*age
h_cons ~ a6*mig + b6*rel + c6*educ + d6*sex + e6*inc + f6*age
rel ~ a7*mig + c7*educ + d7*sex + e7*inc + f7*age


#total effect of migration

m1:= a7*b6*c2*a1 #mig-rel-hcon-inf-vaxx 
m2:= a7*b6*c3*b1 #mig-rel-hcon-contr-vaxx 
m3:= a7*b4*a2*a1 #mig-rel-trust-inf-vaxx 
m4:= a7*b4*a3*b1 #mig-rel-trust-contr-vaxx 

m5:= a7*b4*k1    #mig-rel-trust-vaxx 
m6:= a7*x3*b1    #mig-rel-contr-vaxx
m7:= a7*x1    #mig-rel-vaxx

totmig:= m1+m2+m3+m4+m5+m6+m7+i1

'

fit_m2 <- sem(model2, df1, meanstructure=T, missing="fiml")

summary(fit_m1, standardized=TRUE, fit.measures = T, rsquare=T)
summary(fit_m2, standardized=TRUE, fit.measures = T, rsquare=T)

modindices(fit_m2, sort.=T)

fitmeasures(fit_m2, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "cfi"))
fitmeasures(fit_m1, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "cfi"))

### Model 1###################################

#unstandardized table
b1 <- parameterEstimates(fit_m1)
b1 <- b1[b1$op=="~",]
b1 <- b1[c("lhs", "rhs", "est", "se", "pvalue")]
b1$est <- round(b1$est, digits=3)
b1$pvalue <- round(b1$pvalue, digits=3)
b1$se <- round(b1$se, digits=3)



###standardized table
a1 <- standardizedsolution(fit_m1)
a1 <- a1[a1$op=="~",]
a1 <- a1[c("lhs", "rhs", "est.std", "se", "pvalue")]

a1$est.std <- round(a1$est.std, digits=3)
a1$se <- round(a1$se, digits=3)
a1$pvalue <- round(a1$pvalue, digits=3)

###############################################################

####Model  1+ 
#unstandardized
  b1 <- parameterEstimates(fit_m2)
  b1 <- b1[b1$op=="~",]
  b1 <- b1[c("lhs", "rhs", "est", "se", "pvalue")]
  b1$est <- round(b1$est, digits=3)
  b1$pvalue <- round(b1$pvalue, digits=3)
  b1$se <- round(b1$se, digits=3)



###standardized
a1 <- standardizedsolution(fit_m2)
a1 <- a1[a1$op=="~",]
a1 <- a1[c("lhs", "rhs", "est.std", "se", "pvalue")]

a1$est.std <- round(a1$est.std, digits=3)
a1$se <- round(a1$se, digits=3)
a1$pvalue <- round(a1$pvalue, digits=3)












