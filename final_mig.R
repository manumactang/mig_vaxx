######
#### Migration Only Modell incl. Acculturation#######


library(foreign) 
library(Hmisc)
library(dplyr)
library(tidyverse)

library(lavaan)

rm(list=ls())  #alles löschen

load("C:/Users/Manuel/Nextcloud/project-empsf/UB_MIGRATION STUDIE 2020/Datensätze/ready.RDa")


df_m <- df1[c("media_ger" , "media_cor" , 
              "h_worry", "h_imp" , "media_di", 
              "fear_infect", "fear_contr",
              "otrust_gov" , "otrust_med" ,"otrust_parl",
              "otrust_pol" , "otrust_hth" , 
              "inc", "sex", "mig", "vaxx", "age",
              "rel", "ysm", "educ", "region")]


df_m <- df_m[df_m$mig==1,]



#####
#df_m <- na.omit(df_m)

df_m$euro <- NA
df_m$euro[df_m$region == "europ"] <- 1 
df_m$euro[df_m$region == "noneuro" | df_m$region == "other"] <- 0 

df_m$other <- NA
df_m$other[df_m$region == "other"] <- 1 
df_m$other[df_m$region == "noneuro" | df_m$region == "europ"] <- 0 


#######################################
##Mit Regionsdummy 
df_m <- df_m[df_m$region != "other",]

options(max.print=1000000)

df_m$age <- df_m$age/100
df_m$ysm <- df_m$ysm/100

###################################################################
model0 <- '
  #measurement model
  h_cons =~ h_worry  + media_di + h_imp
  pol_trust =~ otrust_gov + otrust_hth + otrust_med + otrust_pol #+ otrust_parl 
  
  otrust_med ~~ otrust_pol
  otrust_gov ~~ otrust_hth
  
  '

fit_m0 <- sem(model0, df_m, meanstructure=T, missing="fiml")

summary(fit_m0, standardized=TRUE, fit.measures = T, rsquare=T)

modindices(fit_m0, sort.=T)




model_m1 <- '
  #measurement model
  h_cons =~ h_worry  + media_di + h_imp 
  pol_trust =~ otrust_gov + otrust_hth + otrust_med +  otrust_pol #+ otrust_parl 

  #Covariances
  educ ~~ age + sex + inc + ysm  + euro
  age ~~ sex + inc + ysm  + euro 
  sex ~~ inc + ysm  + euro 
  inc ~~ ysm  + euro 
  ysm ~~ euro 

  
pol_trust ~~ h_cons + vaxx
  
#Error Correlations
fear_infect ~~ fear_contr  
h_worry ~~ fear_infect 
media_cor ~~ media_ger

otrust_med ~~ otrust_pol
otrust_gov ~~  otrust_hth

#regressions
vaxx ~ a1*fear_infect + b1*fear_contr  + e1*educ + f1*sex + g1*inc + h1*age 
fear_infect ~ a2*pol_trust  + d2*h_cons +  j2*educ + k2*sex + l2*inc + m2*age 
fear_contr ~  a3*pol_trust + d3*h_cons + j3*educ + k3*sex + l3*inc + m3*age 
pol_trust ~  a4*media_cor + b4*media_ger  + x4*ysm + h4*euro + c4*rel + d4*educ + e4*sex + f4*inc + g4*age
h_cons ~ a6*media_cor + b6*media_ger + x6*ysm  + d6*euro + e6*rel + f6*educ + g6*sex + h6*inc + i6*age
media_cor ~ a7*ysm + b7*euro + c7*educ + d7*sex + e7*inc + f7*age 
media_ger ~ a8*ysm + b8*euro + c8*educ + d8*sex + e8*inc + f8*age 
rel ~ a9*ysm + b9*euro + c9*educ + d9*sex + e9*inc + f9*age 
'

fit_m1 <- sem(model_m1, df_m, meanstructure=T, missing="fiml")
################################################################################


model_m2 <- '
  #measurement model
  h_cons =~ h_worry  + media_di + h_imp 
  pol_trust =~ otrust_gov + otrust_hth + otrust_med +  otrust_pol #+ otrust_parl 

  #Covariances
  educ ~~ age + sex + inc + ysm  + euro
  age ~~ sex + inc + ysm  + euro 
  sex ~~ inc + ysm  + euro 
  inc ~~ ysm  + euro 
  ysm ~~ euro 

pol_trust ~~ h_cons + vaxx
  
#Error Correlations
fear_infect ~~ fear_contr  
h_worry ~~ fear_infect 
media_cor ~~ media_ger

otrust_med ~~ otrust_pol
otrust_gov ~~  otrust_hth


#regressions

vaxx ~ a1*fear_infect + b1*fear_contr  + e1*educ + f1*sex + g1*inc + h1*age + x1*euro + y1*rel + q1*media_ger
fear_infect ~ a2*pol_trust  + d2*h_cons +  j2*educ + k2*sex + l2*inc + m2*age + n2*euro
fear_contr ~  a3*pol_trust + d3*h_cons + j3*educ + k3*sex + l3*inc + m3*age 
pol_trust ~  a4*media_cor + b4*media_ger  + x4*ysm + h4*euro + c4*rel + d4*educ + e4*sex + f4*inc + g4*age
h_cons ~ a6*media_cor + b6*media_ger + x6*ysm  + d6*euro + e6*rel + f6*educ + g6*sex + h6*inc + i6*age
media_cor ~ a7*ysm + b7*euro + c7*educ + d7*sex + e7*inc + f7*age 
media_ger ~ a8*ysm + b8*euro + c8*educ + d8*sex + e8*inc + f8*age 
rel ~ a9*ysm + b9*euro + c9*educ + d9*sex + e9*inc + f9*age 
'



fit_m2 <- sem(model_m2, df_m, meanstructure=T, missing="fiml")

summary(fit_m2, standardized=TRUE, fit.measures = T, rsquare=T)
modindices(fit_m2, sort.=T)



summary(fit_m1, standardized=TRUE, fit.measures = T, rsquare=T)

fitmeasures(fit_m1, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "cfi"))
fitmeasures(fit_m2, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "cfi"))



modindices(fit_m1, sort.=T)

####### Model 1 #####

b1 <- parameterEstimates(fit_m1)
b1 <- b1[b1$op=="~",]
b1 <- b1[c("lhs", "rhs", "est", "se", "pvalue")]
b1$est <- round(b1$est, digits=3)
b1$pvalue <- round(b1$pvalue, digits=3)
b1$se <- round(b1$se, digits=3)


###standardized
a1 <- standardizedsolution(fit_m1)
a1 <- a1[a1$op=="~",]
a1 <- a1[c("lhs", "rhs", "est.std", "se", "pvalue")]

a1$est.std <- round(a1$est.std, digits=3)
a1$se <- round(a1$se, digits=3)
a1$pvalue <- round(a1$pvalue, digits=3)


### Model + modindices path####


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





