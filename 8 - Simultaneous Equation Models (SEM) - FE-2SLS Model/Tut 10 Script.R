rm(list = ls())

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)
library(lmtest)
library(AER)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 10/crime4.csv")
View(data)

panel <- pdata.frame(data, index = c("county","year"))
View(panel)

'(a). Summary Stats'
summary(panel)

'(c). POLS Model'

POLS <- plm(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87,
            data = panel, model = "pooling", effect = "twoway")
summary(POLS)

'(d). First Difference Model'

FD <- plm(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87,
          data = panel, model = "fd", effect = "individual")
summary(FD)

'(e). Fixed Effect Model'

FE <- plm(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87,
          data = panel, model = "within", effect = "individual")
summary(FE)


'(g). Testing for Simultaneity'



'(h). Pooled 2SLS'

pooled_2SLS_reg <- ivreg(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87 
                         |  ltaxpc + lmix + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87,
                         data = panel)
summary(pooled_2SLS_reg)

p_2SLS_reg <- plm(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87 
                   |  ltaxpc + lmix + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87, 
                   data = panel,
                   model = "pooling", effect = "individual")
summary(p_2SLS_reg) 

'(i). FE-2SLS'

fe_2SLS_reg <- plm(lcrmrte ~ lprbarr + lpolpc + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87 
                   |  ltaxpc + lmix + lprbpris + lavgsen + d82  + d83  + d84  + d85  + d86  + d87, 
                 data = panel,
                  model = "within", effect = "individual")
summary(fe_2SLS_reg) 


stargazer(POLS, FD, FE, p_2SLS_reg, fe_2SLS_reg,
          title="SEM Model Comparison to Single Equation Models",
          header=FALSE, 
          type="text",  
          omit.table.layout="n",
          digits=3, 
          intercept.bottom=TRUE, 
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: lcrmrte",
          model.names=FALSE,
          omit.stat=c("LL","ser"),
          column.labels=c("Pooled OLS", "First Difference", "Fixed Effects", "Pooled 2SLS", "Fixed Effects 2SLS"))

M_2SLS <- ivreg(cmrdrte ~  cexec + cunem | cexec + cunem + cexec_1, data = panel_e)
summary(M_2SLS)


