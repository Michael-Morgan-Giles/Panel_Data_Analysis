rm(list = ls())

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)
library(AER)
library(systemfit)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 7/investment RC.csv")
View(data)

panel <- pdata.frame(data, index = c("id","year"))
View(panel)

'(a). POLS Model'

POLS <- plm(INV ~ MV + CS, data = panel, model = "pooling")
summary(POLS)

"(b). FE Model"

FE <- plm(INV ~ MV + CS, data = panel, model = "within")
summary(FE)

'(c). Poolability F-Test'
pooltest(POLS)

'(d). RE Model'

RE <- plm(INV ~ MV + CS, data = panel, model = "random")
summary(RE)

'(e). Wu-Hausman Test'

phtest(FE, RE)

'(f). Bresuch-Pagan Test'

plmtest(POLS, effect = "individual", type = "bp")

bptest(POLS)
#bptest is the studentised BP test

pbgtest(POLS)
#pbgtest test serial correlation in idiosyncratic errors

# pwfdtest(POLS) | pwfdtest needs to be FD model

pdwtest(POLS)
#pdwtest is the Durbin Watson test for serial auto corr

# pwartest must be for within models

pbsytest(POLS)
#Bera, Sosa-Escudero and Yoon locally robust test - balanced panel

# pbltest for random effect models only

plmtest(POLS)


'(g). iOLS'

panel_1 <- panel %>% filter(id == 1)
panel_2 <- panel %>% filter(id == 2)
panel_3 <- panel %>% filter(id == 3)
panel_4 <- panel %>% filter(id == 4)
panel_5 <- panel %>% filter(id == 5)


iOLS_1 <- plm(INV ~ MV + CS, data = panel_1, model = "pooling")
iOLS_2 <- plm(INV ~ MV + CS, data = panel_2, model = "pooling")
iOLS_3 <- plm(INV ~ MV + CS, data = panel_3, model = "pooling")
iOLS_4 <- plm(INV ~ MV + CS, data = panel_4, model = "pooling")
iOLS_5 <- plm(INV ~ MV + CS, data = panel_5, model = "pooling")

stargazer(iOLS_1, iOLS_2, iOLS_3, iOLS_4, iOLS_5,
          title="iOLS regressions",
          header=FALSE, 
          type="text",  
          omit.table.layout="n",
          digits=3, 
          intercept.bottom=TRUE, 
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: INV",
          model.names=FALSE,
          omit.stat=c("LL","ser"),
          column.labels=c("Firm 1", "Firm 2", "Firm 3", "Firm 4", "Firm 5"))

'(h). SUR'
install.packages("reshape2")                            
library("reshape2")  

sur_panel <- reshape(panel, idvar = "year", timevar = "id", direction = "wide")

#  'system of equations' | this was some youtube way to do it, but the code was helpful

#r1 <- INV.1 ~ MV.1 + CS.1
#r2 <- INV.2 ~ MV.2 + CS.2
#r3 <- INV.3 ~ MV.3 + CS.3
#r4 <- INV.4 ~ MV.4 + CS.4
#r5 <- INV.5 ~ MV.5 + CS.5

#sur_mod <- systemfit(list(firm1 = r1, firm2 = r2, firm3 = r3, firm4 = r4, firm5 = r5), data = sur_panel)

sur_mod <- systemfit(INV ~ MV + CS, data = panel, method = "SUR")
summary(sur_mod)

'(i). Random Coefficient Model'

RC_mod <- pvcm(INV ~ MV + CS, data = panel, model="random")
summary(RC_mod)

