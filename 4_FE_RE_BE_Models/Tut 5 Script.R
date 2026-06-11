rm(list = ls())

pack = c('readr', 'dplyr', 'car', 'Hmisc', 'plm', 'stargazer')
install.packages(pack, dep =TRUE)

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)
library(lmtest)

#lmtest contains Breusch-Pagan Test

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 5/RENTAL 0.csv")
View(data)

panel <- pdata.frame(data, index = c("city","year"))
View(panel)

'(a). POLS Model'

POLS <- plm(lrent ~ y90 + lpop + lavginc + pctstu, data = panel, model = "pooling")
summary(POLS)

'(b). First Difference Model'

FD <- plm(clrent ~  clpop + clavginc + cpctstu, data = panel, model = "pooling")
summary(FD)

'(c). Fixed Effect Model'

FE <- plm(lrent ~  y90 + lpop + lavginc + pctstu, data = panel, model = "within")
summary(FE)


'(d). Random Effect Model'

RE <- plm(lrent ~  y90 + lpop + lavginc + pctstu, data = panel, model = "random")
summary(RE)

'(e).'

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

pooltest(POLS$formula , data = panel, model = "pooling")

'(f). Wu-Hausman Test'

phtest(FE, RE)

'comparison'

stargazer(POLS, FD, FE, RE,
          title="Panel regressions",
          header=FALSE, 
          type="text",  
          omit.table.layout="n",
          digits=3, 
          intercept.bottom=TRUE, 
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: lrent",
          model.names=FALSE,
          omit.stat=c("LL","ser"),
          column.labels=c("Pooled OLS", "First Difference", "Fixed Effects", "Random Effects"))




