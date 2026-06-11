rm(list = ls())

pack = c('readr', 'dplyr', 'car', 'Hmisc', 'plm', 'stargazer')
install.packages(pack, dep =TRUE)

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 1/rental.csv")

View(data)

panel <- pdata.frame(data, index = c("city","year"))
View(panel)

"(a)."
summary(panel)


"(b)."

reg1 <- plm(lrent ~ y90 + lpop + lavginc + pctstu  , data = panel, model = "pooling")
summary(reg1)

