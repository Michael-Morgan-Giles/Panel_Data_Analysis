rm(list = ls())

pack = c('readr', 'dplyr', 'car', 'Hmisc', 'plm', 'stargazer')
install.packages(pack, dep =TRUE)

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 3/vote2.csv")
View(data)

panel <- pdata.frame(data, index = c("state","district"))
View(panel)

'(a).'

POLS1_90 <- plm(vote90 ~ linexp90 + lchexp90 + incshr90 , data = panel, model = "pooling")
summary(POLS1_90)

POLS2_88 <- plm(vote88 ~ linexp88 + lchexp88 + incshr88 , data = panel, model = "pooling")
summary(POLS2_88)


'(b).'

Mod_fd <- plm(cvote ~ clinexp + clchexp + cincshr , data = panel, model = "pooling")
summary(Mod_fd)


'(c).'
linearHypothesis(Mod_fd, c("clinexp = 0","clchexp = 0"),test = "F")


'(d).'

Mod_fd2 <- plm(cvote ~ cincshr , data = panel, model = "pooling")
summary(Mod_fd2)


'(e).'

panel_rpt <- panel %>% filter(rptchall == 1)

Mod_fd3 <- plm(cvote ~ cincshr , data = panel_rpt, model = "pooling")
summary(Mod_fd3)







