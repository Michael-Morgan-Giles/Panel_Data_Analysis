rm(list = ls())

pack = c('readr', 'dplyr', 'car', 'Hmisc', 'plm', 'stargazer')
install.packages(pack, dep =TRUE)

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)
library(AER)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 4/murder.csv")
View(data)

panel <- pdata.frame(data, index = c("id","year"))
View(panel)

'(b).'

panel_b <- panel %>% filter(d90 == 1 | d93 == 1)

POLS <- plm(mrdrte ~ d93 + exec + unem, data = panel_b, model = "pooling")
summary(POLS)


'(c).'

panel_c <- panel %>% filter(d93 == 1)

FD <- plm(cmrdrte ~  cexec + cunem, data = panel_c, model = "pooling")
summary(FD)


'(d).'

panel_d <- panel %>% filter(d93 == 1)

Mod_Serial <- plm(exec ~  cexec_1, cunem, data = panel, model = "pooling")
summary(Mod_Serial)


'(e).'

panel_e <- panel %>% filter(d93 == 1)

M_2SLS <- ivreg(cmrdrte ~  cexec + cunem | cexec + cunem + cexec_1, data = panel_e)
summary(M_2SLS)