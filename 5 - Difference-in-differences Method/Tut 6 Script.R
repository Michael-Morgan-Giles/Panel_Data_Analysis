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

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 6/TaxCredit93_dv.csv")
View(data)

data['dkids*dpost'] <- data['dkids']*data['dpost']
data['agesq'] <- data['age']*data['age']
data['nlinc'] <- data['finc']-data['earn']
data['dkidsdpost92'] = data['dkids']*data['dpost92']

'(a).'

summary(data)

panel_a_0 <- data %>% filter(children == 0)

summary(panel_a_0)

panel_a_1 <- data %>% filter(children == 1)

summary(panel_a_1)

panel_a_2 <- data %>% filter(children >= 2)

summary(panel_a_2)


"(b). create DV completed in  graph | completed in excel but should practice in here"

'(c). DID Estimation'

panel_b_c <- data %>% filter(dkids == 0 & dpost==0)
panel_b_t <- data %>% filter(dkids == 0 & dpost==1)
panel_a_c <- data %>% filter(dkids == 1 & dpost==0)
panel_a_t <- data %>% filter(dkids == 1 & dpost==1)

bc <- mean(panel_b_c$work)
bt <- mean(panel_b_t$work)
ac <- mean(panel_a_c$work)
at <- mean(panel_a_t$work)

did <- (at-ac)-(bt-bc)
print(did)

'(d). DID Regresion 1'

DID <- lm(work ~ dkids + dpost + dkidsdpost, data = data)
summary(DID)

'(e).'
DID1 <- lm(work ~ dkids + dpost + dkidsdpost + age + agesq + ed + finc + nlinc, data = data)
summary(DID1)

'(f).'

DID2 <- lm(work ~ dkids + dpost + dkidsdpost + age + agesq + ed + finc + nlinc + urate, data = data)
summary(DID2)


'(g).'

DID3 <- lm(work ~ dpost + onekid + twokid + onepost + twopost + age + agesq + ed + finc + nlinc + urate, data = data)
summary(DID3)

'(h).'
data_h <- data %>% filter(year < 1994)

DID4 <- lm(work ~ dkids + dpost92 + dkidsdpost92 + age + agesq + ed + finc + nlinc + urate, data = data_h)
summary(DID4)
