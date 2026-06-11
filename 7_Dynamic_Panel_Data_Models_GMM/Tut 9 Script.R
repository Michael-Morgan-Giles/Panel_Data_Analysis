rm(list = ls())

library(readr)
library(dplyr)
library(car)
library(Hmisc)
library(plm)
library(stargazer)
library(AER)
library(tidyr)

data <- read.csv("~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 9/airfare.csv")
View(data)

panel <- pdata.frame(data, index = c("id","year"))
View(panel)
head(attr(panel, "index"))

'Variables for Model (2)'

'lags'
panel['lfare_it1'] <- lag(panel$lfare)
panel['lfare_it2'] <- lag(panel$lfare, k=2L)
panel['lfare_it3'] <- lag(panel$lfare, k=3L)
panel['concen_it1'] <- lag(panel$concen)
panel['concen_it2'] <- lag(panel$concen, k=2L)
panel['y99_t1'] <- lag(panel$y99)
panel['y00_t1'] <- lag(panel$y00)

'differences'
panel['delta_lfare_it'] <- panel['lfare']-panel['lfare_it1']
panel['delta_lfare_it1'] <- panel['lfare_it1']-panel['lfare_it2']
panel['delta_lfare_it2'] <- panel['lfare_it2']-panel['lfare_it3']

panel['delta_concen_it'] <- panel['concen']-panel['concen_it1']
panel['delta_concen_it1'] <- panel['concen_it1']-panel['concen_it2']

drop_na(panel)

# exporting adjusted csv #
write.csv(panel,"~/1. University/Subjects/ECMT6007 - Analysis of Panel Data/Tutorials/Tut 9/airfare_adj.csv", row.names = FALSE)

'(a). POLS Overestimates and FE Underestimates lambda on lfare_1'

POLS_a <- plm(lfare ~ y99 + y00 + lfare_it1 + concen , data = panel, model = "pooling")
summary(POLS_a)

FE_a <- plm(lfare ~ y99 + y00 + lfare_it1 + concen , data = panel, model = "within")
summary(FE_a)

'(b). POLS'

POLS <- plm(delta_lfare_it ~ y00 + delta_lfare_it1 + delta_concen_it , data = panel, model = "pooling")
summary(POLS)

fd <- plm(lfare ~ y99 + y00 + lfare_it1 + concen , data = panel, model = "fd")
summary(fd)

'(c). NA '

'(d). Anderson-Hsaio Estimator'

M_2SLS <- ivreg(delta_lfare_it ~ y00 + dconcen + delta_lfare_it1 | y00 + dconcen + lfare_it2, data = panel)
summary(M_2SLS)

'(e). NA'

'(f). Arellano-Bond Estimator'

AR <- pgmm(lfare ~ lag(lfare, 1:2) + lag(concen, 1:2) + lag(y99, 1:2) + lag(y00, 1:2)
           |lag(lfare, 2:3)
             ,data = panel, effect = "twoway", model = "twostep")

AR <- pgmm(lfare ~ lag(lfare, 1:2) + lag(concen, 1:2)
           |lag(lfare, 2:3)
           ,data = panel, effect = "twoway", model = "twostep")

summary(AR, robust = TRUE)
summary(AR)

library(dynpanel)
data(Produc)

AB <-dpd(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,Produc,index=c("state","year"),1,4)

AB <-dpd(lfare ~ y99 + y00 + concen , data = data, index=c("id","year") ,1 ,4)
summary(AB)

'(g). second order autocorrelation test'

mtest(AR, order = 1)
mtest(AR, order = 2, vcov = vcovHC)

'(h). Arellano-Bond Estimator concen instrument'

'(i). NA'

####--examples--------------------------------------------------

data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4 col. b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1, robust = FALSE)

mtest(z1, order = 1)
mtest(z1, order = 2, vcov = vcovHC)

## Blundell and Bond (1998) table 4 (cf. DPD for OX p. 12 col. 4)
z2 <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
             lag(log(capital), 0:1) | lag(log(emp), 2:99) +
             lag(log(wage), 2:99) + lag(log(capital), 2:99),
           data = EmplUK, effect = "twoways", model = "onestep", 
           transformation = "ld")
summary(z2, robust = TRUE)

####-----------------------------------------------------------

#### GapMinder Test

gap_panel <- pdata.frame(gapminder, index = c("country","year"))

gap_mod <- pgmm(log(lifeExp) ~ lag(log(lifeExp), 1) 
           + log(pop)
           + log(gdpPercap) 
           | lag(log(lifeExp), 2:12),
           data = gap_panel, effect = "twoways", model = "twosteps")

gap_mod <- pgmm(log(lifeExp) ~ log(lifeExp)
                + log(pop)
                + log(gdpPercap) 
                | lag(log(lifeExp), 3),
                data = gap_panel, effect = "twoways", model = "twosteps")

summary(gap_mod, robust = FALSE)

mtest(gap_mod, order = 1)
mtest(gap_mod, order = 2, vcov = vcovHC)

FE <- plm(log(lifeExp) ~ log(pop) + log(gdpPercap), 
          data = gap_panel, 
          model = "within",
          effect = "twoway")

summary(FE)
