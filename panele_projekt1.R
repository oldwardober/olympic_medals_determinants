
# DETERMINANTY LICZBY ZDOBYTYCH MEDALI OLIMPIJSKICH

setwd("C:/Users/kszaf/OneDrive/Pulpit/dane_panelowe/projekt1") #change to your directory

library("plm")
library('stargazer')

dane = readxl::read_excel('igrzyska_panel.xlsx')
dane = dane[,1:10]
dane = dane[1:1218,]

dane$gospodarz = as.factor(dane$gospodarz)
dane$postkom = as.factor(dane$postkom)
dane$region = as.factor(dane$region)
dane$igrzyska = as.factor(dane$igrzyska)

summary(dane)

dane2 = na.omit(dane)

mean(dane$medale) #4.75
var(dane$medale) #176.7 czyli poisson raczej odpada...

# TOBIT

dane <- within(dane, region <- relevel(region, ref = "WEU"))

FEtobit = pldv(medale ~ log(pkb_pc) + log(populacja) + zdrowie + log(sportowcy) + gospodarz + region + igrzyska, data = dane, model = 'fd',
               index = c('kraj', 'igrzyska'), lower = 0, upper = +Inf, objfun = "lsq", sample ="cens")
summary(FEtobit)

REtobit = pldv(medale ~ log(pkb_pc) + log(populacja) + zdrowie + log(sportowcy) + gospodarz + region + igrzyska, data = dane, 
               model = 'random', index = c('kraj', 'igrzyska'),  method = 'bfgs', lower = 0, upper = +Inf, sample ="cens")
summary(REtobit)

#testy

phtest(FEtobit, REtobit) #odrzucamy


#dlaczego parametr ujemny przy kolejnych igrzyskach?
sum(dane$medale[dane$igrzyska == "2000"]) #919
sum(dane$medale[dane$igrzyska == "2004"]) #925
sum(dane$medale[dane$igrzyska == "2008"]) #955
sum(dane$medale[dane$igrzyska == "2012"]) #961
sum(dane$medale[dane$igrzyska == "2016"]) #967
sum(dane$medale[dane$igrzyska == "2020"]) #1066

#mo¿e by³o wiêcej pañstw, które zdoby³y przynajmniej 1 medal...
sum(dane$medale > 0 & dane$igrzyska == "2000") #78
sum(dane$medale > 0 & dane$igrzyska == "2004") #73
sum(dane$medale > 0 & dane$igrzyska == "2008") #87
sum(dane$medale > 0 & dane$igrzyska == "2012") #84
sum(dane$medale > 0 & dane$igrzyska == "2016") #84
sum(dane$medale > 0 & dane$igrzyska == "2020") #90
#niby tak, ale nie do koñca


# stargazer

library("maxLik")
library("texreg")
extract.maxLik <- function (model, include.nobs = TRUE, ...){
  s <- summary(model, ...)
  names <- rownames(s$estimate)
  class(names) <- "character"
  co <- s$estimate[, 1]
  se <- s$estimate[, 2]
  pval <- s$estimate[, 4]
  class(co) <- class(se) <- class(pval) <- "numeric"
  n <- nrow(model$gradientObs)
  lik <- logLik(model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  gof <- c(gof, n, lik)
  gof.names <- c(gof.names, "Num. obs.", "Log Likelihood")
  gof.decimal <- c(gof.decimal, FALSE, TRUE)
  tr <- createTexreg(coef.names = names, coef = co, se = se, pvalues = pval,
                     gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
  return(tr)
}
setMethod("extract", signature = className("maxLik", "maxLik"), definition = extract.maxLik)

screenreg(list(REtobit, FEtobit))

