
library(readxl)
library(tidyverse)

dane = readxl::read_xlsx("C:/Users/kszaf/OneDrive/Pulpit/I rok - mag/dane_panelowe/projekt1/igrzyska_panel.xlsx")
dane = dane[,-c(11:13)]
dane = dane[c(1:1218),]
dane$region = as.factor(dane$region)
dane = na.omit(dane)
summary(dane)

#grupuje dane po regionie i wyci¹gam œrednie
grouped1 <- dane %>% group_by(region) %>% summarize(suma = sum(medale), srednia = mean(medale))
grouped2 <- dane %>% group_by(region, igrzyska) %>% summarize(suma = sum(medale), srednia = mean(medale), 
                                                              srednia_pkb = mean(pkb_pc))



plot1 <- ggplot(grouped2, aes(x=igrzyska,y=srednia, color=region)) + geom_line() + geom_point()
plot1

plot2 <- ggplot(grouped2[grouped2$region != "WEU",], aes(x=igrzyska, y=srednia_pkb, color=region)) + geom_line() + geom_point()
plot2

plot3 <- ggplot(dane[dane$kraj == "Poland",], aes(x=igrzyska, y=medale)) + geom_bar(stat='identity', fill = 'red')
plot3

?geom_bar

plot4 <- ggplot(dane, aes(x=pkb_pc, y=medale, color=region)) + geom_point()
plot4

plot5 <- ggplot(dane[(dane$igrzyska == "2020" & dane$medale != 0),], aes(x=populacja, y=medale, color=region)) + geom_point() + scale_x_log10()
plot5

plot6 <- ggplot(dane[(dane$igrzyska == "2020" & dane$medale != 0),], aes(x=zdrowie, y=medale, color=region, label=kraj)) + geom_point() + geom_text(hjust=0, vjust=0)
plot6

