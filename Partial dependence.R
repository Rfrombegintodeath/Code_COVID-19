##evaluate the one fator and two factor partial dependence for RF model (bweek_train.forest)
library(randomForest)
library(pdp)
##here we use follw7 as an example 
##two factor PDP--plot, each significant factor and CRNA 
grid.arrange(
  inbound <- partial(bweek.forest,pred.var = c("crna","inbound"),chull = T)
  total_passenger <- partial(bweek.forest,pred.var = c("crna","total.passenger"),chull = T)
  pratio <- partial(bweek.forest,pred.var = c("crna","pratio"),chull = T)
  Delta <- partial(bweek.forest,pred.var = c("crna","Delta"),chull = T)
  Omicron <- partial(bweek.forest,pred.var = c("crna","Omicron"),chull = T)
  Minority <- partial(bweek.forest,pred.var = c("crna","Minority"),chull = T)
  healthcare.system <- partial(bweek.forest,pred.var = c("crna","healthcare.system"),chull = T)
  pop.18.65 <- partial(bweek.forest,pred.var = c("crna","pop.18.65"),chull = T)
  popratio.65 <- partial(bweek.forest,pred.var = c("crna","popratio.65"),chull = T)
  test.sites.per.100k <- partial(bweek.forest,pred.var = c("crna","test.sites.per.100k"),chull = T)
)
##visualization, we use the partial dependence of inbound passenger before June 2022 as example here
inbound_f7_b6 <- autoplot(inbound,contour = F, legend.title = "Cases")+
  theme(panel.grid = element_blank(),panel.background = element_rect(color="black",fill = "transparent"))#for each factor
library(patchwork)
#air passenger whole plot
(inbound_f7_b6+inbound_f7_a6)/(total_passenger_f7_b6+total_passenger_f7_b6)/(inbound_f14_b6+inbound_f14_a6)/(total_passenger_f14_b6+total_passenger_f14_b6)