####################################################################################################################
################################### Carolina Johnson                    ############################################
################################### Civic Participation and Democracy   ############################################
################################### Publication Models & Figures        ############################################
################################### Last Updated: 6/21/2013             ####################################################
#######################################################################################################################

rm(list=ls())

library(foreign)
library(MASS)
library(xtable)
library(car)
library(tile)
library(simcf)
library(gplots)
library(lme4)
library(ordinal)

ameliaout1<-read.csv("ameliaout1.csv")
ameliaout2<-read.csv("ameliaout2.csv")
ameliaout3<-read.csv("ameliaout3.csv")
ameliaout4<-read.csv("ameliaout4.csv")
ameliaout5<-read.csv("ameliaout5.csv")


ameliaout1$bme <- rep(0,14095)
ameliaout1$bme[ameliaout1$ethnic6!=1] <- 1
ameliaout2$bme <- rep(0,14095)
ameliaout2$bme[ameliaout2$ethnic6!=1] <- 1
ameliaout3$bme <- rep(0,14095)
ameliaout3$bme[ameliaout3$ethnic6!=1] <- 1
ameliaout4$bme <- rep(0,14095)
ameliaout4$bme[ameliaout4$ethnic6!=1] <- 1
ameliaout5$bme <- rep(0,14095)
ameliaout5$bme[ameliaout5$ethnic6!=1] <- 1



regLabels <-  c("Northeast", "Northwest", "Yorkshire", "East Midlands", "West Midlands", 
                "East of England", "London", "Southeast", "Southwest", "Wales")

attitude.all<-c(ameliaout1$attitude,ameliaout2$attitude,ameliaout3$attitude,ameliaout4$attitude,ameliaout5$attitude)
valueseq<-seq(min(ameliaout1$participtot), max(ameliaout1$participtot), 1)

#################################################################################################################################
####### LMER Random Effects, varying intercepts & slopes

lmer.slopes <- vector("list", 5)
model.slope <- attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep

lmer.slopes[[1]] <- lmer(attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep +(1 + participtot + participlog|gor) , data=ameliaout1)
lmer.slopes[[2]] <- lmer(attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep +(1 + participtot + participlog|gor) , data=ameliaout2)
lmer.slopes[[3]] <- lmer(attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep +(1 + participtot + participlog|gor) , data=ameliaout3)
lmer.slopes[[4]] <- lmer(attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep +(1 + participtot + participlog|gor) , data=ameliaout4)
lmer.slopes[[5]] <- lmer(attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep +(1 + participtot + participlog|gor) , data=ameliaout5)

sims <- 10000
simbetasj <- vector("list", 10)
xhyp2 <- vector("list", 10)
valueseq <- seq(0,27,1)
yhyp.slopes <- vector("list", 10)
trace.slopes <- vector("list", 10)

## combining MI sets with random slopes
for (i in 1:5) {
  
  pe<-fixef(lmer.slopes[[i]])
  vc<-vcov(lmer.slopes[[i]])
  simbetaso <- mvrnorm(sims/5, pe, vc)
  simbetas <- simbetaso
  print(summary(simbetas[,3]))
  
  for(j in 1:10){
    simbetas[,2] <- simbetaso[,2] + ranef(lmer.slopes[[i]])$gor[j,2]
    simbetas[,3] <- simbetaso[,3] + ranef(lmer.slopes[[i]])$gor[j,3]
    print(ranef(lmer.slopes[[i]])$gor[j,3])
    simbetasj[[j]] <- rbind(simbetasj[[j]], simbetas)
  }
}

### rei= random intercept: combining MI sets
rei <- NULL
for(j in 1:10){
  re1 <- NULL
  for (i in 1:5){
    re1 <- c(re1, ranef(lmer.slopes[[i]])$gor[j,1])
  }
  resum<-sum(re1)
  rei <- c(rei, (resum/5))
}

#model coefs
lmer.slopes.pe <-apply(simbetaso,2,mean)
lmer.slopes.se <- apply(simbetaso,2,sd)

#ranefs 
re <- vector("list", 3)
for(i in 1:5){
  re[[1]] <- cbind(re[[1]],ranef(lmer.slopes[[i]])$gor[,1])
  re[[2]] <- cbind(re[[2]],ranef(lmer.slopes[[i]])$gor[,2])
  re[[3]] <- cbind(re[[3]],ranef(lmer.slopes[[i]])$gor[,3])
}
rownames(re[[1]])<-regLabels
rownames(re[[2]])<-regLabels
rownames(re[[3]])<-regLabels

remean <- function(x){apply(x,1,mean)}
reffects <- sapply(re,remean)
colnames(reffects) <- c("Intercept","Participation Dummy","Log(Participation)")

xtable(reffects)


##simulations and traces for predicted values for regional effects

for(j in 1:10){
  
  xhyp2[[j]] <- cfMake(model.slope, ameliaout5, nscen=length(valueseq))
  for(i in 1:length(valueseq)){    
    xhyp2[[j]] <- cfChange(xhyp2[[j]], "participtot", valueseq[i], scen=i)
    if (valueseq[i]>0) {
      xhyp2[[j]] <- cfChange(xhyp2[[j]], "participlog", log(valueseq[i]), scen=i)}
    else {xhyp2[[j]] <- cfChange(xhyp2[[j]], "participlog", 0, scen=i)}
  }
  
  
  yhyp.slopes[[j]]<- linearsimev(xhyp2[[j]], simbetasj[[j]])
  yhyp.slopes[[j]]$pe <- yhyp.slopes[[j]]$pe + rei[j]
  yhyp.slopes[[j]]$lower <- yhyp.slopes[[j]]$lower + rei[j]
  yhyp.slopes[[j]]$upper <- yhyp.slopes[[j]]$upper + rei[j]
  
  trace.slopes[[j]] <- lineplot(x = valueseq,
                                y = yhyp.slopes[[j]]$pe,
                                lower = yhyp.slopes[[j]]$lower,
                                upper = yhyp.slopes[[j]]$upper,
                                ci = list(mark="shaded"),
                                col = "blue",
                                plot = j
  )
  
}

## Code for plot with each region as separate tile - not used in published version ##
# tc <- tile(trace.slopes,
#            RxC = c(2,5), #increase these if you're using multiple plots at once
#            limits=c(0,27,2,10),   #xlim first, then ylim
#            xaxistitle= list(labels="Participation Index")  ,
#            yaxistitle=list(labels=""),
#            maintitle = list(labels=""),
#            plottitle = list(labels=c("Northeast","Northwest","Yorkshire","East Midlands", "West Midlands","East of England", "London", "Southeast", "Southwest", "Wales")),
#            gridlines = list(type="xy"),
#            frame=TRUE,
#            output=list
#            (outfile="lmer-slopes", type="pdf")
# )


### Simulations and trace for 'typical' line
xhyp2.typ <- cfMake(model.slope, ameliaout1, nscen=length(valueseq))

for(i in 1:length(valueseq)){
  xhyp2.typ <- cfChange(xhyp2.typ, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {xhyp2.typ <- cfChange(xhyp2.typ, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp2.typ <- cfChange(xhyp2.typ, "participlog", 0, scen=i)}
}

yhyp2.typ <-  linearsimev(xhyp2.typ, simbetaso)


trace.typ <- lineplot(x = valueseq,
                      y = yhyp2.typ$pe,
                      lower = yhyp2.typ$lower,
                      upper = yhyp2.typ$upper,
                      ci = list(mark="shaded"),
                      col = "dark green",
                      lex = 1.5,
                      plot = 1
)

for(j in 1:10) {
  trace.slopes[[j]]$lower<-NULL
  trace.slopes[[j]]$upper<-NULL
  trace.slopes[[j]]$col <- "black"
  trace.slopes[[j]]$lex <- .7
  trace.slopes[[j]]$lty <- "dashed"
  trace.slopes[[j]]$plot <- 1
}

#### Making final plot with typical and region lines - FIGURE 3 #####
tc <- tile(trace.slopes,trace.typ,
           limits=c(0,27,3.5,8),   
           xaxistitle= list(labels="Participation Index")  ,
           yaxistitle=list(labels="Attitude Index"),
           rightaxistitle=list(labels="Attitude Index, in s.d. units", cex=.8),
           rightaxis=list(at=mean(attitude.all)+(sd(attitude.all)*c(-.5,0,.5,1,1.5)), labels=c("-.5","0","0.5","1","1.5"), 
                          tick.length=.2,add=T,cex=.8),
           maintitle = list(labels=""),
           gridlines = list(type="xy"),
           frame=TRUE,
           width = list(rightaxis.labelspace=.01,rightaxistitle = .5),
           output = list(width = 5, outfile="typ-slopes", type="pdf")
)




#############################################################################################################
### Figure 4:  Ropeladder
### First difference  simulations and plotting

xscen.slopes <- vector("list",10)
yhyp.slopes <- vector("list",10)
trace.rl.slopes  <- vector("list",10)

for(j in 1:10){
  xscen.slopes[[j]] <- cfMake(model.slope, data=ameliaout5, nscen=2)
  xscen.slopes[[j]] <- cfChange(xscen.slopes[[j]], "participtot", xpre=0, x=1, scen=1)
  xscen.slopes[[j]] <- cfChange(xscen.slopes[[j]], "participlog", xpre=0, x=log(1), scen=1)
  xscen.slopes[[j]] <- cfChange(xscen.slopes[[j]], "participtot", xpre=5, x=6, scen=2)
  xscen.slopes[[j]] <- cfChange(xscen.slopes[[j]], "participlog", xpre=log(5), x=log(6), scen=2)                                   
  yhyp.slopes[[j]]<- linearsimfd(xscen.slopes[[j]], simbetasj[[j]])
}

slopesLow <- matrix(NA, nrow=10, ncol = 3)
slopesHi <- matrix(NA, nrow=10, ncol = 3)

for(j in 1:10){
  slopesLow[j,1] <- yhyp.slopes[[j]]$pe[1]
  slopesLow[j,2] <- yhyp.slopes[[j]]$lower[1]
  slopesLow[j,3] <- yhyp.slopes[[j]]$upper[1]
  slopesHi[j,1] <- yhyp.slopes[[j]]$pe[2]
  slopesHi[j,2] <- yhyp.slopes[[j]]$lower[2]
  slopesHi[j,3] <- yhyp.slopes[[j]]$upper[2]
}
colnames(slopesLow) <- c("pe","lower", "upper")
rownames(slopesLow) <- regLabels
colnames(slopesHi) <- c("pe","lower", "upper")
rownames(slopesHi) <- regLabels

slopesLow <- as.data.frame(slopesLow)
slopesHi <- as.data.frame(slopesHi)

slopesLowS <- ladderSort(x=slopesLow, sort.by = slopesHi$pe)
slopesHiS <- ladderSort(x=slopesHi)

trace.rl.slopes1 <- ropeladder(
  x = slopesLowS$pe,
  lower = slopesLowS$lower,
  upper = slopesLowS$upper,
  labels=rownames(slopesLowS),
  col = "blue",
  entryheight = .2,
  plot = 1
)

trace.rl.slopes2 <- ropeladder(
  x = slopesHiS$pe,
  lower = slopesHiS$lower,
  upper = slopesHiS$upper,
  labels=rownames(slopesHiS),
  pch=17,
  col = "dark green",
  plot = 1
)

legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(slopesLowS$pe[1],slopesHiS$pe[1]),
                   y=c(.981,.962),
                   #   cex=.6,
                   col=c("blue","dark green"),
                   lineheight=.9,
                   clip=F,
                   plot=1)

trace.rl.slopes1$entryheight <- 0.1
trace.rl.slopes2$entryheight <-0.1

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      col="dark gray",
                      plot = c(1)
)


tc <- tile(trace.rl.slopes1,trace.rl.slopes2,legend,
           vertmark,
           #           rug,
           limits=c(-.1,.65,0,1),
           xaxistitle = list(labels="Change in Predicted Attitude"),
           xaxis = list(at=c(0,.2,.4,.6)),
           gridlines=list(type="xy",col="gray"),
           topaxis = list(add=T, at=c(0,.2,.4,.6), labels=round(sd(attitude.all)*c(0,.2,.4,.6),digits=2), 
                          tick.length=0 ),
           topaxistitle = list(labels="Standard deviation unit change in Attitude"),
           #width = list(null=4),
           output = list(file="rl-slopes", width = 8)
)

############################################################################################################
########## Model Implications  #############################################################################
############################################################################################################

#################################################################################################################################
######## LMER Interactions with Satisfaction : with ropeladder plots - Figure 5 

lmer.sat <- vector("list",5)
model.sat <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))*satisf + participlog*satisf + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.sat <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))*satisf + participlog*satisf + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1|gor)
lmer.sat[[1]] <- lmer(model.lmer.sat , data=ameliaout1)
lmer.sat[[2]] <- lmer(model.lmer.sat , data=ameliaout2)
lmer.sat[[3]] <- lmer(model.lmer.sat , data=ameliaout3)
lmer.sat[[4]] <- lmer(model.lmer.sat , data=ameliaout4)
lmer.sat[[5]] <- lmer(model.lmer.sat , data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- fixef(lmer.sat[[i]])
  vc <- vcov(lmer.sat[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc)
  simbetas <- rbind(simbetas, simbeta)
}
lmer.sat.bhat <- apply(simbetas,2,mean)
lmer.sat.se <- apply(simbetas,2,sd)

### Plotting: simulated first differences and ropeladder
xscen1 <- xscen <- cfMake(model.sat, data=ameliaout1, nscen=5)
xscen <-cfName(xscen, "0 to 1 Participation, Satisfaction=1", scen=5)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=5)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=5) 
xscen <- cfChange(xscen, "satisf", x=0, xpre=0, scen=5)

xscen <- cfName(xscen, "0 to 1 Participation, Satisfaction=2", scen=4)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=4)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=4)
xscen <- cfChange(xscen, "satisf", x=1, xpre=1, scen=4)

xscen <- cfName(xscen, "0 to 1 Participation, Satisfaction=3", scen=3)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=3)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=3)
xscen <- cfChange(xscen, "satisf", x=2, xpre=2, scen=3)

xscen <- cfName(xscen, "0 to 1 Participation, Satisfaction=4", scen=2)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=2)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=2)
xscen <- cfChange(xscen, "satisf", x=3, xpre=3, scen=2)

xscen <- cfName(xscen, "0 to 1 Participation, Satisfaction=5", scen=1)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=1)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=1)
xscen <- cfChange(xscen, "satisf", x=4, xpre=4, scen=1)

#
xscen1 <- cfName(xscen1, "5-6 Participation, Satisfaction=1", scen=5)
xscen1 <- cfChange(xscen1, "participtot", x=6, xpre=5, scen=5)
xscen1 <- cfChange(xscen1, "participlog", x=log(6), xpre=log(5), scen=5)
xscen1 <- cfChange(xscen1, "satisf", x=0, xpre=0, scen=5)

xscen1 <- cfName(xscen1, "5-6 Participation, Satisfaction=2", scen=4)
xscen1 <- cfChange(xscen1, "participtot", x=6, xpre=5, scen=4)
xscen1 <- cfChange(xscen1, "participlog", x=log(6), xpre=log(5), scen=4)
xscen1 <- cfChange(xscen1, "satisf", x=1, xpre=1, scen=4)

xscen1 <- cfName(xscen1, "5-6 Participation, Satisfaction=3", scen=3)
xscen1 <- cfChange(xscen1, "participtot", x=6, xpre=5, scen=3)
xscen1 <- cfChange(xscen1, "participlog", x=log(6), xpre=log(5), scen=3)
xscen1 <- cfChange(xscen1, "satisf", x=2, xpre=2, scen=3)

xscen1 <- cfName(xscen1, "5-6 Participation, Satisfaction=4", scen=2)
xscen1 <- cfChange(xscen1, "participtot", x=6, xpre=5, scen=2)
xscen1 <- cfChange(xscen1, "participlog", x=log(6), xpre=log(5), scen=2)
xscen1 <- cfChange(xscen1, "satisf", x=3, xpre=3, scen=2)

xscen1 <- cfName(xscen1, "5-6 Participation, Satisfaction=5", scen=1)
xscen1 <- cfChange(xscen1, "participtot", x=6, xpre=5, scen=1)
xscen1 <- cfChange(xscen1, "participlog", x=log(6), xpre=log(5), scen=1)
xscen1 <- cfChange(xscen1, "satisf", x=4, xpre=4, scen=1)

### Calculate FDs
fds.sat <- linearsimfd(xscen, simbetas, ci=.95)
fds1.sat <- linearsimfd(xscen1, simbetas, ci=.95)

#Building traces for satisfaction plots
trace.satlad <- ropeladder(x=fds.sat$pe,
                           lower=fds.sat$lower,
                           upper=fds.sat$upper, 
                           labels=c("Highest Satisfaction","Satisfaction=4","Satisfaction=3","Satisfaction=2","Lowest Satisfaction"),
                           col="blue",
                           plot=1
)
trace.satlad1 <- ropeladder(x=fds1.sat$pe,
                            lower=fds1.sat$lower,
                            upper=fds1.sat$upper, 
                            col="green",
                            plot=1
)


legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(fds.sat$pe[1],fds1.sat$pe[1]),
                   y=c(.955,.955),
                   col=c("blue","green"),
                   plot=1)

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lex=.8,
                      plot = 1)

trace.satlad$entryheight <- .15
trace.satlad1$entryheight <-.15

# Plotting satisfaction using Tile - ** Figure 5 **
tc <- tile(trace.satlad,trace.satlad1,legend,
           vertmark,
           limits = c(-.05,.58,0,1),
           xaxistitle = list(labels="Change in Predicted Attitude"),
           xaxis = list(at= seq(0,.55,.1), labels=c(0,"",.2,"",.4,"")),
           topaxis = list(add=T, at=c(0,.2,.4), labels=round(sd(attitude.all)*c(0,.2,.4),digits=2), 
                          tick.length=0 ),
           topaxistitle = list(labels="Standard deviation unit change in Attitude"),
           gridlines=list(type="xy"),
           width = list(null=5, plot=1, rightborder=0, leftborder=0),
           height = list(plot=.5, topborder=0,bottomborder=0), 
           output = list(file="sat-ladder")
)


# simulate DD:
source("C:/Users/Carolina/Documents/UW PhD/TileSimcf/Gitattempt/tile-simcf/simcf/R/linearsimdd.r")
# set up CF:

xscenls <- xscen <- cfMake(model.sat, data=ameliaout1, nscen=2)
xscen <-cfName(xscen, "0 to 1, High Sat", scen=1)
xscen <- cfChange(xscen, "participtot", x=1, xpre=0, scen=1)
xscen <- cfChange(xscen, "participlog", x=log(1), xpre=0, scen=1) 
xscen <- cfChange(xscen, "satisf", x=4, xpre=4, scen=1)

xscen <-cfName(xscen, "5 to 6, High Sat", scen=2)
xscen <- cfChange(xscen, "participtot", x=6, xpre=5, scen=2)
xscen <- cfChange(xscen, "participlog", x=log(6), xpre=log(5), scen=2) 
xscen <- cfChange(xscen, "satisf", x=4, xpre=4, scen=2)

xscenls <-cfName(xscenls, "0 to 1, Low Sat", scen=1)
xscenls <- cfChange(xscenls, "participtot", x=1, xpre=0, scen=1)
xscenls <- cfChange(xscenls, "participlog", x=log(1), xpre=0, scen=1)
xscenls <- cfChange(xscenls, "satisf", x=0, xpre=0, scen=1)



xscenls <-cfName(xscenls, "5 to 6, Low Sat", scen=2)
xscenls <- cfChange(xscenls, "participtot", x=6, xpre=5, scen=2)
xscenls <- cfChange(xscenls, "participlog", x=log(6), xpre=log(5), scen=2) 
xscenls <- cfChange(xscenls, "satisf", x=0, xpre=0, scen=2)

satdd <- linearsimdd(x=xscen,xd=xscenls, simbetas, ci=c(.95))

#################################################################################################################################
######## Ordered probit of trust ~ participation -- 6
polr.trust <- vector("list",5)
model.trust <- as.factor(parltrust) ~  I(as.numeric(participtot>0)) + participlog + trust + age + female + bme + alevels + incomereal + econstat + reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
polr.trust[[1]] <- polr(model.trust, method="probit", Hess=TRUE, data=ameliaout1)
polr.trust[[2]] <- polr(model.trust, method="probit", Hess=TRUE, data=ameliaout2)
polr.trust[[3]] <- polr(model.trust, method="probit", Hess=TRUE, data=ameliaout3)
polr.trust[[4]] <- polr(model.trust, method="probit", Hess=TRUE, data=ameliaout4)
polr.trust[[5]] <- polr(model.trust, method="probit", Hess=TRUE, data=ameliaout5)

sims <- 1000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- c(polr.trust[[i]]$coefficients, polr.trust[[i]]$zeta)
  vc <- vcov(polr.trust[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc )
  simbetas <- rbind(simbetas, simbeta)
}
polr.trust.bhat <- apply(simbetas,2,mean)
polr.trust.se <- apply(simbetas,2,sd)
trust.tbl <- cbind(polr.trust.bhat, polr.trust.se)

xhyp.trust <- cfMake(model.trust, ameliaout1,nscen=length(valueseq))
for(i in 1:length(valueseq)){
  xhyp.trust <- cfChange(xhyp.trust, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {
    xhyp.trust <- cfChange(xhyp.trust, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp.trust <- cfChange(xhyp.trust, "participlog", 0, scen=i)}
}
# Simulate E(Sat) for each counterfactual, polr version
#   Must set constant = NA to predict from polr
yhyp.plr <- oprobitsimev(xhyp.trust, simbetas, constant=NA, cat=4)
print(yhyp.plr)


polrTrace1 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,1], #expected probability on y-axis
                       lower=yhyp.plr$lower[,1], # lower confidence interval
                       upper=yhyp.plr$upper[,1], #upper confidence interval
                       col="green", 
                       plot=1)
polrTrace2 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,2], #expected probability on y-axis
                       lower=yhyp.plr$lower[,2], # lower confidence interval
                       upper=yhyp.plr$upper[,2], #upper confidence interval
                       col="blue", 
                       plot=1)
polrTrace3 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,3], #expected probability on y-axis
                       lower=yhyp.plr$lower[,3], # lower confidence interval
                       upper=yhyp.plr$upper[,3], #upper confidence interval
                       col="magenta", 
                       plot=1)
polrTrace4 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,4], #expected probability on y-axis
                       lower=yhyp.plr$lower[,4], # lower confidence interval
                       upper=yhyp.plr$upper[,4], #upper confidence interval
                       col="red", 
                       plot=1)


# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Not at all","Not very much","A fair amount","A lot"),
                       x=c( 10, 20, 17, 3),
                       y=c( .15, .48, .3, .09),
                       col=c("green", "blue", "magenta", "red"),
                       fontsize=8,
                       plot=1)


legendTrace <- textTile(labels=c("Ordered probit estimates:", "95% confidence", "interval is shaded"),
                        x=c(3.65, 3.65, 3.65),
                        y=c(0.93, 0.86, 0.79),
                        fontsize=(8),
                        plot=4)

# Plot traces using tile - ** Figure 6 **

tc<-tile(polrTrace1,
         polrTrace2,
         polrTrace3,
         polrTrace4,
         labelTrace,
         #     legendTrace,
         limits=c(0,27,0,.8),
         gridlines=list(type="xy"),
         xaxistitle=list(labels="Participation Index", cex=.8),
         yaxistitle=list(labels="Probability of reporting each level of trust", cex=.8),
         frame=TRUE,
         width = list(null=3,leftborder = 0, rightborder = 1),
         height = list(topborder = 0, bottomborder = 0),
         output=list(file="trust")
)


#################################################################################################################################
######## Breakdowns of legitimacy measure: Figure 7

#### ~~~~ Ordered probit ~ local efficacy
polr.infloc <- vector("list",5)
model.infloc <- as.factor(infloc) ~  I(as.numeric(participtot>0)) + participlog + age + female + bme + alevels + incomereal + econstat + reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
polr.infloc[[1]] <- polr(model.infloc, method="probit", Hess=T, data=ameliaout1)
polr.infloc[[2]] <- polr(model.infloc, method="probit", Hess=T, data=ameliaout2)
polr.infloc[[3]] <- polr(model.infloc, method="probit", Hess=T, data=ameliaout3)
polr.infloc[[4]] <- polr(model.infloc, method="probit", Hess=T, data=ameliaout4)
polr.infloc[[5]] <- polr(model.infloc, method="probit", Hess=T, data=ameliaout5)

sims <- 1000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- c(polr.infloc[[i]]$coefficients, polr.infloc[[i]]$zeta)
  vc <- vcov(polr.infloc[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc )
  simbetas <- rbind(simbetas, simbeta)
}
polr.infloc.bhat <- apply(simbetas,2,mean)
polr.infloc.se <- apply(simbetas,2,sd)
infloc.tbl <- cbind(polr.infloc.bhat, polr.infloc.se)

xhyp.infloc <- cfMake(model.infloc, ameliaout1,nscen=length(valueseq))
for(i in 1:length(valueseq)){
  xhyp.infloc <- cfChange(xhyp.infloc, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {
    xhyp.infloc <- cfChange(xhyp.infloc, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp.infloc <- cfChange(xhyp.infloc, "participlog", 0, scen=i)}
}


# Simulate E(Sat) for each counterfactual, polr version
#   Must set constant = NA to predict from polr
yhyp.infloc <- oprobitsimev(xhyp.infloc, simbetas, constant=NA, cat=4)
print(yhyp.infloc)


inflocTrace1 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infloc$pe[,1], #expected probability on y-axis
                         lower=yhyp.infloc$lower[,1], # lower confidence interval
                         upper=yhyp.infloc$upper[,1], #upper confidence interval
                         col="dark green", 
                         plot=1)
inflocTrace2 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infloc$pe[,2], #expected probability on y-axis
                         lower=yhyp.infloc$lower[,2], # lower confidence interval
                         upper=yhyp.infloc$upper[,2], #upper confidence interval
                         col="blue", 
                         plot=1)
inflocTrace3 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infloc$pe[,3], #expected probability on y-axis
                         lower=yhyp.infloc$lower[,3], # lower confidence interval
                         upper=yhyp.infloc$upper[,3], #upper confidence interval
                         col="magenta", 
                         plot=1)
inflocTrace4 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infloc$pe[,4], #expected probability on y-axis
                         lower=yhyp.infloc$lower[,4], # lower confidence interval
                         upper=yhyp.infloc$upper[,4], #upper confidence interval
                         col="red", 
                         plot=1)


# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Not at all","Not very much","A fair amount","A lot"),
                       x=c( 6, 22, 15, 24),
                       y=c( .04, .15, .5, .37),
                       col=c("dark green", "blue", "magenta", "red"),
                       fontsize=8,
                       plot=1)

# Plot traces using tile -- ** Figure 7b **

tc<-tile(inflocTrace1,
         inflocTrace2,
         inflocTrace3,
         inflocTrace4,
         labelTrace,
         limits=c(0,27,0,.8),
         gridlines=list(type="xy"),
         xaxistitle=list(labels="Participation Index", cex=.9),
         yaxistitle=list(labels="Pr(levels of local political influence)", cex=.9),
         frame=TRUE,
         width = list(null=3),
         output=list(file="infloc")#, width=4.5)
)

#### ~~~~ Ordered probit ~ national efficacy
polr.infnat <- vector("list",5)
model.infnat <- as.factor(infnat) ~  I(as.numeric(participtot>0)) + participlog + age + female + bme + alevels + incomereal + econstat +natnews + reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
polr.infnat[[1]] <- polr(model.infnat, method="probit", Hess=T, data=ameliaout1)
polr.infnat[[2]] <- polr(model.infnat, method="probit", Hess=T, data=ameliaout2)
polr.infnat[[3]] <- polr(model.infnat, method="probit", Hess=T, data=ameliaout3)
polr.infnat[[4]] <- polr(model.infnat, method="probit", Hess=T, data=ameliaout4)
polr.infnat[[5]] <- polr(model.infnat, method="probit", Hess=T, data=ameliaout5)

sims <- 1000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- c(polr.infnat[[i]]$coefficients, polr.infnat[[i]]$zeta)
  vc <- vcov(polr.infnat[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc )
  simbetas <- rbind(simbetas, simbeta)
}
polr.infnat.bhat <- apply(simbetas,2,mean)
polr.infnat.se <- apply(simbetas,2,sd)
infnat.tbl <- cbind(polr.infnat.bhat, polr.infnat.se)

xhyp.infnat <- cfMake(model.infnat, ameliaout1,nscen=length(valueseq))
for(i in 1:length(valueseq)){
  xhyp.infnat <- cfChange(xhyp.infnat, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {
    xhyp.infnat <- cfChange(xhyp.infnat, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp.infnat <- cfChange(xhyp.infnat, "participlog", 0, scen=i)}
}


# Simulate E(Sat) for each counterfactual, polr version
#   Must set constant = NA to predict from polr
yhyp.infnat <- oprobitsimev(xhyp.infnat, simbetas, constant=NA, cat=4)
print(yhyp.infnat)


infnatTrace1 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infnat$pe[,1], #expected probability on y-axis
                         lower=yhyp.infnat$lower[,1], # lower confidence interval
                         upper=yhyp.infnat$upper[,1], #upper confidence interval
                         col="dark green", 
                         plot=1)
infnatTrace2 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infnat$pe[,2], #expected probability on y-axis
                         lower=yhyp.infnat$lower[,2], # lower confidence interval
                         upper=yhyp.infnat$upper[,2], #upper confidence interval
                         col="blue", 
                         plot=1)
infnatTrace3 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infnat$pe[,3], #expected probability on y-axis
                         lower=yhyp.infnat$lower[,3], # lower confidence interval
                         upper=yhyp.infnat$upper[,3], #upper confidence interval
                         col="magenta", 
                         plot=1)
infnatTrace4 <- lineplot(x=valueseq, # milforce on x-axis
                         y=yhyp.infnat$pe[,4], #expected probability on y-axis
                         lower=yhyp.infnat$lower[,4], # lower confidence interval
                         upper=yhyp.infnat$upper[,4], #upper confidence interval
                         col="red", 
                         plot=1)


# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Not at all","Not very much","A fair amount","A lot"),
                       x=c( 3, 20, 15, 24),
                       y=c( .3, .41, .2, .12),
                       col=c("dark green", "blue", "magenta", "red"),
                       fontsize=8,
                       plot=1)

# Plot traces using tile - ** Figure 7a **

tc<-tile(RxC = c(1,1),
         infnatTrace1,
         infnatTrace2,
         infnatTrace3,
         infnatTrace4,
         labelTrace,
         #     legendTrace,
         limits=c(0,27,0,.8),
         gridlines=list(type="xy"),
         xaxistitle=list(labels="Participation Index", cex=.9),
         yaxistitle=list(labels="Pr(levels of national political influence)", cex=.9),
         frame=TRUE,
         width = list(null=3),
         output=list(file="infnat")#, width=4.5)
)



########################################################################################
### ~~~ 5) Ordered probit impinfl  ~ participation
polr.impinfl <- vector("list",5)
model.impinfl <- as.factor(impinfl) ~  I(as.numeric(participtot>0)) + participlog + age + female + bme + alevels + incomereal + econstat + natnews + reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
polr.impinfl[[1]] <- polr(model.impinfl, method="probit", Hess=T, data=ameliaout1)
polr.impinfl[[2]] <- polr(model.impinfl, method="probit", Hess=T, data=ameliaout2)
polr.impinfl[[3]] <- polr(model.impinfl, method="probit", Hess=T, data=ameliaout3)
polr.impinfl[[4]] <- polr(model.impinfl, method="probit", Hess=T, data=ameliaout4)
polr.impinfl[[5]] <- polr(model.impinfl, method="probit", Hess=T, data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- c(polr.impinfl[[i]]$coefficients, polr.impinfl[[i]]$zeta)
  vc <- vcov(polr.impinfl[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc )
  simbetas <- rbind(simbetas, simbeta)
}
polr.impinfl.bhat <- apply(simbetas,2,mean)
polr.impinfl.se <- apply(simbetas,2,sd)
impinfl.tbl <- cbind(polr.impinfl.bhat, polr.impinfl.se)

xhyp.impinfl <- cfMake(model.impinfl, ameliaout1,nscen=length(valueseq))
for(i in 1:length(valueseq)){
  xhyp.impinfl <- cfChange(xhyp.impinfl, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {
    xhyp.impinfl <- cfChange(xhyp.impinfl, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp.impinfl <- cfChange(xhyp.impinfl, "participlog", 0, scen=i)}
}


# Simulate E(Sat) for each counterfactual, polr version
#   Must set constant = NA to predict from polr
yhyp.plr <- oprobitsimev(xhyp.impinfl, simbetas, constant=NA, cat=4)
print(yhyp.plr)


polrTrace1 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,1], #expected probability on y-axis
                       lower=yhyp.plr$lower[,1], # lower confidence interval
                       upper=yhyp.plr$upper[,1], #upper confidence interval
                       col="dark green", 
                       plot=1)
polrTrace2 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,2], #expected probability on y-axis
                       lower=yhyp.plr$lower[,2], # lower confidence interval
                       upper=yhyp.plr$upper[,2], #upper confidence interval
                       col="blue", 
                       plot=1)
polrTrace3 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,3], #expected probability on y-axis
                       lower=yhyp.plr$lower[,3], # lower confidence interval
                       upper=yhyp.plr$upper[,3], #upper confidence interval
                       col="magenta", 
                       plot=1)
polrTrace4 <- lineplot(x=valueseq, # milforce on x-axis
                       y=yhyp.plr$pe[,4], #expected probability on y-axis
                       lower=yhyp.plr$lower[,4], # lower confidence interval
                       upper=yhyp.plr$upper[,4], #upper confidence interval
                       col="red", 
                       plot=1)


# Set up traces with labels and legend
labelTrace <- textTile(labels=c("Not at all","Not very much","A fair amount","A lot"),
                       x=c( 3, 20, 20, 20),
                       y=c( .02, .05, .3, .68),
                       col=c("dark green", "blue", "magenta", "red"),
                       fontsize=8,
                       plot=1)


legendTrace <- textTile(labels=c("Ordered probit estimates:", "95% confidence", "interval is shaded"),
                        x=c(3.65, 3.65, 3.65),
                        y=c(0.93, 0.86, 0.79),
                        fontsize=(8),
                        plot=4)

# Plot traces using tile -  ** Figure 7c **

tc<-tile(RxC = c(1,1),
         polrTrace1,
         polrTrace2,
         polrTrace3,
         polrTrace4,
         labelTrace,
         #     legendTrace,
         limits=c(0,27,0,.8),
         gridlines=list(type="xy"),
         #     xaxis=list(at=c(valueseq), labels=c("low",2,3,4,"high"), fontsize=8),
         #     yaxis=list(fontsize=8),
         xaxistitle=list(labels="Participation Index", cex=.9),
         yaxistitle=list(labels="Pr(levels of importance of having influence)", cex=.9),
         #     plottitle=list(cex=.8,
         #     width=list(plottitle=2),
         frame=TRUE,
         width = list(null=3),
         output=list(file="impinfl")#, width=4.5)
)



########################################################################################
#### ~~~~ beta-binomial ~ values
library(aod)
source(file="C:/Users/Carolina/Documents/UW PhD/TileSimcf/betabinsimev.r")
ameliaout1$values<-ameliaout1$rival4a+ameliaout1$rival8a+ameliaout1$rival16a
ameliaout2$values<-ameliaout2$rival4a+ameliaout2$rival8a+ameliaout2$rival16a
ameliaout3$values<-ameliaout3$rival4a+ameliaout3$rival8a+ameliaout3$rival16a
ameliaout4$values<-ameliaout4$rival4a+ameliaout4$rival8a+ameliaout4$rival16a
ameliaout5$values<-ameliaout5$rival4a+ameliaout5$rival8a+ameliaout5$rival16a
values.all<-c(ameliaout1$values,ameliaout2$values,ameliaout3$values,ameliaout4$values,ameliaout5$values) #mean same in all
c(sd(ameliaout1$values),sd(ameliaout2$values),sd(ameliaout3$values),sd(ameliaout4$values),sd(ameliaout5$values)) #sd same in all = .6049369
values.sd<-.6049369

bbin.values<-vector("list",5)
model.values <- cbind(values, 3-values) ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.values <- cbind(values, 3-values) ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels


bbin.values[[1]] <- betabin(model.values, ~1, data=ameliaout1, link="logit")
bbin.values[[2]] <- betabin(model.values, ~1, data=ameliaout2, link="logit")
bbin.values[[3]] <- betabin(model.values, ~1, data=ameliaout3, link="logit")
bbin.values[[4]] <- betabin(model.values, ~1, data=ameliaout4, link="logit")
bbin.values[[5]] <- betabin(model.values, ~1, data=ameliaout5, link="logit")


sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe<-slot(bbin.values[[i]],"fixed.param")
  vc<-vcov(bbin.values[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc )
  simbetas <- rbind(simbetas, simbeta)
}
bbin.values.bhat <- apply(simbetas,2,mean)
bbin.values.se <- apply(simbetas,2,sd)


valueseq<-seq(min(ameliaout1$participtot), max(ameliaout1$participtot), 1)
xhyp.bb <- cfMake(model.values, ameliaout5, nscen=length(valueseq))

for(i in 1:length(valueseq)){
  xhyp.bb <- cfChange(xhyp.bb, "participtot", valueseq[i], scen=i)
  if (valueseq[i]>0) {
    xhyp.bb <- cfChange(xhyp.bb, "participlog", log(valueseq[i]), scen=i)}
  else {xhyp.bb <- cfChange(xhyp.bb, "participlog", 0, scen=i)}
}

yhyp.bb <- (betabinsimev(xhyp.bb,simbetas, ci=.95, m=3))

trace.bb <- lineplot(x = valueseq,
                     y = yhyp.bb$pe,
                     lower = yhyp.bb$lower,
                     upper = yhyp.bb$upper,
                     ci = list(mark="shaded"),
                     col = "blue",
                     plot = 1
)

## Plotting values -- ** Figure 7d **
tc <- tile(trace.bb,
           limits=c(0,27,.2,1),
           frame=T,
           gridlines=list(type="xy"),
           yaxistitle=list(labels=c("Predicted values score")),
           xaxistitle=list(labels=c("Participation Index")),
           rightaxis=list(at=mean(values.all)+(values.sd)*c(-.5,0,.5), labels=c("-.5", "0","0.5"), 
                          ticks=T,tick.length=.2,add=T, cex = .8), # paste(round(mean(values.all),digits=2),"(mean)")
           rightaxistitle = list(labels="Predicted values score, s.d. units from mean", cex=.8),
           width = list(rightborder = 0, leftborder=0, rightaxis.labelspace=.01),
           height = list(topborder = 0, bottomborder = 0),
           #titleboxes = TRUE,
           #axisboxes = TRUE,
           output=list(file="bbin", width = 4.5)
)


#################################################################################################################################
######## Other interactions with Participation: looking for social capital, participation effects - Figure 8

# econstat
model.econ <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* econstat + participlog*econstat + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.econ <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* econstat + participlog*econstat + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1 |gor)

lmer.econ <- vector("list",5)
lmer.econ[[1]] <- lmer(model.lmer.econ , data=ameliaout1)
lmer.econ[[2]] <- lmer(model.lmer.econ , data=ameliaout2)
lmer.econ[[3]] <- lmer(model.lmer.econ , data=ameliaout3)
lmer.econ[[4]] <- lmer(model.lmer.econ , data=ameliaout4)
lmer.econ[[5]] <- lmer(model.lmer.econ , data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- fixef(lmer.econ[[i]])
  vc <- vcov(lmer.econ[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc)
  simbetas <- rbind(simbetas, simbeta)
}
lmer.econ.bhat <- apply(simbetas,2,mean)
lmer.econ.se <- apply(simbetas,2,sd)

# simulations and plotting for socio-econ status:
xscen.econ1fd <- xscen.econ2fd <- cfMake(model.econ, ameliaout1, nscen=4)

xscen.econ1fd <- cfName(xscen.econ1fd, "Unemployed/Student", scen=4)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participtot", xpre=0, x=1, scen=4)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participlog", xpre=0, x=log(1), scen=4)
xscen.econ1fd <- cfChange(xscen.econ1fd, "econstat", xpre=0, x=0, scen=4)

xscen.econ1fd <- cfName(xscen.econ1fd, "Semi-routine/routine", scen=3)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participtot", xpre=0, x=1, scen=3)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participlog", xpre=0, x=log(1), scen=3)
xscen.econ1fd <- cfChange(xscen.econ1fd, "econstat", xpre=1, x=1, scen=3)

xscen.econ1fd <- cfName(xscen.econ1fd, "Small employers", scen=2)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participtot", xpre=0, x=1, scen=2)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participlog", xpre=0, x=log(1), scen=2)
xscen.econ1fd <- cfChange(xscen.econ1fd, "econstat", xpre=2, x=2, scen=2)


xscen.econ1fd <- cfName(xscen.econ1fd, "Management", scen=1)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participtot", xpre=0, x=1, scen=1)
xscen.econ1fd <- cfChange(xscen.econ1fd, "participlog", xpre=0, x=log(1), scen=1)
xscen.econ1fd <- cfChange(xscen.econ1fd, "econstat", xpre=3, x=3, scen=1)

xscen.econ2fd <- cfName(xscen.econ2fd, "4 to 5 Participation, SES = Full time student/longterm unemployed", scen=4)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participtot", xpre=5, x=6, scen=4)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participlog", xpre=log(5), x=log(6), scen=4)
xscen.econ2fd <- cfChange(xscen.econ2fd, "econstat", xpre=0, x=0, scen=4)

xscen.econ3fd <- cfName(xscen.econ2fd, "4 to 5 Participation, SES = Semi-routine/routine worker", scen=3)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participtot", xpre=5, x=6, scen=3)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participlog", xpre=log(5), x=log(6), scen=3)
xscen.econ2fd <- cfChange(xscen.econ2fd, "econstat", xpre=1, x=1, scen=3)

xscen.econ2fd <- cfName(xscen.econ2fd, "4 to 5 Participation, SES = Intermediate, small employers", scen=2)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participtot", xpre=5, x=6, scen=2)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participlog", xpre=log(5), x=log(6), scen=2)
xscen.econ2fd <- cfChange(xscen.econ2fd, "econstat", xpre=2, x=2, scen=2)

xscen.econ2fd <- cfName(xscen.econ2fd, "4 to 5 Participation, SES = Higher and lower management", scen=1)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participtot", xpre=5, x=6, scen=1)
xscen.econ2fd <- cfChange(xscen.econ2fd, "participlog", xpre=log(5), x=log(6), scen=1)
xscen.econ2fd <- cfChange(xscen.econ2fd, "econstat", xpre=3, x=3, scen=1)

fdhyp.econ1 <- linearsimfd(xscen.econ1fd, simbetas)
fdhyp.econ2 <- linearsimfd(xscen.econ2fd, simbetas)

trace.econfd1 <- ropeladder(
  x = fdhyp.econ1$pe,
  lower = fdhyp.econ1$lower,
  upper = fdhyp.econ1$upper,
  labels=row.names(xscen.econ1fd$x[1]),
  cex=.9,
  col = "blue",
  plot = 1
)
trace.econfd2 <- ropeladder(
  x = fdhyp.econ2$pe,
  lower = fdhyp.econ2$lower,
  upper = fdhyp.econ2$upper,
  #              cex=.8,
  col = "green",
  plot = 1
)
trace.econfd1$entryheight <- 0.18

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      plot = 1)

legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(.35,.075),
                   y=c(.955,.935),
                   #                   cex=.7,
                   col=c("blue","dark green"),
                   lineheight=.9,
                   clip=F,
                   plot=1)

## Plotting socio-econ status only, not in paper
tc <- tile(trace.econfd1, trace.econfd2,
           legend,
           vertmark,
           limits=c(0,.65,0,1),
           xaxistitle = list(labels="Change in Predicted Attitude"), 
           xaxis = list(labels=c(0,"",.2,"",.4,"",.6)),
           yaxistitle=list(labels=""),
           topaxis = list(add=T, at=c(0,.2,.4,.6), labels=round(sd(attitude.all)*c(0,.2,.4,.6),digits=2), 
                          tick.length=0 ),
           topaxistitle = list(labels="S.D. unit change in Predicted Attitude"),
           #maintitle = list(labels=""),
           gridlines = list(type="xy"),
           frame=F,
           width = list(leftborder=0),
           #           titleboxes=T,
           output=list(width=5,file="econ-rl")
)


#####################################################################################################################################
#education
model.edu <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* alevels + participlog*alevels + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.edu <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* alevels + participlog*alevels + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1|gor)

lmer.edu <- vector("list",5)
lmer.edu[[1]] <- lmer(model.lmer.edu , data=ameliaout1)
lmer.edu[[2]] <- lmer(model.lmer.edu , data=ameliaout2)
lmer.edu[[3]] <- lmer(model.lmer.edu , data=ameliaout3)
lmer.edu[[4]] <- lmer(model.lmer.edu , data=ameliaout4)
lmer.edu[[5]] <- lmer(model.lmer.edu , data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- fixef(lmer.edu[[i]])
  vc <- vcov(lmer.edu[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc)
  simbetas <- rbind(simbetas, simbeta)
}
lmer.edu.bhat <- apply(simbetas,2,mean)
lmer.edu.se <- apply(simbetas,2,sd)

# plotting: ropeladder
xscen.edu1fd <- xscen.edu2fd <- cfMake(model.edu, ameliaout1, nscen=2)

xscen.edu1fd <- cfName(xscen.edu1fd, "No A-Levels", scen=1)
xscen.edu1fd <- cfChange(xscen.edu1fd, "participtot", xpre=0, x=1, scen=1)
xscen.edu1fd <- cfChange(xscen.edu1fd, "participlog", xpre=0, x=log(1), scen=1)
xscen.edu1fd <- cfChange(xscen.edu1fd, "alevels", xpre=0, x=0, scen=1)

xscen.edu1fd <- cfName(xscen.edu1fd, "A-Levels", scen=2)
xscen.edu1fd <- cfChange(xscen.edu1fd, "participtot", xpre=0, x=1, scen=2)
xscen.edu1fd <- cfChange(xscen.edu1fd, "participlog", xpre=0, x=log(1), scen=2)
xscen.edu1fd <- cfChange(xscen.edu1fd, "alevels", xpre=1, x=1, scen=2)

xscen.edu2fd <- cfName(xscen.edu2fd, "No A-Levels", scen=1)
xscen.edu2fd <- cfChange(xscen.edu2fd, "participtot", xpre=5, x=6, scen=1)
xscen.edu2fd <- cfChange(xscen.edu2fd, "participlog", xpre=log(5), x=log(6), scen=1)
xscen.edu2fd <- cfChange(xscen.edu2fd, "alevels", xpre=0, x=0, scen=1)

xscen.edu2fd <- cfName(xscen.edu2fd, "A-Levels", scen=2)
xscen.edu2fd <- cfChange(xscen.edu2fd, "participtot", xpre=5, x=6, scen=2)
xscen.edu2fd <- cfChange(xscen.edu2fd, "participlog", xpre=log(5), x=log(6), scen=2)
xscen.edu2fd <- cfChange(xscen.edu2fd, "alevels", xpre=1, x=1, scen=2)

fdhyp.edu1 <- linearsimfd(xscen.edu1fd, simbetas)
fdhyp.edu2 <- linearsimfd(xscen.edu2fd, simbetas)

trace.edufd1 <- ropeladder(
  x = fdhyp.edu1$pe,
  lower = fdhyp.edu1$lower,
  upper = fdhyp.edu1$upper,
  labels=row.names(xscen.edu1fd$x),
  #                sublabels="Raise Participation from 0 to 1",
  #                cex=.8,
  #                shadowbox=T,
  col = "blue",
  plot = 1
)
trace.edufd2 <- ropeladder(
  x = fdhyp.edu2$pe,
  lower = fdhyp.edu2$lower,
  upper = fdhyp.edu2$upper,
  #              sublabels="Raise Participation from 5 to 6",
  #              cex=.8,
  #              shadowbox=T,
  col = "green",
  #         labels=c("No A-Levels", "A Levels"),
  plot = 1
)
trace.edufd1$sublabelsX <- .8
trace.edufd2$sublabelsX <- .8

trace.edufd2$entryheight <- .25

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      plot = 1)

legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(.4,.11),
                   y=c(.92,.90),
                   #                   cex=.7,
                   col=c("blue","dark green"),
                   lineheight=.9,
                   clip=F,
                   plot=1)

## Plotting education only (not in paper)
tc <- tile( trace.edufd1, trace.edufd2,
            legend,
            vertmark,
            limits=c(0,.65,0,1),
            xaxistitle = list(labels="Change in Predicted Attitude"), 
            xaxis = list(labels=c(0,"",.2,"",.4,"",.6)),
            yaxistitle=list(labels=""),
            topaxis = list(add=T, at=c(0,.2,.4,.6), labels=round(sd(attitude.all)*c(0,.2,.4,.6),digits=2), 
                           tick.length=0 ),
            topaxistitle = list(labels="S.D. unit change in Predicted Attitude"),
            gridlines = list(type="xy"),
            frame=F,
            #          titleboxes=T,
            width = list(leftborder=0),
            output=list(width=5,outfile="edu-rl", type="pdf")
)


#####################################################################################################################################
# religious activity

model.relprac <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* relprac + participlog*relprac + age + I(age^2) + female + bme + alevels + relprac + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.relprac <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* relprac + participlog*relprac + age + I(age^2) + female + bme + alevels + relprac + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1|gor)

lmer.relprac <- vector("list",5)
lmer.relprac[[1]] <- lmer(model.lmer.relprac , data=ameliaout1)
lmer.relprac[[2]] <- lmer(model.lmer.relprac , data=ameliaout2)
lmer.relprac[[3]] <- lmer(model.lmer.relprac , data=ameliaout3)
lmer.relprac[[4]] <- lmer(model.lmer.relprac , data=ameliaout4)
lmer.relprac[[5]] <- lmer(model.lmer.relprac , data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- fixef(lmer.relprac[[i]])
  vc <- vcov(lmer.relprac[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc)
  simbetas <- rbind(simbetas, simbeta)
}
lmer.relprac.bhat <- apply(simbetas,2,mean)
lmer.relprac.se <- apply(simbetas,2,sd)

#plotting: ropeladder 
xscen.relprac1fd <- xscen.relprac2fd <- cfMake(model.relprac, ameliaout1, nscen=2)

xscen.relprac1fd <- cfName(xscen.relprac1fd, "Not Active", scen=1)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "participtot", xpre=0, x=1, scen=1)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "participlog", xpre=0, x=log(1), scen=1)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "relprac", xpre=0, x=0, scen=1)

xscen.relprac1fd <- cfName(xscen.relprac1fd, "Practicing", scen=2)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "participtot", xpre=0, x=1, scen=2)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "participlog", xpre=0, x=log(1), scen=2)
xscen.relprac1fd <- cfChange(xscen.relprac1fd, "relprac", xpre=1, x=1, scen=2)

xscen.relprac2fd <- cfName(xscen.relprac2fd, "Not Active", scen=1)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "participtot", xpre=5, x=6, scen=1)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "participlog", xpre=log(5), x=log(6), scen=1)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "relprac", xpre=0, x=0, scen=1)

xscen.relprac2fd <- cfName(xscen.relprac2fd, "Practicing", scen=2)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "participtot", xpre=5, x=6, scen=2)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "participlog", xpre=log(5), x=log(6), scen=2)
xscen.relprac2fd <- cfChange(xscen.relprac2fd, "relprac", xpre=1, x=1, scen=2)

fdhyp.relprac1 <- linearsimfd(xscen.relprac1fd, simbetas)
fdhyp.relprac2 <- linearsimfd(xscen.relprac2fd, simbetas)

trace.relpracfd1 <- ropeladder(
  x = fdhyp.relprac1$pe,
  lower = fdhyp.relprac1$lower,
  upper = fdhyp.relprac1$upper,
  labels=row.names(xscen.relprac1fd$x),#c("Not Active", " Practicing"),# 
  #                sublabels=c("Raise participation from 0 to 1"),
  #cex=.8,
  #                shadowbox=T,
  col = "blue",
  plot = 1
)
trace.relpracfd2 <- ropeladder(
  x = fdhyp.relprac2$pe,
  lower = fdhyp.relprac2$lower,
  upper = fdhyp.relprac2$upper,
  #                sublabels=c("Raise participation from 5 to 6"),
  #cex=.8,
  #              shadowbox=T,
  col = "green",
  plot = 1
)

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      plot = 1)

trace.relpracfd1$entryheight <- 0.25

legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(.35,.1),
                   y=c(.92,.88),
                   #       cex=.7,
                   col=c("blue","dark green"),
                   lineheight=.9,
                   clip=F,
                   plot=1)

#### Religious practice only plot######
tc <- tile( trace.relpracfd1, trace.relpracfd2,
            legend,
            vertmark,
            limits=c(0,.65,0,1),
            xaxistitle = list(labels="Change in Predicted Attitude"), 
            xaxis = list(labels=c(0,"",.2,"",.4,"",.6)),
            yaxistitle=list(labels=""),
            topaxis = list(add=T, at=c(0,.2,.4,.6), labels=round(sd(attitude.all)*c(0,.2,.4,.6),digits=2), 
                           tick.length=0 ),
            topaxistitle = list(labels="S.D. unit change in Predicted Attitude"),
            gridlines = list(type="xy"),
            frame=F,
            width = list(leftborder=0),
            output=list(width=5, outfile="relprac-rl", type="pdf")
)


###########################################################################################################################################
# Race

model.bme <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* bme + participlog*bme + age + I(age^2) + female + bme + alevels + bme + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep  + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.bme <- attitude ~  I(as.numeric(participtot>0)) + participlog + I(as.numeric(participtot>0))* bme + participlog*bme + age + I(age^2) + female + bme + alevels + bme + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1|gor)

lmer.bme <- vector("list",5)
lmer.bme[[1]] <- lmer(model.lmer.bme , data=ameliaout1)
lmer.bme[[2]] <- lmer(model.lmer.bme , data=ameliaout2)
lmer.bme[[3]] <- lmer(model.lmer.bme , data=ameliaout3)
lmer.bme[[4]] <- lmer(model.lmer.bme , data=ameliaout4)
lmer.bme[[5]] <- lmer(model.lmer.bme , data=ameliaout5)

sims <- 10000
simbetas <- NULL
simbeta <- NULL
pe <- NULL
vc <- NULL
for (i in 1:5) {
  pe <- fixef(lmer.bme[[i]])
  vc <- vcov(lmer.bme[[i]])
  simbeta <- mvrnorm(sims/5, pe, vc)
  simbetas <- rbind(simbetas, simbeta)
}
lmer.bme.bhat <- apply(simbetas,2,mean)
lmer.bme.se <- apply(simbetas,2,sd)

#plotting: building ropeladder
xscen.bme1fd <- xscen.bme2fd <- cfMake(model.bme, ameliaout1, nscen=2)

xscen.bme1fd <- cfName(xscen.bme1fd, "White", scen=1)
xscen.bme1fd <- cfChange(xscen.bme1fd, "participtot", xpre=0, x=1, scen=1)
xscen.bme1fd <- cfChange(xscen.bme1fd, "participlog", xpre=0, x=log(1), scen=1)
xscen.bme1fd <- cfChange(xscen.bme1fd, "bme", xpre=0, x=0, scen=1)

xscen.bme1fd <- cfName(xscen.bme1fd, "Non-white", scen=2)
xscen.bme1fd <- cfChange(xscen.bme1fd, "participtot", xpre=0, x=1, scen=2)
xscen.bme1fd <- cfChange(xscen.bme1fd, "participlog", xpre=0, x=log(1), scen=2)
xscen.bme1fd <- cfChange(xscen.bme1fd, "bme", xpre=1, x=1, scen=2)

xscen.bme2fd <- cfName(xscen.bme2fd, "White", scen=1)
xscen.bme2fd <- cfChange(xscen.bme2fd, "participtot", xpre=5, x=6, scen=1)
xscen.bme2fd <- cfChange(xscen.bme2fd, "participlog", xpre=log(5), x=log(6), scen=1)
xscen.bme2fd <- cfChange(xscen.bme2fd, "bme", xpre=0, x=0, scen=1)

xscen.bme2fd <- cfName(xscen.bme2fd, "Non-white", scen=2)
xscen.bme2fd <- cfChange(xscen.bme2fd, "participtot", xpre=5, x=6, scen=2)
xscen.bme2fd <- cfChange(xscen.bme2fd, "participlog", xpre=log(5), x=log(6), scen=2)
xscen.bme2fd <- cfChange(xscen.bme2fd, "bme", xpre=1, x=1, scen=2)

fdhyp.bme1 <- linearsimfd(xscen.bme1fd, simbetas)
fdhyp.bme2 <- linearsimfd(xscen.bme2fd, simbetas)

trace.bmefd1 <- ropeladder(
  x = fdhyp.bme1$pe,
  lower = fdhyp.bme1$lower,
  upper = fdhyp.bme1$upper,
  labels= row.names(xscen.bme1fd$x),
  #sublabels=c("Raise participation from 0 to 1"),
  #cex=.8,
  #shadowbox=T,
  col = "blue",
  plot = 1
)
trace.bmefd2 <- ropeladder(
  x = fdhyp.bme2$pe,
  lower = fdhyp.bme2$lower,
  upper = fdhyp.bme2$upper,
  #sublabels=c("Raise participation from 5 to 6"),
  #cex=.8,
  #shadowbox=T,
  col = "green",
  #labels=c("No A-Levels", "A Levels"),
  plot = 1
)

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      plot = 1)


trace.bmefd1$entryheight <- 0.25

legend <- textTile(labels=c("0 to 1","5 to 6"),
                   x=c(.337,.102),
                   y=c(.92,.88),
                   #cex=.7,
                   col=c("blue","dark green"),
                   lineheight=.9,
                   clip=F,
                   plot=1)

#### BME only plot ######
tc <- tile( trace.bmefd1, trace.bmefd2,
            legend,
            vertmark,
            limits=c(-.05,.85,0,1),
            xaxistitle = list(labels="Change in Predicted Attitude"), 
            xaxis = list(at=seq(0,.8,.1), labels=c(0,"",.2,"",.4,"",.6,"",.8)),
            yaxistitle=list(labels=""),
            topaxis = list(add=T, at=c(0,.2,.4,.6,.8), labels=round(sd(attitude.all)*c(0,.2,.4,.6,.8),digits=2), 
                           tick.length=0 ),
            topaxistitle = list(labels="S.D. unit change in Predicted Attitude"),
            gridlines = list(type="xy"),
            frame=F,
            width = list(leftborder=0),
            output=list(width=5, outfile="bme-rl", type="pdf")
)



######### Combining all in one tiled plot: Figure 8
trace.econfd1$plot <- 1
trace.econfd2$plot <- 1
trace.edufd1$plot <- 2
trace.edufd2$plot <- 2
trace.relpracfd1$plot <- 3
trace.relpracfd2$plot <- 3
trace.bmefd1$plot <- 4
trace.bmefd2$plot <- 4
trace.econfd1$entryheight <- .1

vertmark <- linesTile(x =c(0,0),
                      y = c(0,1),
                      lty= "solid",
                      lwd=.5,
                      plot = c(1:4))

legend.bme <- textTile(labels=c("0 to 1","5 to 6"),
                       x=c(.337,.102),
                       y=c(.92,.88),
                       #cex=.7,
                       col=c("blue","dark green"),
                       lineheight=.9,
                       clip=F,
                       plot=4)


legend.econ <- textTile(labels=c("0 to 1","5 to 6"),
                        x=c(.35,.075),
                        y=c(.955,.935),
                        #                      cex=.7,
                        col=c("blue","dark green"),
                        lineheight=.9,
                        clip=F,
                        plot=1)


legend.edu <- textTile(labels=c("0 to 1","5 to 6"),
                       x=c(.4,.11),
                       y=c(.92,.90),
                       #                     cex=.7,
                       col=c("blue","dark green"),
                       lineheight=.9,
                       clip=F,
                       plot=2)


legend.relprac <- textTile(labels=c("0 to 1","5 to 6"),
                           x=c(.35,.1),
                           y=c(.92,.88),
                           #       cex=.7,
                           col=c("blue","dark green"),
                           lineheight=.9,
                           clip=F,
                           plot=3)

tc <- tile( trace.econfd1,trace.econfd2,trace.edufd1,trace.edufd2,trace.relpracfd1,trace.relpracfd2,trace.bmefd1, trace.bmefd2,
            legend.econ,legend.edu,legend.relprac,legend.bme,
            vertmark,
            RxC = c(2,2),
            limits=c(-.05,.75,0,1),
            xaxistitle = list(labels="Change in Predicted Attitude"), 
            xaxis = list(at=seq(0,.7,.1), labels=c(0,"",.2,"",.4,"",.6,"")),
            yaxistitle=list(labels=""),
            topaxis = list(add=T, at=c(0,.2,.4,.6), labels=round(sd(attitude.all)*c(0,.2,.4,.6),digits=2), 
                           tick.length=0 ),
            topaxistitle = list(labels="S.D. unit change in Predicted Attitude"),
            plottitle = list(labels=c("Interacted with class","Interacted with education","Interacted with religious practice","Interacted with race")),
            gridlines = list(type="xy"),
            #frame=F,
            width = list(null=3,leftborder=0),
            output=list(outfile="4by", type="pdf")
)





################################################################################################################
########## DESCRIPTIVE STATISTICS #############################################################################
#Participation distribution -- Figure 1
pdf(file="par-dist.pdf",width=7,height=4)
par(mfrow=c(1,2))
hist(ameliaout5$participtot, breaks=27,xlab="Participation Index", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0], breaks=26, xlab="Participation Index,\nonly participators", main="", col="light green")

#Attitudes over participation  -- Figure 2
particip3<-NULL
particip3 <-  recode(ameliaout5$participtot, "0='0';
                     1='1';
                     else='2'")
pdf(file="att-dist.pdf")
par(mfrow=c(1,3), din=c(10,5))
hist(ameliaout5$attitude[particip3==0], xlab="Attitudes of Non-Participators", main="", col="light blue")
hist(ameliaout5$attitude[particip3==1], xlab="Attitudes of Single Participators", main="", col="light blue")
hist(ameliaout5$attitude[particip3==2], xlab="Attitudes of Repeat Participators", main="", col="light blue")
dev.off()

# Attitudes over regions - Figure 10
pdf(file="att-reg.pdf", width=10, height=5)
par(mfrow=c(2,5))
hist(ameliaout5$attitude[ameliaout5$gor==1], xlab="Northeast", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==2], xlab="Northwest", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==3], xlab="Yorkshire", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==4], xlab="East Midlands", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==5], xlab="West Midlands", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==6], xlab="East of England", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==7], xlab="London", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==8], xlab="Southeast", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==9], xlab="Southwest", main="", col="light blue")
hist(ameliaout5$attitude[ameliaout5$gor==10], xlab="Wales", main="", col="light blue")
mtext("Distributions of Attitudes across Regions", side=3, outer=TRUE, line=-3)
dev.off()
# Participation over regions -- Figure 9
pdf(file="par-reg.pdf", width=10, height=5)
par(mfrow=c(2,5))
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor], breaks=26, xlab="Northeast", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==2], breaks=26, xlab="Northwest", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==3], breaks=26, xlab="Yorkshire", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==4], breaks=26, xlab="East Midlands", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==5], breaks=26, xlab="West Midlands", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==6], breaks=26, xlab="East of England", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==7], breaks=26, xlab="London", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==8], breaks=26, xlab="Southeast", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==9], breaks=26, xlab="Southwest", main="", col="light green")
hist(ameliaout5$participtot[ameliaout5$participtot>0 & ameliaout5$gor==10], breaks=26, xlab="Wales", main="", col="light green")
mtext("Distributions of Participators across Regions", side=3, outer=TRUE, line=-3)
dev.off()


