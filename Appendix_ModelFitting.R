################################################################################################################################
################################### Carolina Johnson                                        ####################################
################################### Civic Participation and Legitimacy: Model fitting       ####################################################
################################### 20 Jan 2012                                             ####################################################
###################################                                                         ####################################################
################################### 1) nooprobit                                            ####################################################
################################### 2) Linear/Logged/Binary                                 ####################################################
###################################    -AIC/BIC                                             ####################################################
###################################    -AvP plot                                            ####################################################
################################################################################################################################

## 1) demonstrating lack of need for oprobit: nooprobit

clm.full <- clm(as.factor(attitude) ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc+ reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA, data=ameliaout1, link="probit", Hess=TRUE,  threshold="flexible")
pdf("no-oprobit.pdf")
plot(x=c(1:11), y=clm.full$alpha, xlab="", ylab="", xaxt="n", points(clm.full$alpha, pch=19,cex=2,col="blue"))
axis(1,at=c(1:11), labels=c( "0|1","1|2","2|3","3|4","4|5","5|6","6|7","7|8","8|9","9|10","10|11"))
title(xlab="Cutpoints", ylab="Coefficients", cex.lab=1.5)
dev.off()

## 2) Model fitting: Linear, Logged, or Binary? Model fitting on a single imputation

source("C:/Users/Carolina/Documents/UW PhD/Year3/POLS 510/avp.r")

model.base <- attitude ~ participtot + age + I(age^2) + female + bme +alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.base <- attitude ~ participtot + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1|gor)
lmer.base <- lmer(model.lmer.base , data=ameliaout5)

model.log <- attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.log <- attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA + (1 | gor)
lmer.log <- lmer(model.lmer.log, data=ameliaout5)

particip2 <- ameliaout5$participtot
particip2[ameliaout5$participtot>0] <- 1
model.2 <- attitude ~ particip2 + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA
model.lmer.2 <- attitude ~ particip2 + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep + NE + NW + YS + EM + WM + EE + SE + SW + WA +(1|gor)
lmer.2 <- lmer(model.lmer.2, data=ameliaout5)

compare <- rbind(slot(summary(lmer.base), "AICtab"),slot(summary(lmer.log), "AICtab"),slot(summary(lmer.2), "AICtab"))
print(compare)

## actual v. predicted plot, all three models
y<-ameliaout5$attitude
x1 <- cbind(ameliaout5$participtot, ameliaout5$age, I(ameliaout5$age^2), ameliaout5$female, ameliaout5$bme, ameliaout5$alevels, ameliaout5$incomereal, ameliaout5$econstat, ameliaout5$homeowner, ameliaout5$resident, ameliaout5$natnews, ameliaout5$locnews, ameliaout5$satisf, ameliaout5$cooperate, ameliaout5$beloc, ameliaout5$reldep, ameliaout5$NE, ameliaout5$NW, ameliaout5$YS, ameliaout5$EM, ameliaout5$WM, ameliaout5$EE, ameliaout5$SE, ameliaout5$SW, ameliaout5$WA)
avp(y,
    x=cbind(rep(1,nrow(x1)),x1),
    beta=fixef(lmer.base),
    fnform="linear",
    cutpoints=seq(0,30,1),
    usr=c(-0.1,15,-0.1,15),
    sizefactor=.4,
    color = "blue",
    output = list(outfile="avp.pdf", type="pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = 3, y=14, str="Linear", col="blue", cex=1),
    ylab = "Actual attitudes, by bins of predicted",
    xlab = "Predicted attitudes, binned",
    closeplot=F)  ## don't close the plot here because we will add addtional points

x2 <- cbind(I(as.numeric(ameliaout5$participtot>0)), ameliaout5$participlog, ameliaout5$age, I(ameliaout5$age^2), ameliaout5$female, ameliaout5$bme, ameliaout5$alevels, ameliaout5$incomereal, ameliaout5$econstat, ameliaout5$homeowner, ameliaout5$resident, ameliaout5$natnews, ameliaout5$locnews, ameliaout5$satisf, ameliaout5$cooperate, ameliaout5$beloc, ameliaout5$reldep, ameliaout5$NE, ameliaout5$NW, ameliaout5$YS, ameliaout5$EM, ameliaout5$WM, ameliaout5$EE, ameliaout5$SE, ameliaout5$SW, ameliaout5$WA)
avp(y,
    x=cbind(rep(1,nrow(x2)),x2),
    beta=fixef(lmer.log),
    fnform="linear",
    cutpoints=seq(0,30,1),
    usr=c(-0.1,15,-0.1,15),
    sizefactor=.4,
    color = "green",
    output = list(outfile="avp.pdf", type="pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = 3, y=13, str="Logged", col="green", cex=1),
    #ylab = "Actual attitudes, by bins of predicted",
    #xlab = "Predicted attitudes, binned",
    addtoplot=T,
    closeplot=F)  ## don't close the plot here because we will add addtional points


x3 <- cbind(particip2, ameliaout5$age, I(ameliaout5$age^2), ameliaout5$female, ameliaout5$bme, ameliaout5$alevels, ameliaout5$incomereal, ameliaout5$econstat, ameliaout5$homeowner, ameliaout5$resident, ameliaout5$natnews, ameliaout5$locnews, ameliaout5$satisf, ameliaout5$cooperate, ameliaout5$beloc, ameliaout5$reldep, ameliaout5$NE, ameliaout5$NW, ameliaout5$YS, ameliaout5$EM, ameliaout5$WM, ameliaout5$EE, ameliaout5$SE, ameliaout5$SW, ameliaout5$WA)
avp(y,
    x=cbind(rep(1,nrow(x3)),x3),
    beta=fixef(lmer.2),
    fnform="linear",
    cutpoints=seq(0,30,1),
    usr=c(-0.1,15,-0.1,15),
    sizefactor=.4,
    color = "red",
    output = list(outfile="avp.pdf", type="pdf",high=6,wide=5.5,epsi=FALSE),
    lab = list(x = 3, y=12, str="Binary", col="red", cex=1),
    #ylab = "Actual Post-invasion deaths, by bins of predicted",
    #xlab = "Predicted Post-invasion deaths, binned",
    addtoplot=T,
    closeplot=T)  ## don't close the plot here because we will add addtional points


model.lmer <- attitude ~ I(as.numeric(participtot>0)) + participlog + age + I(age^2) + female + bme + alevels + incomereal +econstat + homeowner + resident + natnews + locnews + satisf + cooperate + beloc + reldep  + (1 | gor)
lmer <- lmer(model.lmer, data=ameliaout5)
slot(summary(lmer),"AICtab")
compare[2,]

slot(summary(lmer.x),"AICtab") #lmer.x from paperscratch.R

#lmer.x - eg random effects both slop and intercept for gor, no FE is actually the best fitting by AIC/BIC measure.