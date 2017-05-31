####################################################################################################################
################################### Carolina Johnson                    ############################################
################################### Civic Participation and Democracy   ############################################
################################### Multiple Imputation w/Amelia        ############################################
################################### 20 Jan 2012                         ############################################
####################################################################################################################

rm(list=ls())

library(foreign)
library(MASS)
library(xtable)
library(car)
library(Amelia)

dataf<- read.dta("citizen0708.dta", convert.factors=FALSE)
attach(dataf)

###REMOVING NAs####
pconoft[is.na(pconoft)]<-0
coften[is.na(coften)]<-0
poften[is.na(poften)]<-0

pconoftlh <- pconoft
pconoftlh <- recode(pconoftlh, "1='3'; 3='1'")
pconoftlh[pofoth1>=12 & pconoft==4]<-2
pconoftlh[pofoth1<12 & pconoft==4]<-1
pconoftlh[is.na(pofoth1) & pconoft==4] <- 1

coftenlh <- coften
coftenlh <- recode(coftenlh, "1='3'; 3='1'")
coftenlh[cofoth>=12 & coften==4]<-2
coftenlh[cofoth<12 & coften==4]<-1
coftenlh[is.na(cofoth) & coften==4] <- 1

pactoflh <- poften
pactoflh <- recode(pactoflh, "1='3'; 3='1'")
pactoflh[pofoth>=12 & poften==4 ]<-2
pactoflh[pofoth<12 & poften==4]<-1
pactoflh[is.na(pofoth) & poften==4] <- 1

alevels <- rep(0,14095)
alevels[zquals<=3]<- 1

locnews<-rep(0,14095)
locnews[mlocnew==1]<-1

natnews<-rep(0,14095)
natnews[mnatnew==1]<-1

rival4a<- rep(0,14095)
rival4a[rival4==1]<-1

rival16a<- rep(0,14095)
rival16a[rival16==1]<-1

rival8a<- rep(0,14095)
rival8a[rival8==1]<-1

civact11[is.na(civact11)]<-0
civact12[is.na(civact12)]<-0
civact13[is.na(civact13)]<-0
civact14[is.na(civact14)]<-0
civact21[is.na(civact21)]<-0
civact21[is.na(civact21)]<-0
civact22[is.na(civact22)]<-0
civact23[is.na(civact23)]<-0
civact24[is.na(civact24)]<-0
civact25[is.na(civact25)]<-0
civact26[is.na(civact26)]<-0
civact27[is.na(civact27)]<-0
pconsul1[is.na(pconsul1)]<-0
pconsul2[is.na(pconsul2)]<-0
pconsul3[is.na(pconsul3)]<-0
pactuk1[is.na(pactuk1)]<-0
pactuk2[is.na(pactuk2)]<-0
pactuk3[is.na(pactuk3)]<-0
pactuk4[is.na(pactuk4)]<-0
pactuk5[is.na(pactuk5)]<-0
pactuk6[is.na(pactuk6)]<-0
pactuk7[is.na(pactuk7)]<-0
pactuk8[is.na(pactuk8)]<-0
prally1[is.na(prally1)]<-0
prally2[is.na(prally2)]<-0
prally3[is.na(prally3)]<-0

#####RELATIVE DEPRIVATION#####
reldep<-NA
reldep[is.na(dimd)]<-wdepd[is.na(dimd)]
reldep[is.na(wdepd)]<-dimd[is.na(wdepd)]

######INCOME in 10,000s - keeping NAs######
incomereal<-income
incomereal<-recode(incomereal, "15='0'; 1='.125'; 2='.375'; 3='.75'; 4='1.25'; 5='1.75'; 6='2.25';  7='2.75'; 8='3.25'; 9='3.75'; 10='4.25'; 11='4.75'; 12='6.25'; 13='8.75'; 14='20'")
incomelog <- recode(incomereal, "0='60'")
incomelog <- log(incomelog)

impinfl<- rep(NA, 14095)
impinfl[pinfl==1]<-3
impinfl[pinfl==2]<-2
impinfl[pinfl==3]<-1
impinfl[pinfl==4]<-0

infloc<- rep(NA, 14095)
infloc[paffloc==1]<-3
infloc[paffloc==2]<-2
infloc[paffloc==3]<-1
infloc[paffloc==4]<-0

infnat<- rep(NA, 14095)
infnat[paffgb==1]<-3
infnat[paffgb==2]<-2
infnat[paffgb==3]<-1
infnat[paffgb==4]<-0



###IVs###

##Regions

NE <- rep(0, 14095)
NE[gor==1] <- 1
NW <- rep(0, 14095)
NW[gor==2] <- 1
YS <- rep(0, 14095)
YS[gor==3] <- 1
EM <- rep(0, 14095)
EM[gor==4] <- 1
WM <- rep(0, 14095)
WM[gor==5] <- 1
EE <- rep(0, 14095)
EE[gor==6] <- 1
LON <- rep(0, 14095)
LON[gor==7] <- 1
SE <- rep(0, 14095)
SE[gor==8] <- 1
SW <- rep(0, 14095)
SW[gor==9] <- 1
WA <- rep(0, 14095)
WA[gor==10] <- 1

#regions<-cbind(NE, NW, YS, EM, WM, EE, LON, SE, SW, WA)

#CLASS#
econstat <- rnssec4
econstat <- recode(econstat, "4='0'; 3='1'; 1='3'")
table(econstat, rnssec4)

#EDUCATION#
#alevels <- rep(0,14095)
#alevels[zquals<=3]<- 1

#AGE#
age <- dvage

#GENDER#
female <- rep(0,14095)
female[sex==2]<-1


#HOMEOWNER#
homeowner<-hhldr
homeowner<-recode(homeowner, "1='1'; 2='1'; 3='0'")

#RESIDENT LENGTH#
resident<-slive7
resident<-recode(resident,  "1='0'; 2='1'; 3='4'; 4='8'; 5='15'; 6='25'; 7='50'")

#belonging to local area#
beloc<-sbeloc
beloc<-recode(beloc, "3='1'; 2='2'; 1='3'; 4='0'")


###satisfaction with local government###
satisf<-locsat
satisf<-recode(satisf, "5='0'; 4='1'; 3='2'; 2='3'; 1='4'")

sat3 <- satisf
sat3 <- recode(sat3, "0='0'; 1='0'; 2='1'; 3='2'; 4='2'")

####local trust & cooperation###
trust<-strust
trust<-recode(trust, "4='0'; 3='1'; 2='2'; 1='3'")

cooperate<-rep(0, 14095)
cooperate[spull==1 | spull==2]<-1
cooperate<-recode(cooperate, "1='1'; 2='1'; 3='0'; 4='0'")

#parliament trust
parltrust <- ptparl
parltrust <- recode(parltrust, "4='0'; 3='1'; 2='2'; 1='3'")


ameliaset<-data.frame(pconoft, coften, poften, pconoftlh, coftenlh, pactoflh, alevels, locnews,
                      natnews, rival4a, rival16a, rival8a, civact11, civact12, civact13, civact14, civact21, civact22,
                      civact23, civact24, civact25, civact26, civact27, pconsul1, pconsul2, pconsul3, pactuk1,
                      pactuk2, pactuk3, pactuk4, pactuk5, pactuk6, pactuk7, pactuk8, prally1, prally2, prally3,
                      impinfl, infloc, infnat, incomereal, 
                      reldep, marstat, numadult, numchild, sbeneigh,
                      sbegb, senjoy, ssafe, svalue, rilo4a, zquals1, xfriends, relprac, xmoimpt,
                      dmhsize, rnssec17, zcivact1, age, female, ethnic6, econstat, trust, parltrust, satisf, beloc, cooperate, 
                      homeowner, resident, gor 
                      #, NE, NW, YS, EM, WM, EE, LON, SE, SW, WA
)

colnames(ameliaset)<- c("pconoft", "coften", "poften", "pconoftlh", "coftenlh", "pactoflh", "alevels", "locnews",
                        "natnews", "rival4a", "rival16a", "rival8a", "civact11", "civact12", "civact13", "civact14", "civact21", "civact22",
                        "civact23", "civact24", "civact25", "civact26", "civact27", "pconsul1", "pconsul2", "pconsul3", "pactuk1",
                        "pactuk2", "pactuk3", "pactuk4", "pactuk5", "pactuk6", "pactuk7", "pactuk8", "prally1", "prally2", "prally3",
                        "impinfl", "infloc", "infnat", "incomereal", 
                        "reldep", "marstat", "numadult", "numchild", "sbeneigh",
                        "sbegb", "senjoy", "ssafe", "svalue", "rilo4a", "zquals1", "xfriends", "relprac", "xmoimpt",
                        "dmhsize", "rnssec17", "zcivact1", "age", "female", "ethnic6", "econstat", "trust", "parltrust", "satisf", "beloc", "cooperate",
                        "homeowner", "resident", "gor"
                        #, "NE", "NW", "YS", "EM", "WM", "EE", "LON", "SE", "SW", "WA"
)

#### a.out, without transformations, is the final decision based on post-imputation diagnostics ###
# ************************************************************************************************#
a.out<-amelia(ameliaset, m=5, noms=c("ethnic6", "gor") 
              , ords=c("impinfl", "infloc", "infnat", "parltrust"
                       #    ,"pconoft", "coften", "poften", "pconoftlh", "coftenlh", "pactoflh",  "sbeneigh","sbegb", "senjoy", 
                       #      "ssafe", "svalue", "rilo4a", "zquals1", "xfriends", "relprac", "xmoimpt",
                       #     "dmhsize", "rnssec17", "zcivact1", "econstat", "trust", "parltrust",
                       #    "satisf", "beloc", "cooperate"
              ) 
)
######################################################################################################

a.out.log<-amelia(ameliaset, m=5, noms=c("ethnic6", "gor") 
                  , ords=c("impinfl", "infloc", "infnat","parltrust"
                           #    ,"pconoft", "coften", "poften", "pconoftlh", "coftenlh", "pactoflh",  "sbeneigh","sbegb", "senjoy", 
                           #      "ssafe", "svalue", "rilo4a", "zquals1", "xfriends", "relprac", "xmoimpt",
                           #     "dmhsize", "rnssec17", "zcivact1", "econstat", "trust", "parltrust",
                           #    "satisf", "beloc", "cooperate"
                  ) 
                  , logs="incomereal")

bds <- matrix(c(41, 0, 100), nrow = 1, ncol = 3)

a.out.bound <- amelia(ameliaset, m=5, noms=c("ethnic6", "gor") 
                      , ords=c("impinfl", "infloc", "infnat", "parltrust"
                               #    ,"pconoft", "coften", "poften", "pconoftlh", "coftenlh", "pactoflh",  "sbeneigh","sbegb", "senjoy", 
                               #      "ssafe", "svalue", "rilo4a", "zquals1", "xfriends", "relprac", "xmoimpt",
                               #     "dmhsize", "rnssec17", "zcivact1", "econstat", "trust", "parltrust",
                               #    "satisf", "beloc", "cooperate"
                      ) 
                      , bounds=bds
)

###########~~~~~~~~~~~~~~~~~~~~~ Diagnostics ~~~~~~~~~~~~~~~~~~~~~~~~~~####################

overimpute(a.out, var="incomereal")
overimpute(a.out.log, var="incomereal")
overimpute(a.out.bound, var="incomereal")
plot(a.out, which.vars="incomereal")
plot(a.out.log, which.vars="incomereal")
plot(a.out.bound, which.vars="incomereal")
compare.density(a.out.bound, var="incomereal")
compare.density(a.out,var="incomereal")

###########~~~~~~~~~~ Further Variable Construction ~~~~~~~~~~~~~~~~~~~~####################


for (i in 1:a.out$m){
  civmember <- rep(0,14095)
  civmemberM <- a.out$imputations[[i]]$civact21 + a.out$imputations[[i]]$civact22 + a.out$imputations[[i]]$civact23 + a.out$imputations[[i]]$civact24 + a.out$imputations[[i]]$civact25 + a.out$imputations[[i]]$civact26 + a.out$imputations[[i]]$civact27
  civmember[civmemberM>0 & civmemberM<8] <- civmemberM[civmemberM>0 & civmemberM<8]
  pcontot <- rep(0,14095)
  pcontotM <- a.out$imputations[[i]]$pconsul1 + a.out$imputations[[i]]$pconsul2 + a.out$imputations[[i]]$pconsul3
  pcontot[pcontotM>0 & pcontotM<4] <- pcontotM[pcontotM>0 & pcontotM<4]
  pconindex <- pcontot*a.out$imputations[[i]]$pconoftlh
  civtot <- a.out$imputations[[i]]$zcivact1 + civmember
  civindex<-civtot*a.out$imputations[[i]]$coftenlh
  particip <- civindex+pconindex
  
  pactukindex <- rep(0,14095)
  pactuktot=a.out$imputations[[i]]$pactuk1 +a.out$imputations[[i]]$pactuk2 +a.out$imputations[[i]]$pactuk3 +a.out$imputations[[i]]$pactuk4 +a.out$imputations[[i]]$pactuk5 +a.out$imputations[[i]]$pactuk6 +a.out$imputations[[i]]$pactuk7 +a.out$imputations[[i]]$pactuk8
  pactukindex[pactuktot>0 & pactuktot<9] <- pactuktot[pactuktot>0 & pactuktot<9]
  pactukindex <-pactukindex/2
  
  prallyindex <- rep(0,14095)
  prallyindex <- a.out$imputations[[i]]$prally1 + a.out$imputations[[i]]$prally2 + a.out$imputations[[i]]$prally3
  prallyindex <- prallyindex/2
  activetot <- prallyindex + pactukindex
  pactindex <- activetot*a.out$imputations[[i]]$pactoflh
  
  a.out$imputations[[i]]$participtot <- NULL
  #  a.out$imputations[[i]]$participtot <- pactindex + civindex+ pconindex
  a.out$imputations[[i]]$participtot <- civindex + pconindex  
  a.out$imputations[[i]]$particip10 <- a.out$imputations[[i]]$participtot
  a.out$imputations[[i]]$particip10 <- recode(a.out$imputations[[i]]$particip10, "0='0'; .5='1'; 1='2'; 1.5='3'; 2='4'; 2.5='5'; 3='6'; 3.5='7'; 4='8'; else='9'")
  
  #Log participation workaround   
  a.out$imputations[[i]]$participlog <- rep(0,14095)
  a.out$imputations[[i]]$participlog[a.out$imputations[[i]]$participtot>0] <- log(a.out$imputations[[i]]$participtot[a.out$imputations[[i]]$participtot>0])
  
  #ATTITUDE#
  values<-a.out$imputations[[i]]$rival4a+a.out$imputations[[i]]$rival8a+a.out$imputations[[i]]$rival16a
  a.out$imputations[[i]]$attitude<- values+a.out$imputations[[i]]$impinfl + a.out$imputations[[i]]$infloc +a.out$imputations[[i]]$infnat
  
  #sat3
  a.out$imputations[[i]]$sat3 <- a.out$imputations[[i]]$satisf
  a.out$imputations[[i]]$sat3[a.out$imputations[[i]]$satisf<=1.8] <- 0
  a.out$imputations[[i]]$sat3[a.out$imputations[[i]]$satisf>1.8] <- 1
  a.out$imputations[[i]]$sat3[a.out$imputations[[i]]$satisf>2.2] <- 2
  
  a.out$imputations[[i]]$NE <- rep(0, 14095)
  a.out$imputations[[i]]$NE[gor==1] <- 1
  a.out$imputations[[i]]$NW <- rep(0, 14095)
  a.out$imputations[[i]]$NW[gor==2] <- 1
  a.out$imputations[[i]]$YS <- rep(0, 14095)
  a.out$imputations[[i]]$YS[gor==3] <- 1
  a.out$imputations[[i]]$EM <- rep(0, 14095)
  a.out$imputations[[i]]$EM[gor==4] <- 1
  a.out$imputations[[i]]$WM <- rep(0, 14095)
  a.out$imputations[[i]]$WM[gor==5] <- 1
  a.out$imputations[[i]]$EE <- rep(0, 14095)
  a.out$imputations[[i]]$EE[gor==6] <- 1
  a.out$imputations[[i]]$LON <- rep(0, 14095)
  a.out$imputations[[i]]$LON[gor==7] <- 1
  a.out$imputations[[i]]$SE <- rep(0, 14095)
  a.out$imputations[[i]]$SE[gor==8] <- 1
  a.out$imputations[[i]]$SW <- rep(0, 14095)
  a.out$imputations[[i]]$SW[gor==9] <- 1
  a.out$imputations[[i]]$WA <- rep(0, 14095)
  a.out$imputations[[i]]$WA[gor==10] <- 1
  
}

############################################################################################
###########~~~~~~~~~~ Writing to CSV for future use ~~~~~~~~~~~~~~~~~~~~####################
############################################################################################

write.amelia(obj=a.out, file.stem="ameliaout")
