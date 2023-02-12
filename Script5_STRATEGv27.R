#libraries
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)
library(xtable)
library(pastecs)
library(tidyr)

#library(quantmod)
library(PerformanceAnalytics)
library(SharpeR)

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/FasePortOpt/')
source('CryptFunctPortf.R')

#########################################################################
##        Dataset 2018-21: REMOVING VOL = 0 obs (652 investing days)   ##
##        DS_Portfopt_v11.R                                            ##
#########################################################################

# nfich1 <- c("Datasets/FeatBUILDminTC4.RData")
# load(nfich1)
# 
# # Risl label
# nfich2 <- c("Datasets/TrainingRisk2.RData") #RiskQ.tab
# load(nfich2)
###########################################################

# Dataset 2018-21 (851 investing days)
nfich1 <- c("Datasets/FeatBUILDminTC3.RData")
load(nfich1)

# Risl label
nfich2 <- c("Datasets/TrainingRisk.RData") #RiskQ.tab
load(nfich2)

# For transaction costs in basis points: typically 5 bps ($0.0005) per crypto
# https://www.blueskycapitalmanagement.com/how-to-trade-hedge-crypto-and-related-transaction-cost-analysis-tca/
BaseInv <- 1.0
#TC <- 0.0005  #5 bps

################## Data Cleaning #########################
## We remove observations with returns higher than 10   ##
##########################################################
#barplot(Feat.tab$retGP.clust)
#summary(Feat.tab$retGP.clust)

#Feat.tab.filtered <- Feat.tab[Feat.tab$retGP.clust>=-1.5 & Feat.tab$retGP.clust<=1.5,]
Feat.tab.filtered <- Feat.tab[,c('dateTestStart','dateTestEnd','dateTrainStart','dateTrainEnd',
                                 'WBaseInv','WBaseInv.clust',
                                 'accumGainTest','accumGainTest.clust',
                                 'retGP','cardGP','retGP.clust','cardGP.clust','benchTest',
                                 'SRTrain','SRTrain.clust',
                                 'SRTest','SRTest.clust',
                                 'RtMeanTrain.clust','RtVolTrain.clust',
                                 'clust','card.clust')]
Feat.tab.filtered <- merge(Feat.tab.filtered, RiskQ.tab, all.x = TRUE, by.x = 'dateTrainEnd', by.y = 'dateTrainEnd')
# Feat.tab.filtered <- Feat.tab[,c('dateTestStart','dateTestEnd','dateTrainStart','dateTrainEnd',
#                                  'WBaseInv','WBaseInv.clust',
#                                  'accumGainTest','accumGainTest.clust',
#                                  'retGP','cardGP','retGP.clust','cardGP.clust','benchTest',
#                                  'SRTrain','SRTrain.clust',
#                                  'SRTest','SRTest.clust',
#                                  'RtMeanTrain.clust','RtVolTrain.clust',
#                                  'clust')]

Feat.tab.filtered <- data.table(Feat.tab.filtered)
Feat.tab.filtered$Num <- as.numeric(rownames(Feat.tab.filtered))
Feat.tab.filtered <- Feat.tab.filtered[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                WBaseInv, WBaseInv.clust,
                                accumGainTest, accumGainTest.clust, 
                                retGP, retGP.clust, retGP.bench = (benchTest-BaseInv)/BaseInv, 
                                SRTrain, SRTrain.clust, SRTest, SRTest.clust,
                                RtMeanTrain.clust, RtVolTrain.clust, quart1st, quart2nd, quart3rd,
                                Risk = quantFunc2(RtVolTrain.clust, quart1st, quart2nd, quart3rd),
                                cardGP, cardGP.clust,
                                clust,card.clust), by = Num]

##########################################################################################################
# Strategy 1: We choose the cluster with the Best Sharpe Ratio ###########################################
##########################################################################################################

Strateg1.tab <- Feat.tab.filtered

#Strateg1.tab <- data.table(Strateg1.tab)
Strateg1.tab <- Strateg1.tab[Strateg1.tab[,.I[which.max(SRTrain.clust)], by=dateTestStart]$V1]
#Strateg1.tab$retGP.bench <- (Strateg1.tab$benchTest-BaseInv)/BaseInv
Strateg1.tab <- Strateg1.tab[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                WBaseInv, WBaseInv.clust,
                                accumGainTest, accumGainTest.clust, 
                                retGP, retGP.clust, retGP.bench,
                                SRTrain, SRTrain.clust, SRTest, SRTest.clust,
                                cardGP,cardGP.clust,
                                clust, card.clust)]

Strateg1.df <- as.data.frame(Strateg1.tab)
Strateg1.xts <- as.xts(Strateg1.df[,c('retGP','retGP.clust','retGP.bench')],
                       Strateg1.df[,c('dateTestEnd')])

StrategAll.df <- Strateg1.df[,c('dateTestEnd','retGP','retGP.clust','retGP.bench')]
colnames(StrategAll.df) <- c('Date','RetMV','RetMV.SR','RetIdx')

CardAll.df <- Strateg1.df[,c('dateTestEnd','cardGP','cardGP.clust')]
colnames(CardAll.df) <- c('Date','cardMV','cardMV.SR')

colnames(Strateg1.xts) <- c('MV','MV.clust','IdX')


###########################################################################
# We select investing days from 12 to 41 (30 investors) with 28 investing days #
###########################################################################
NewInvestDays <- Strateg1.tab$dateTestStart #Days on which new investors can allocate/re-allocate a portfolio
newinvdays <- length(NewInvestDays) 
if(newinvdays%%30 != 0){
  PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
} else {PortfReallocatSeq <- newinvdays/30}

#PortfReallocatSeq<- round(newinvdays/30)+1
InvestDays.mtx<- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.clust.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.bench.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
for(i in 1:newinvdays){
  dayspos <- seq.int(i, newinvdays, by=30)
  InvestDays.mtx[i,1:(length(dayspos))] <- dayspos
  retGP.mtx[i,1:(length(dayspos))] <- Strateg1.df[dayspos,"retGP"]
  retGP.clust.mtx[i,1:(length(dayspos))] <- Strateg1.df[dayspos,"retGP.clust"]
  retGP.bench.mtx[i,1:(length(dayspos))] <- Strateg1.df[dayspos,"retGP.bench"]
}

#xtable(tail(InvestDays.mtx))

#####################################################################################
#      We filter-in selected investors per month (30 investors, 28 allocation days) #      #
#####################################################################################
Invest <- seq(1,30)

nrmin <- 12
nrmax <- 41 #Maximum number of investors: nrmax - nrmin + 1 = 30
ncmin <- 1
ncmax <- 28 #Max. number of cumulated investing months: 

SelectedStartDays <- NewInvestDays[nrmin:nrmax] #From 2019-012-12 to 2019-02-10

InvestDays.mtx.red <- InvestDays.mtx[nrmin:nrmax,ncmin:ncmax] #We select maximum number of investors
#xtable(InvestDays.mtx.red, digits = 0)
months <- ncol(InvestDays.mtx.red)

retGP.mtx.red <- retGP.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.clust.mtx.red <- retGP.clust.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.bench.mtx.red <- retGP.bench.mtx[nrmin:nrmax,ncmin:ncmax]

retGP.mtx.MV <- retGP.mtx.red
retGP.mtx.SR <- retGP.clust.mtx.red
retGP.mtx.IdX<- retGP.bench.mtx.red

## Cumulative Returns

retCUM.mtx.MV <-apply(retGP.mtx.MV+1,1,cumprod)
retCUM.mtx.MV <- t(retCUM.mtx.MV - 1)
#retCUM.mtx.MV <- cbind.data.frame(retCUM.mtx.MV,Invest)

retCUM.mtx.IdX <-apply(retGP.mtx.IdX+1,1,cumprod)
retCUM.mtx.IdX <- t(retCUM.mtx.IdX - 1)
#retCUM.mtx.IdX <- cbind.data.frame(retCUM.mtx.IdX,Invest)

retCUM.mtx.SR <-apply(retGP.mtx.SR+1,1,cumprod)
retCUM.mtx.SR <- t(retCUM.mtx.SR - 1)
#retCUM.mtx.SR <- cbind.data.frame(retCUM.mtx.SR,Invest)

## End Cumulative Returns

retGP.cum <- t(Return.cumulative(t(retGP.mtx.red)))
retGP.clust.cum <- t(Return.cumulative(t(retGP.clust.mtx.red)))
retGP.bench.cum <- t(Return.cumulative(t(retGP.bench.mtx.red)))

retGP.ann <- funcRetCum(retGP.cum, ncmax)
retGP.clust.ann <- funcRetCum(retGP.clust.cum,ncmax)
retGP.bench.ann <- funcRetCum(retGP.bench.cum,ncmax)

InvestStrateg1.mtx <- matrix(0, nrow = nrow(InvestDays.mtx.red), ncol = 24, byrow = TRUE)
colnames(InvestStrateg1.mtx)<- c('MVRetAcc','ClustRetAcc.SR','IdXRetAcc',
                                 'MVCalmar','ClustCalmar.SR','IdXCalmar',
                                 'MVMaxDD','ClustMaxDD.SR','IdXMaxDD',
                                 'MVOmega','ClustOmega.SR','IdXOmega',
                                 'MVVaR','ClustVaR.SR','IdXVaR',
                                 'MVETL','ClustETL.SR','IdXETL',
                                 'MVSharpeR','ClustSharpeR.SR','IdXSharpeR',
                                 'MVRetAnn','ClustRetAnn.SR','IdXRetAnn')

newinvdays.red <- nrow(InvestDays.mtx.red)
CumRetStrateg1.tot <- data.frame()
CumRetStrateg1.one <- data.frame()
for(i in 1:newinvdays.red){
  idx.alloc <- InvestDays.mtx.red[i,]

  days <- NewInvestDays[idx.alloc] + 30 #Selected End-Test days
  temp <- Strateg1.tab[idx.alloc]$retGP
  temp <- cbind(temp, Strateg1.tab[idx.alloc]$retGP.clust)
  temp <- cbind(temp, Strateg1.tab[idx.alloc]$retGP.bench)
  rend.xts <- as.xts(temp,days)
  
  #Return cumulation
  CumRetStrateg1.one <-apply(temp+1,2,cumprod)
  CumRetStrateg1.one <- CumRetStrateg1.one - 1
  CumRetStrateg1.one <- cbind.data.frame(CumRetStrateg1.one,days)
  CumRetStrateg1.tot <- rbind(CumRetStrateg1.tot, data.frame(CumRetStrateg1.one,i))
  
  SR <- as.sr(rend.xts, c0 = 0, ope = 12, epoch = "yr")
  
  InvestStrateg1.mtx[i,c(1:3)] <- Return.cumulative(rend.xts)
  InvestStrateg1.mtx[i,c(4:6)] <- CalmarRatio(rend.xts)
  InvestStrateg1.mtx[i,c(7:9)] <- maxDrawdown(rend.xts)
  InvestStrateg1.mtx[i,c(10:12)] <- Omega(rend.xts, method = 'simple')
  InvestStrateg1.mtx[i,c(13:15)] <- VaR(rend.xts, method='historical')
  InvestStrateg1.mtx[i,c(16:18)] <- ETL(rend.xts, method = 'historical') #Expected Shortfall or ConditionalValue at Risk or Expected Tail Loss
  InvestStrateg1.mtx[i,c(19:21)] <- SR$sr[1:3]
  InvestStrateg1.mtx[i,c(22:24)] <- Return.annualized(rend.xts, scale = 12, geometric = TRUE)
  
}

colnames(CumRetStrateg1.tot) <- c('MV','MV.clust','IdX','Date','Investor')

InvestStrateg1.df <- data.frame(InvestStrateg1.mtx)
#InvestStrateg1.df$Date <- NewInvestDays[1:(newinvdays-29)]

InvestStrateg1.df$Date <- SelectedStartDays 
# colnames(InvestStrateg1.df) <- c('MVRetAcc.SR','ClustRetAcc.SR','IdXRetAcc.SR','MVCalmar.SR',
#                                    'ClusCalmar.SR','IdXCalmar.SR','MVMaxDD.SR','ClustMaxDD.SR','IdXMaxDD.SR',
#                                    'MVOmega.SR','ClustOmega.SR','IdXOmega.SR','MVVaR.SR','ClustVaR.SR','IdXVar.SR',
#                                    'MVETL.SR','ClustETL.SR','IdXETL.SR','MVSharpeR.SR','ClustSharpeR.SR','IdXSharpeR.SR',
#                                    'MVRetAnn.SR','ClustRetAnn.SR','IdXRetAnn.SR',
#                                    'Date')

InvestStrategAll.df <- InvestStrateg1.df
# colnames(InvestStrategAll.df) <- c('SRetAcc.SR','XRetAcc.SR','BRetAcc.SR','SCalmar.SR',
#                                    'XCalmar.SR','BCalmar.SR','SMaxDD.SR','XMaxDD.SR','BMaxDD.SR',
#                                    'SOmega.SR','XOmega.SR','BOmega.SR','SVaR.SR','XVaR.SR','BVar.SR',
#                                    'SETL.SR','XETL.SR','BETL.SR','SSharpeR.SR','XSharpeR.SR','BSharpeR.SR',
#                                    'SRetAnn.SR','XRetAnn.SR','BRetAnn.SR',
#                                    'Date')



########################
### Plots: Strateg1 ####
########################

### Cumulative Returns per investor ############
CumRetStrateg1.tot <- CumRetStrateg1.tot[order(CumRetStrateg1.tot$Date),]
CumRetStrateg1.long <- CumRetStrateg1.tot %>%
   gather(key='variable',value = 'value',c(-Date,-Investor))

 #By Strategy

 p <- ggplot(CumRetStrateg1.long, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size=0.5)
 p + labs(title = 'Cumulative Returns: SR strategy (Strategy 1)')
 
  
 p<- ggplot(CumRetStrateg1.long, aes(x = Date, y = value)) +
   geom_line(aes(color = variable), size=0.5) + ylim(0,25)
 p + labs(title = 'Cumulative Returns (detail): SR strategy (Strategy 1)') + 
   ylab('Ordinary value')

 p <- ggplot(CumRetStrateg1.long, aes(x = Date, y = log10(value))) +
   geom_line(aes(color = variable), size=0.5)
 p + labs(title = 'Cumulative Returns: SR strategy (Strategy 1)')

 #By Investor
 
 p <- ggplot(CumRetStrateg1.long, aes(x = Date, y = log10(value))) +
   geom_line(aes(color = Investor), size=0.1)
 p + labs(title = 'Cumulative Returns by Investor: SR strategy (Strategy 1)')

strat1.AcRet <- InvestStrateg1.df[,c('Date','MVRetAcc','ClustRetAcc.SR','IdXRetAcc')]
colnames(strat1.AcRet) <- c('Date','MV','MV.clust','Idx')
strat1.AcRet.xts <- as.xts(strat1.AcRet[,-1],strat1.AcRet[,1])

chart.TimeSeries(strat1.AcRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: SR strategy (Strategy 1)',
                 legend.loc = 'topleft')
chart.TimeSeries(log10(strat1.AcRet.xts), colorset=rich6equal,lwd=2,
                 main = 'Total Cumulative Log(Returns): SR strategy (Strategy 1)',
                 legend.loc = 'topright')


strat1.AnnRet <- InvestStrateg1.df[,c('Date','MVRetAnn','ClustRetAnn.SR','IdXRetAnn')]
colnames(strat1.AnnRet) <- c('Date','MV','MV.clust','Idx')
strat1.AnnRet.xts <- as.xts(strat1.AnnRet[,-1],strat1.AnnRet[,1])

chart.TimeSeries(strat1.AnnRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized Monthly Returns: SR strategy (Strategy 1)',
                 legend.loc = 'bottomright')
chart.TimeSeries(log10(strat1.AnnRet.xts), colorset=rich6equal,lwd=2,
                 main = 'Annualized Monthly Log(Returns) per Investor:SR strategy (Strategy 1)',
                 legend.loc = 'bottomright')

strat1.SR <- InvestStrateg1.df[,c('Date','MVSharpeR','ClustSharpeR.SR','IdXSharpeR')]
strat1.SR <- strat1.SR[strat1.SR$IdXSharpeR < 50,]
strat1.SR <- strat1.SR[strat1.SR$ClustSharpeR < 50,]
strat1.SR <- na.omit(strat1.SR)
colnames(strat1.SR) <- c('Date','MV','MV.clust','Idx')
strat1.SR.xts <- as.xts(strat1.SR[,-1],strat1.SR[,1])
chart.TimeSeries(strat1.SR.xts, colorset = rich6equal,lwd=2,
                 main = 'Annualized Sharpe Ratio: SR strategy (Strategy 1)',
                 legend.loc = 'bottomleft')

strat1.DD <- InvestStrateg1.df[,c('Date','MVMaxDD','ClustMaxDD.SR','IdXMaxDD')]
colnames(strat1.DD) <- c('Date','MV','MV.clust','Idx')
strat1.DD.xts <- as.xts(strat1.DD[,-1],strat1.DD[,1])
chart.TimeSeries(-strat1.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: SR strategy (Strategy 1)',
                 legend.loc = 'topright')

###########################################################################
#                Descriptive statistics for daily investing days          #
###########################################################################

 #Strateg1.xts
 chart.TimeSeries(Strateg1.xts, colorset = rich6equal,lwd = 2,
                  main = 'Daily Returns: SR strategy (Strategy 1)',
                  legend.loc = 'topleft')

 chart.Boxplot(Strateg1.xts, main = "Daily Returns")


 chart.Histogram(Strateg1.xts[,1,drop=F], main = "Density: MV", breaks=40, 
                 methods = c("add.density", "add.normal","add.risk"))
 chart.Histogram(Strateg1.xts[,2,drop=F], main = "Density: MV.clust", breaks=40, 
                 methods = c("add.density", "add.normal","add.risk"))
 chart.Histogram(Strateg1.xts[,3,drop=F], main = "Density: IdX", breaks=40, 
                 methods = c("add.density", "add.normal","add.risk"))


########################################################################################
# Strategy 2: Centroid-based strategies considering investor risk-aversion
# For certain risk-aversion (volatility percentile) interval we choose the higher return centroid
########################################################################################

Strateg2.tab <- Feat.tab.filtered
Strateg2.aux <- unique(Feat.tab.filtered[,c('dateTestStart','dateTestEnd','retGP','retGP.bench', 'cardGP')])


#1) Low Risk: investor (higher risk-aversion) ####################################################
Strateg2.LR <- Strateg2.tab[Risk=='Low',]
Strateg2.LR <- Strateg2.LR[Strateg2.LR[,.I[which.max(RtMeanTrain.clust)], by=dateTestStart]$V1]
#Strateg2.LR$retGP.bench <- (Strateg2.LR$benchTest-BaseInv)/BaseInv
Strateg2.LR <- merge(Strateg2.aux, Strateg2.LR[,c('dateTestEnd','retGP.clust','cardGP.clust','card.clust')], all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')
Strateg2.LR[is.na(Strateg2.LR)] <- 0
# Strateg2.LR <- Strateg2.LR[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
#                                 WBaseInv, WBaseInv.clust,
#                                 accumGainTest, accumGainTest.clust, 
#                                 retGP, retGP.clust, retGP.bench, benchTest,SRTest, SRTest.clust,
#                                 cardGP, cardGP.clust,
#                                 clust)]

Strateg2LR.df <- as.data.frame(Strateg2.LR)
# StrategAll.df <- merge(StrategAll.df,Strateg2LR.df[,c('dateTestEnd','retGP.clust')], all.x = TRUE, 
#                        by.x = 'Date', by.y = 'dateTestEnd')
Strateg2LR.xts <- as.xts(Strateg2LR.df[,c('retGP','retGP.clust','retGP.bench')],
                         Strateg2LR.df[,c('dateTestEnd')])

colnames(Strateg2LR.xts) <- c('MV','MV.clust','IdX')

StrategAll.df <- merge(StrategAll.df,Strateg2.LR[,c('dateTestEnd','retGP.clust')], all.x=TRUE,
                       by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df) <- c('Date','RetMV','RetMV.SR','RetIdX','RetMV.LR')

CardAll.df <- merge(CardAll.df, Strateg2.LR[,c('dateTestEnd','cardGP.clust')], all.x = TRUE, by.x='Date', by.y = 'dateTestEnd')
colnames(CardAll.df) <- c('Date','cardMV','cardMV.SR','cardMV.LR')

###########################################################################
# We select investing days from 12 to 41 (30 investors) with 28 investing days #
###########################################################################

NewInvestDays <- Strateg2.LR$dateTestStart #Days on which new investors can allocate/re-allocate a portfolio
newinvdays <- length(NewInvestDays)
if(newinvdays%%30 != 0){
  PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
} else {PortfReallocatSeq <- newinvdays/30}
#PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
InvestDays.mtx<- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.clust.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.bench.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)

for(i in 1:newinvdays){
  dayspos <- seq.int(i, newinvdays, by=30)
  InvestDays.mtx[i,1:(length(dayspos))] <- dayspos
  retGP.mtx[i,1:(length(dayspos))] <- Strateg2LR.df[dayspos,"retGP"]
  retGP.clust.mtx[i,1:(length(dayspos))] <- Strateg2LR.df[dayspos,"retGP.clust"]
  retGP.bench.mtx[i,1:(length(dayspos))] <- Strateg2LR.df[dayspos,"retGP.bench"]
}


#####################################################################################
#      We filter-in selected investors per month (30 investors, 28 allocation days) #      #
#####################################################################################

nrmin <- 12
nrmax <- 41 #Maximum number of investors: nrmax - nrmin + 1 = 30
ncmin <- 1
ncmax <- 28 #Max. number of cumulated investing months: 

SelectedStartDays <- NewInvestDays[nrmin:nrmax] #From 2019-012-12 to 2019-02-10

InvestDays.mtx.red <- InvestDays.mtx[nrmin:nrmax,ncmin:ncmax] #We select maximum number of investors
months <- ncol(InvestDays.mtx.red)
retGP.mtx.red <- retGP.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.clust.mtx.red <- retGP.clust.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.bench.mtx.red <- retGP.bench.mtx[nrmin:nrmax,ncmin:ncmax]

retGP.mtx.LR <- retGP.clust.mtx.red

retCUM.mtx.LR <-apply(retGP.mtx.LR+1,1,cumprod)
retCUM.mtx.LR <- t(retCUM.mtx.LR - 1)
#retCUM.mtx.LR <- cbind.data.frame(retCUM.mtx.LR,Invest)

retGP.cum <- t(Return.cumulative(t(retGP.mtx.red)))
retGP.clust.cum <- t(Return.cumulative(t(retGP.clust.mtx.red)))
retGP.bench.cum <- t(Return.cumulative(t(retGP.bench.mtx.red)))

retGP.ann <- funcRetCum(retGP.cum, ncmax)
retGP.clust.ann <- funcRetCum(retGP.clust.cum, ncmax)
retGP.bench.ann <- funcRetCum(retGP.bench.cum, ncmax)

newinvdays.red <- nrow(InvestDays.mtx.red)
InvestStrateg2LR.mtx <- matrix(0, nrow = newinvdays.red, ncol = 24, byrow = TRUE)
colnames(InvestStrateg2LR.mtx)<- c('MVRetAcc.LR','ClustRetAcc.LR','IdXRetAcc.LR',
                                   'MVCalmar.LR','ClustCalmar.LR','IdXCalmar.LR',
                                   'MVMaxDD.LR','ClustMaxDD.LR','IdXMaxDD.LR',
                                   'MVOmega.LR','ClustOmega.LR','IdXOmega.LR',
                                   'MVVaR.LR','ClustVaR.LR','IdXVaR.LR',
                                   'MVETL.LR','ClustETL.LR','IdXETL.LR',
                                   'MVSharpeR.LR','ClustSharpeR.LR','IdXSharpeR.LR',
                                   'MVRetAnn.LR','ClustRetAnn.LR','IdXRetAnn.LR')


CumRetStrateg2LR.tot <- data.frame()
CumRetStrateg2LR.one <- data.frame()

for(i in 1:newinvdays.red){
  idx.alloc <- InvestDays.mtx.red[i,]
  #BaseInv.tot <- length(which(idx.alloc!=0))*BaseInv
  
  days <- NewInvestDays[idx.alloc] + 30 #End-Test days
  temp <- Strateg2.LR[idx.alloc]$retGP
  temp <- cbind(temp, Strateg2.LR[idx.alloc]$retGP.clust)
  temp <- cbind(temp, Strateg2.LR[idx.alloc]$retGP.bench)
  rend.xts <- as.xts(temp,days)
  
  #Return cumulation
  CumRetStrateg2LR.one <-apply(temp+1,2,cumprod)
  CumRetStrateg2LR.one <- CumRetStrateg2LR.one - 1
  CumRetStrateg2LR.one <- cbind.data.frame(CumRetStrateg2LR.one,days)
  CumRetStrateg2LR.tot <- rbind(CumRetStrateg2LR.tot, data.frame(CumRetStrateg2LR.one,i))
  
  SR <- as.sr(rend.xts, c0 = 0, ope = 12, epoch = "yr")
  
  InvestStrateg2LR.mtx[i,c(1:3)] <- Return.cumulative(rend.xts)
  InvestStrateg2LR.mtx[i,c(4:6)] <- CalmarRatio(rend.xts)
  InvestStrateg2LR.mtx[i,c(7:9)] <- maxDrawdown(rend.xts)
  InvestStrateg2LR.mtx[i,c(10:12)] <- Omega(rend.xts, method = 'simple')
  InvestStrateg2LR.mtx[i,c(13:15)] <- VaR(rend.xts, method='historical')
  InvestStrateg2LR.mtx[i,c(16:18)] <- ETL(rend.xts, method = 'historical') #Expected Shortfall or ConditionalValue at Risk or Expected Tail Loss
  InvestStrateg2LR.mtx[i,c(19:21)] <- SR$sr[1:3]
  InvestStrateg2LR.mtx[i,c(22:24)] <- Return.annualized(rend.xts, scale = 12, geometric = TRUE)

}

colnames(CumRetStrateg2LR.tot) <- c('MV','MV.clust','IdX','Date','Investor')

InvestStrateg2LR.df <- data.frame(InvestStrateg2LR.mtx)
InvestStrateg2LR.df$Date <- SelectedStartDays

# colnames(InvestStrateg2LR.df) <- c('MVRetAcc.LR','ClustRetAcc.LR','IdXRetAcc.LR','MVCalmar.LR',
#                                    'ClusCalmar.LR','IdXCalmar.LR','MVMaxDD.LR','ClustMaxDD.LR','IdXMaxDD.LR',
#                                    'MVOmega.LR','ClustOmega.LR','IdXOmega.LR','MVVaR.LR','ClustVaR.LR','IdXVar.LR',
#                                    'MVETL.LR','ClustETL.LR','IdXETL.LR','MVSharpeR.LR','ClustSharpeR.LR','IdXSharpeR.LR',
#                                    'MVRetAnn.LR','ClustRetAnn.LR','IdXRetAnn.LR',
#                                    'Date')

InvestStrategAll.df <- merge(InvestStrategAll.df, InvestStrateg2LR.df, by = 'Date', all.x = TRUE)
#colnames(InvestStrategAll.df) <- c('Date','SRetAcc.SR','XRetAcc.SR','BRetAcc.SR','SCalmar.SR',
#                                   'XCalmar.SR','BCalmar.SR','SMaxDD.SR','XMaxDD.SR','BMaxDD.SR',
#                                   'SOmega.SR','XOmega.SR','BOmega.SR','SVaR.SR','XVaR.SR','BVar')


########################
### Plots: Strateg2a ####
########################
### Cumulative Returns per investor ############
CumRetStrateg2LR.tot <- CumRetStrateg2LR.tot[order(CumRetStrateg2LR.tot$Date),]
CumRetStrateg2LR.long <- CumRetStrateg2LR.tot %>%
  gather(key='variable',value = 'value',c(-Date,-Investor))

#By Strategy
p <- ggplot(CumRetStrateg2LR.long, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = variable), size=0.5)
p + labs(title = 'Cumulative Returns: LR strategy (Strategy 2a)')

#By Investor
# 
# p <- ggplot(CumRetStrateg2LR.long, aes(x = Date, y = log10(value))) +
#   geom_line(aes(color = Investor), size=0.1)
# p + labs(title = 'Monthly Cumulative Log(Returns) per Investor:LR strategy (Strategy 2a)')

strat2a.AcRet <- InvestStrateg2LR.df[,c('Date','MVRetAcc.LR','ClustRetAcc.LR','IdXRetAcc.LR')]
colnames(strat2a.AcRet) <- c('Date','MV','MV.clust','Idx')
strat2a.AcRet.xts <- as.xts(strat2a.AcRet[,-1],strat2a.AcRet[,1])

chart.TimeSeries(strat2a.AcRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: LR strategy (Strategy 2a)',
                 legend.loc = 'topleft')
# chart.TimeSeries(log10(strat2a.AcRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Total Cumulative Returns: LR strategy (Strategy 2a)',
#                  legend.loc = 'topleft')


strat2a.AnnRet <- InvestStrateg2LR.df[,c('Date','MVRetAnn.LR','ClustRetAnn.LR','IdXRetAnn.LR')]
colnames(strat2a.AnnRet) <- c('Date','MV','MV.clust','Idx')
strat2a.AnnRet.xts <- as.xts(strat2a.AnnRet[,-1],strat2a.AnnRet[,1])

chart.TimeSeries(strat2a.AnnRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized Monthly Returns: LR strategy (Strategy 2a)',
                 legend.loc = 'topright')
# chart.TimeSeries(log10(strat2a.AnnRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Annualized monthly Log(Returns) per Investor:LR strategy (Strategy 2a)',
#                  legend.loc = 'topright')

strat2a.SR <- InvestStrateg2LR.df[,c('Date','MVSharpeR.LR','ClustSharpeR.LR','IdXSharpeR.LR')]
strat2a.SR <- strat2a.SR[strat2a.SR$IdXSharpeR.LR < 50,]
strat2a.SR <- strat2a.SR[strat2a.SR$ClustSharpeR.LR < 50,]
strat2a.SR <- na.omit(strat2a.SR)
colnames(strat2a.SR) <- c('Date','MV','MV.clust','Idx')
strat2a.SR.xts <- as.xts(strat2a.SR[,-1],strat2a.SR[,1])
chart.TimeSeries(strat2a.SR.xts, colorset = rich6equal,lwd=2,
                 main = 'Annualized Sharpe Ratio: LR strategy (Strategy 2a)',
                 legend.loc = 'topright')

strat2a.DD <- InvestStrateg2LR.df[,c('Date','MVMaxDD.LR','ClustMaxDD.LR','IdXMaxDD.LR')]
colnames(strat2a.DD) <- c('Date','MV','MV.clust','Idx')
strat2a.DD.xts <- as.xts(strat2a.DD[,-1],strat2a.DD[,1])
chart.TimeSeries(-strat2a.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: LR strategy (Strategy 2a)',
                 legend.loc = 'topright')

###########################################################################
#                Descriptive statistics for daily investing days          #
###########################################################################

#Strateg2LR.xts
chart.TimeSeries(Strateg2LR.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Returns: LR strategy (Strategy 2a)',
                 legend.loc = 'topleft')
### Zoom-In ##################
chart.TimeSeries(Strateg2LR.xts, colorset = rich6equal,lwd = 2, ylim = c(-0.4,1.0),
                 main = 'Daily Returns (detail): LR strategy (Strategy 2a)',
                 legend.loc = 'topleft')
##############################

chart.Boxplot(Strateg2LR.xts, main = "Daily Returns")


# chart.Histogram(Strateg2LR.xts[,1,drop=F], main = "Density: MV", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))
chart.Histogram(Strateg2LR.xts[,2,drop=F], main = "Density: MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"))
#### Zomm-In ###############
chart.Histogram(Strateg2LR.xts[,2,drop=F], main = "Density (detail): MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"), xlim = c(-1,2))
############################
# chart.Histogram(Strateg2LR.xts[,3,drop=F], main = "Density: IdX", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))




#2) Mean Risk: investor (Mean risk-aversion) #######################################################
Strateg2.MR <- Strateg2.tab[Risk=='Average',]
Strateg2.MR <- Strateg2.MR[Strateg2.MR[,.I[which.max(RtMeanTrain.clust)], by=dateTestStart]$V1]
#Strateg2.MR$retGP.bench <- (Strateg2.MR$benchTest-BaseInv)/BaseInv

Strateg2.MR <- merge(Strateg2.aux, Strateg2.MR[,c('dateTestEnd','retGP.clust','cardGP.clust','card.clust')], all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')
Strateg2.MR[is.na(Strateg2.MR)] <- 0
# Strateg2.LR <- Strateg2.LR[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
#                                 WBaseInv, WBaseInv.clust,
#                                 accumGainTest, accumGainTest.clust, 
#                                 retGP, retGP.clust, retGP.bench, benchTest,SRTest, SRTest.clust,
#                                 cardGP, cardGP.clust,
#                                 clust)]

Strateg2MR.df <- as.data.frame(Strateg2.MR)
# StrategAll.df <- merge(StrategAll.df,Strateg2LR.df[,c('dateTestEnd','retGP.clust')], all.x = TRUE, 
#                        by.x = 'Date', by.y = 'dateTestEnd')
Strateg2MR.xts <- as.xts(Strateg2MR.df[,c('retGP','retGP.clust','retGP.bench')],
                         Strateg2MR.df[,c('dateTestEnd')])

colnames(Strateg2MR.xts) <- c('MV','MV.clust','IdX')

StrategAll.df <- merge(StrategAll.df,Strateg2.MR[,c('dateTestEnd','retGP.clust')], all.x=TRUE,
                       by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df) <- c('Date','RetMV','RetMV.SR','RetIdX','RetMV.LR','RetMV.MR')

CardAll.df <- merge(CardAll.df, Strateg2.MR[,c('dateTestEnd','cardGP.clust')], all.x = TRUE, 
                    by.x='Date', by.y = 'dateTestEnd')
colnames(CardAll.df) <- c('Date','cardMV','cardMV.SR','cardMV.LR','cardMV.MR')

###########################################################################
# We select investing days from 12 to 41 (30 investors) with 28 investing days #
###########################################################################

NewInvestDays <- Strateg2.MR$dateTestStart #Days on which new investors can allocate/re-allocate a portfolio
newinvdays <- length(NewInvestDays)
if(newinvdays%%30 != 0){
  PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
} else {PortfReallocatSeq <- newinvdays/30}
#PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
InvestDays.mtx<- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.clust.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.bench.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)

for(i in 1:newinvdays){
  dayspos <- seq.int(i, newinvdays, by=30)
  InvestDays.mtx[i,1:(length(dayspos))] <- dayspos
  retGP.mtx[i,1:(length(dayspos))] <- Strateg2MR.df[dayspos,"retGP"]
  retGP.clust.mtx[i,1:(length(dayspos))] <- Strateg2MR.df[dayspos,"retGP.clust"]
  retGP.bench.mtx[i,1:(length(dayspos))] <- Strateg2MR.df[dayspos,"retGP.bench"]
}


#####################################################################################
#      We filter-in selected investors per month (30 investors, 28 allocation days) #      #
#####################################################################################

nrmin <- 12
nrmax <- 41 #Maximum number of investors: nrmax - nrmin + 1 = 30
ncmin <- 1
ncmax <- 28 #Max. number of cumulated investing months: 

SelectedStartDays <- NewInvestDays[nrmin:nrmax] #From 2019-012-12 to 2019-02-10

InvestDays.mtx.red <- InvestDays.mtx[nrmin:nrmax,ncmin:ncmax] #We select maximum number of investors
months <- ncol(InvestDays.mtx.red)
retGP.mtx.red <- retGP.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.clust.mtx.red <- retGP.clust.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.bench.mtx.red <- retGP.bench.mtx[nrmin:nrmax,ncmin:ncmax]

retGP.mtx.MR <- retGP.clust.mtx.red

retCUM.mtx.MR <-apply(retGP.mtx.MR+1,1,cumprod)
retCUM.mtx.MR <- t(retCUM.mtx.MR - 1)
#retCUM.mtx.MR <- cbind.data.frame(retCUM.mtx.MR,Invest)

retGP.cum <- t(Return.cumulative(t(retGP.mtx.red)))
retGP.clust.cum <- t(Return.cumulative(t(retGP.clust.mtx.red)))
retGP.bench.cum <- t(Return.cumulative(t(retGP.bench.mtx.red)))

retGP.ann <- funcRetCum(retGP.cum, ncmax)
retGP.clust.ann <- funcRetCum(retGP.clust.cum, ncmax)
retGP.bench.ann <- funcRetCum(retGP.bench.cum, ncmax)

newinvdays.red <- nrow(InvestDays.mtx.red)
InvestStrateg2MR.mtx <- matrix(0, nrow = newinvdays.red, ncol = 24, byrow = TRUE)
colnames(InvestStrateg2MR.mtx)<- c('MVRetAcc.MR','ClustRetAcc.MR','IdXRetAcc.MR',
                                   'MVCalmar.MR','ClustCalmar.MR','IdXCalmar.MR',
                                   'MVMaxDD.MR','ClustMaxDD.MR','IdXMaxDD.MR',
                                   'MVOmega.MR','ClustOmega.MR','IdXOmega.MR',
                                   'MVVaR.MR','ClustVaR.MR','IdXVaR.MR',
                                   'MVETL.MR','ClustETL.MR','IdXETL.MR',
                                   'MVSharpeR.MR','ClustSharpeR.MR','IdXSharpeR.MR',
                                   'MVRetAnn.MR','ClustRetAnn.MR','IdXRetAnn.MR')


CumRetStrateg2MR.tot <- data.frame()
CumRetStrateg2MR.one <- data.frame()

for(i in 1:newinvdays.red){
  idx.alloc <- InvestDays.mtx.red[i,]
  #BaseInv.tot <- length(which(idx.alloc!=0))*BaseInv
  
  days <- NewInvestDays[idx.alloc] + 30 #End-Test days
  temp <- Strateg2.MR[idx.alloc]$retGP
  temp <- cbind(temp, Strateg2.MR[idx.alloc]$retGP.clust)
  temp <- cbind(temp, Strateg2.MR[idx.alloc]$retGP.bench)
  rend.xts <- as.xts(temp,days)
  
  #Return cumulation
  CumRetStrateg2MR.one <-apply(temp+1,2,cumprod)
  CumRetStrateg2MR.one <- CumRetStrateg2MR.one - 1
  CumRetStrateg2MR.one <- cbind.data.frame(CumRetStrateg2MR.one,days)
  CumRetStrateg2MR.tot <- rbind(CumRetStrateg2MR.tot, data.frame(CumRetStrateg2MR.one,i))
  
  SR <- as.sr(rend.xts, c0 = 0, ope = 12, epoch = "yr")
  
  InvestStrateg2MR.mtx[i,c(1:3)] <- Return.cumulative(rend.xts)
  InvestStrateg2MR.mtx[i,c(4:6)] <- CalmarRatio(rend.xts)
  InvestStrateg2MR.mtx[i,c(7:9)] <- maxDrawdown(rend.xts)
  InvestStrateg2MR.mtx[i,c(10:12)] <- Omega(rend.xts, method = 'simple')
  InvestStrateg2MR.mtx[i,c(13:15)] <- VaR(rend.xts, method='historical')
  InvestStrateg2MR.mtx[i,c(16:18)] <- ETL(rend.xts, method = 'historical') #Expected Shortfall or ConditionalValue at Risk or Expected Tail Loss
  InvestStrateg2MR.mtx[i,c(19:21)] <- SR$sr[1:3]
  InvestStrateg2MR.mtx[i,c(22:24)] <- Return.annualized(rend.xts, scale = 12, geometric = TRUE)
  
}

colnames(CumRetStrateg2MR.tot) <- c('MV','MV.clust','IdX','Date','Investor')

InvestStrateg2MR.df <- data.frame(InvestStrateg2MR.mtx)
InvestStrateg2MR.df$Date <- SelectedStartDays

InvestStrategAll.df <- merge(InvestStrategAll.df, InvestStrateg2MR.df, by = 'Date', all.x = TRUE)
#colnames(InvestStrategAll.df) <- c('Date','SRetAcc.SR','XRetAcc.SR','BRetAcc.SR','SCalmar.SR',
#                                   'XCalmar.SR','BCalmar.SR','SMaxDD.SR','XMaxDD.SR','BMaxDD.SR',
#                                   'SOmega.SR','XOmega.SR','BOmega.SR','SVaR.SR','XVaR.SR','BVar')

########################
### Plots: Strateg2b ####
########################
### Cumulative Returns per investor ############
CumRetStrateg2MR.tot <- CumRetStrateg2MR.tot[order(CumRetStrateg2MR.tot$Date),]
CumRetStrateg2MR.long <- CumRetStrateg2MR.tot %>%
  gather(key='variable',value = 'value',c(-Date,-Investor))

#By Strategy
p <- ggplot(CumRetStrateg2MR.long, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = variable), size=0.5)
p + labs(title = 'Cumulative Returns: MR strategy (Strategy 2b)')

# #By Investor
# 
# p <- ggplot(CumRetStrateg2MR.long, aes(x = Date, y = log10(value))) +
#   geom_line(aes(color = Investor), size=0.1)
# p + labs(title = 'Monthly Cumulative Log(Returns) per Investor:MR strategy (Strategy 2b)')

strat2b.AcRet <- InvestStrateg2MR.df[,c('Date','MVRetAcc.MR','ClustRetAcc.MR','IdXRetAcc.MR')]
colnames(strat2b.AcRet) <- c('Date','MV','MV.clust','Idx')
strat2b.AcRet.xts <- as.xts(strat2b.AcRet[,-1],strat2b.AcRet[,1])

chart.TimeSeries(strat2b.AcRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: MR strategy (Strategy 2b)',
                 legend.loc = 'topleft')
# chart.TimeSeries(log10(strat2b.AcRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Total Monthly Cumulative Log(Returns) per Investor: MR strategy (Strategy 2b)',
#                  legend.loc = 'topright')


strat2b.AnnRet <- InvestStrateg2MR.df[,c('Date','MVRetAnn.MR','ClustRetAnn.MR','IdXRetAnn.MR')]
colnames(strat2b.AnnRet) <- c('Date','MV','MV.clust','Idx')
strat2b.AnnRet.xts <- as.xts(strat2b.AnnRet[,-1],strat2b.AnnRet[,1])

chart.TimeSeries(strat2b.AnnRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized Monthly Returns: MR strategy (Strategy 2b)',
                 legend.loc = 'right')
# chart.TimeSeries(log10(strat2b.AnnRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Annualized monthly Log(Returns) per Investor:MR strategy (Strategy 2b)',
#                  legend.loc = 'topright')

strat2b.SR <- InvestStrateg2MR.df[,c('Date','MVSharpeR.MR','ClustSharpeR.MR','IdXSharpeR.MR')]
strat2b.SR <- strat2b.SR[strat2b.SR$IdXSharpeR.MR < 50,]
strat2b.SR <- strat2b.SR[strat2b.SR$ClustSharpeR.MR < 50,]
strat2b.SR <- na.omit(strat2b.SR)
colnames(strat2b.SR) <- c('Date','MV','MV.clust','Idx')
strat2b.SR.xts <- as.xts(strat2b.SR[,-1],strat2b.SR[,1])
chart.TimeSeries(strat2b.SR.xts, colorset = rich6equal,lwd=2,
                 main = 'Annualized Sharpe Ratio: MR strategy (Strategy 2b)',
                 legend.loc = 'topleft')

strat2b.DD <- InvestStrateg2MR.df[,c('Date','MVMaxDD.MR','ClustMaxDD.MR','IdXMaxDD.MR')]
colnames(strat2b.DD) <- c('Date','MV','MV.clust','Idx')
strat2b.DD.xts <- as.xts(strat2b.DD[,-1],strat2b.DD[,1])
chart.TimeSeries(-strat2b.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: MR strategy (Strategy 2b)',
                 legend.loc = 'topright')

###########################################################################
#                Descriptive statistics for daily investing days          #
###########################################################################

#Strateg2MR.xts
chart.TimeSeries(Strateg2MR.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Returns: MR strategy (Strategy 2b)',
                 legend.loc = 'topleft')
### Zoom-In ##################
chart.TimeSeries(Strateg2MR.xts, colorset = rich6equal,lwd = 2, ylim = c(-0.4,1.0),
                 main = 'Daily Returns (detail): MR strategy (Strategy 2b)',
                 legend.loc = 'topleft')
##############################

chart.Boxplot(Strateg2MR.xts, main = "Daily Returns")


# chart.Histogram(Strateg2MR.xts[,1,drop=F], main = "Density: MV", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))
chart.Histogram(Strateg2MR.xts[,2,drop=F], main = "Density: MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"))
#### Zomm-In ###############
chart.Histogram(Strateg2MR.xts[,2,drop=F], main = "Density (detail): MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"), xlim = c(-1,2))
############################
# chart.Histogram(Strateg2MR.xts[,3,drop=F], main = "Density: IdX", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))



#3) High Risk: investor (Low risk-aversion)
Strateg2.HR <- Strateg2.tab[Risk=='High',]
Strateg2.HR <- Strateg2.HR[Strateg2.HR[,.I[which.max(RtMeanTrain.clust)], by=dateTestStart]$V1]

Strateg2.HR <- merge(Strateg2.aux, Strateg2.HR[,c('dateTestEnd','retGP.clust','cardGP.clust','card.clust')], all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')
Strateg2.HR[is.na(Strateg2.HR)] <- 0
# Strateg2.LR <- Strateg2.LR[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
#                                 WBaseInv, WBaseInv.clust,
#                                 accumGainTest, accumGainTest.clust, 
#                                 retGP, retGP.clust, retGP.bench, benchTest,SRTest, SRTest.clust,
#                                 cardGP, cardGP.clust,
#                                 clust)]

Strateg2HR.df <- as.data.frame(Strateg2.HR)
# StrategAll.df <- merge(StrategAll.df,Strateg2LR.df[,c('dateTestEnd','retGP.clust')], all.x = TRUE, 
#                        by.x = 'Date', by.y = 'dateTestEnd')
Strateg2HR.xts <- as.xts(Strateg2HR.df[,c('retGP','retGP.clust','retGP.bench')],
                         Strateg2HR.df[,c('dateTestEnd')])

colnames(Strateg2HR.xts) <- c('MV','MV.clust','IdX')

StrategAll.df <- merge(StrategAll.df,Strateg2.HR[,c('dateTestEnd','retGP.clust')], all.x=TRUE,
                       by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df) <- c('Date','MV','SR','IdX','LR','MR', 'HR')
StrategAll.df <- StrategAll.df[,c('Date','MV','IdX','SR','LR','MR', 'HR')]
StrategAll.xts <- as.xts(StrategAll.df[,-1], StrategAll.df[,1])

CardAll.df <- merge(CardAll.df, Strateg2.HR[,c('dateTestEnd','cardGP.clust')], all.x = TRUE, 
                    by.x='Date', by.y = 'dateTestEnd')
colnames(CardAll.df) <- c('Date','cardMV','cardMV.SR','cardMV.LR','cardMV.MR','cardMV.HR')

###########################################################################
# We select investing days from 12 to 41 (30 investors) with 28 investing days #
###########################################################################

NewInvestDays <- Strateg2.HR$dateTestStart #Days on which new investors can allocate/re-allocate a portfolio
newinvdays <- length(NewInvestDays)
if(newinvdays%%30 != 0){
  PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
} else {PortfReallocatSeq <- newinvdays/30}
#PortfReallocatSeq<- floor(newinvdays/30)+1 #Maximum number of monthly portfolios computed (for the older investor)
InvestDays.mtx<- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.clust.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)
retGP.bench.mtx <- matrix(0, nrow = newinvdays, ncol = PortfReallocatSeq, byrow = TRUE)

for(i in 1:newinvdays){
  dayspos <- seq.int(i, newinvdays, by=30)
  InvestDays.mtx[i,1:(length(dayspos))] <- dayspos
  retGP.mtx[i,1:(length(dayspos))] <- Strateg2HR.df[dayspos,"retGP"]
  retGP.clust.mtx[i,1:(length(dayspos))] <- Strateg2HR.df[dayspos,"retGP.clust"]
  retGP.bench.mtx[i,1:(length(dayspos))] <- Strateg2HR.df[dayspos,"retGP.bench"]
}


#####################################################################################
#      We filter-in selected investors per month (30 investors, 28 allocation days) #      #
#####################################################################################

nrmin <- 12
nrmax <- 41 #Maximum number of investors: nrmax - nrmin + 1 = 30
ncmin <- 1
ncmax <- 28 #Max. number of cumulated investing months: 

SelectedStartDays <- NewInvestDays[nrmin:nrmax] #From 2019-012-12 to 2019-02-10

InvestDays.mtx.red <- InvestDays.mtx[nrmin:nrmax,ncmin:ncmax] #We select maximum number of investors
months <- ncol(InvestDays.mtx.red)
retGP.mtx.red <- retGP.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.clust.mtx.red <- retGP.clust.mtx[nrmin:nrmax,ncmin:ncmax]
retGP.bench.mtx.red <- retGP.bench.mtx[nrmin:nrmax,ncmin:ncmax]

retGP.mtx.HR <- retGP.clust.mtx.red

retCUM.mtx.HR <-apply(retGP.mtx.HR+1,1,cumprod)
retCUM.mtx.HR <- t(retCUM.mtx.HR - 1)
#retCUM.mtx.HR <- cbind.data.frame(retCUM.mtx.HR,Invest)

retGP.cum <- t(Return.cumulative(t(retGP.mtx.red)))
retGP.clust.cum <- t(Return.cumulative(t(retGP.clust.mtx.red)))
retGP.bench.cum <- t(Return.cumulative(t(retGP.bench.mtx.red)))

retGP.ann <- funcRetCum(retGP.cum, ncmax)
retGP.clust.ann <- funcRetCum(retGP.clust.cum, ncmax)
retGP.bench.ann <- funcRetCum(retGP.bench.cum, ncmax)

newinvdays.red <- nrow(InvestDays.mtx.red)
InvestStrateg2HR.mtx <- matrix(0, nrow = newinvdays.red, ncol = 24, byrow = TRUE)
colnames(InvestStrateg2HR.mtx)<- c('MVRetAcc.HR','ClustRetAcc.HR','IdXRetAcc.HR',
                                   'MVCalmar.HR','ClustCalmar.HR','IdXCalmar.HR',
                                   'MVMaxDD.HR','ClustMaxDD.HR','IdXMaxDD.HR',
                                   'MVOmega.HR','ClustOmega.HR','IdXOmega.HR',
                                   'MVVaR.HR','ClustVaR.HR','IdXVaR.HR',
                                   'MVETL.HR','ClustETL.HR','IdXETL.HR',
                                   'MVSharpeR.HR','ClustSharpeR.HR','IdXSharpeR.HR',
                                   'MVRetAnn.HR','ClustRetAnn.HR','IdXRetAnn.HR')


CumRetStrateg2HR.tot <- data.frame()
CumRetStrateg2HR.one <- data.frame()

for(i in 1:newinvdays.red){
  idx.alloc <- InvestDays.mtx.red[i,]
  #BaseInv.tot <- length(which(idx.alloc!=0))*BaseInv
  
  days <- NewInvestDays[idx.alloc] + 30 #End-Test days
  temp <- Strateg2.HR[idx.alloc]$retGP
  temp <- cbind(temp, Strateg2.HR[idx.alloc]$retGP.clust)
  temp <- cbind(temp, Strateg2.HR[idx.alloc]$retGP.bench)
  rend.xts <- as.xts(temp,days)
  
  #Return cumulation
  CumRetStrateg2HR.one <-apply(temp+1,2,cumprod)
  CumRetStrateg2HR.one <- CumRetStrateg2HR.one - 1
  CumRetStrateg2HR.one <- cbind.data.frame(CumRetStrateg2HR.one,days)
  CumRetStrateg2HR.tot <- rbind(CumRetStrateg2HR.tot, data.frame(CumRetStrateg2HR.one,i))
  
  SR <- as.sr(rend.xts, c0 = 0, ope = 12, epoch = "yr")
  
  InvestStrateg2HR.mtx[i,c(1:3)] <- Return.cumulative(rend.xts)
  InvestStrateg2HR.mtx[i,c(4:6)] <- CalmarRatio(rend.xts)
  InvestStrateg2HR.mtx[i,c(7:9)] <- maxDrawdown(rend.xts)
  InvestStrateg2HR.mtx[i,c(10:12)] <- Omega(rend.xts, method = 'simple')
  InvestStrateg2HR.mtx[i,c(13:15)] <- VaR(rend.xts, method='historical')
  InvestStrateg2HR.mtx[i,c(16:18)] <- ETL(rend.xts, method = 'historical') #Expected Shortfall or ConditionalValue at Risk or Expected Tail Loss
  InvestStrateg2HR.mtx[i,c(19:21)] <- SR$sr[1:3]
  InvestStrateg2HR.mtx[i,c(22:24)] <- Return.annualized(rend.xts, scale = 12, geometric = TRUE)
  
}

colnames(CumRetStrateg2HR.tot) <- c('MV','MV.clust','IdX','Date','Investor')

InvestStrateg2HR.df <- data.frame(InvestStrateg2HR.mtx)
InvestStrateg2HR.df$Date <- SelectedStartDays

InvestStrategAll.df <- merge(InvestStrategAll.df, InvestStrateg2HR.df, by = 'Date', all.x = TRUE)
InvestStrateg.bck <- InvestStrategAll.df
InvestStrategAll.df <- InvestStrategAll.df[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                                              19,20,21,22,23,24,25,27,30,33,36,39,42,45,48,
                                              51,54,57,60,63,66,69,72,75,78,81,84,87,90,93,
                                              96
                                              )] #We remove MV and IdX columns

#colnames(InvestStrategAll.df) <- c('Date','SRetAcc.SR','XRetAcc.SR','BRetAcc.SR','SCalmar.SR',
#                                   'XCalmar.SR','BCalmar.SR','SMaxDD.SR','XMaxDD.SR','BMaxDD.SR',
#                                   'SOmega.SR','XOmega.SR','BOmega.SR','SVaR.SR','XVaR.SR','BVar')

#colnames(InvestStrategAll.df) <- c("Date","SRetAcc.SR","XRetAcc.SR","BRetAcc.SR", "SCalmar.SR", "XCalmar.SR", "BCalmar.SR","SMaxDD.SR")


########################
### Plots: Strateg2c ####
########################
### Cumulative Returns per investor ############
CumRetStrateg2HR.tot <- CumRetStrateg2HR.tot[order(CumRetStrateg2HR.tot$Date),]
CumRetStrateg2HR.long <- CumRetStrateg2HR.tot %>%
  gather(key='variable',value = 'value',c(-Date,-Investor))

#By Strategy
p <- ggplot(CumRetStrateg2HR.long, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = variable), size=0.5)
p + labs(title = 'Monthly Cumulative Returns: HR strategy (Strategy 2c)')

# #By Investor
# p <- ggplot(CumRetStrateg2HR.long, aes(x = Date, y = log10(value))) +
#   geom_line(aes(color = Investor), size=0.1)
# p + labs(title = 'Monthly Cumulative Log(Returns) per Investor: HR strategy (Strategy 2c)')

strat2c.AcRet <- InvestStrateg2HR.df[,c('Date','MVRetAcc.HR','ClustRetAcc.HR','IdXRetAcc.HR')]
colnames(strat2c.AcRet) <- c('Date','MV','MV.clust','Idx')
strat2c.AcRet.xts <- as.xts(strat2c.AcRet[,-1],strat2c.AcRet[,1])

chart.TimeSeries(strat2c.AcRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: HR strategy (Strategy 2c)',
                 legend.loc = 'topleft')
# chart.TimeSeries(log10(strat2c.AcRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Total Monthly Cumulative Log(Returns) per Investor:HR strategy (Strategy 2c)',
#                  legend.loc = 'topright')


strat2c.AnnRet <- InvestStrateg2HR.df[,c('Date','MVRetAnn.HR','ClustRetAnn.HR','IdXRetAnn.HR')]
colnames(strat2c.AnnRet) <- c('Date','MV','MV.clust','Idx')
strat2c.AnnRet.xts <- as.xts(strat2c.AnnRet[,-1],strat2c.AnnRet[,1])

chart.TimeSeries(strat2c.AnnRet.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized monthly Returns: HR strategy (Strategy 2c)',
                 legend.loc = 'topright')
# chart.TimeSeries(log10(strat2c.AnnRet.xts), colorset=rich6equal,lwd=2,
#                  main = 'Annualized monthly Log(Returns) per Investor:MR strategy (Strategy 2c)',
#                  legend.loc = 'topright')

strat2c.SR <- InvestStrateg2HR.df[,c('Date','MVSharpeR.HR','ClustSharpeR.HR','IdXSharpeR.HR')]
strat2c.SR <- strat2c.SR[strat2c.SR$IdXSharpeR.HR < 50,]
strat2c.SR <- strat2c.SR[strat2c.SR$ClustSharpeR.HR < 50,]
strat2c.SR <- na.omit(strat2c.SR)
colnames(strat2c.SR) <- c('Date','MV','MV.clust','Idx')
strat2c.SR.xts <- as.xts(strat2c.SR[,-1],strat2c.SR[,1])
chart.TimeSeries(strat2c.SR.xts, colorset = rich6equal,lwd=2,
                 main = 'Annualized Sharpe Ratio: HR strategy (Strategy 2c)',
                 legend.loc = 'bottomleft')

strat2c.DD <- InvestStrateg2HR.df[,c('Date','MVMaxDD.HR','ClustMaxDD.HR','IdXMaxDD.HR')]
colnames(strat2c.DD) <- c('Date','MV','MV.clust','Idx')
strat2c.DD.xts <- as.xts(strat2c.DD[,-1],strat2c.DD[,1])
chart.TimeSeries(-strat2c.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: HR strategy (Strategy 2c)',
                 legend.loc = 'topright')

###########################################################################
#                Descriptive statistics for daily investing days          #
###########################################################################

#Strateg2HR.xts
chart.TimeSeries(Strateg2HR.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Returns: HR strategy (Strategy 2c)',
                 legend.loc = 'topleft')
### Zoom-In ##################
chart.TimeSeries(Strateg2HR.xts, colorset = rich6equal,lwd = 2, ylim = c(-0.4,1.0),
                 main = 'Daily Returns (detail): HR strategy (Strategy 2c)',
                 legend.loc = 'topleft')
##############################

chart.Boxplot(Strateg2HR.xts, main = "Daily Returns")


# chart.Histogram(Strateg2HR.xts[,1,drop=F], main = "Density: MV", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))
chart.Histogram(Strateg2HR.xts[,2,drop=F], main = "Density: MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"))
#### Zomm-In ###############
chart.Histogram(Strateg2HR.xts[,2,drop=F], main = "Density (detail): MV.clust", breaks=40, 
                methods = c("add.density", "add.normal"), xlim = c(-1,2))
############################
# chart.Histogram(Strateg2HR.xts[,3,drop=F], main = "Density: IdX", breaks=40, 
#                 methods = c("add.density", "add.normal","add.risk"))


###################################################################################
#### Benchmarking of ALL strategies ###############################################
###################################################################################

CumRetStrategAll.tot <- CumRetStrateg1.tot
CumRetStrategAll.tot <- merge(CumRetStrategAll.tot, CumRetStrateg2LR.tot[,c('Date','MV.clust')], all.x = TRUE, 
                           by.x = 'Date', by.y = 'Date')
colnames(CumRetStrategAll.tot) <- c('Date','MV','MV.SR','IdX','Investor','MV.LR')

CumRetStrategAll.tot <- merge(CumRetStrategAll.tot, CumRetStrateg2MR.tot[,c('Date','MV.clust')], all.x = TRUE, 
                              by.x = 'Date', by.y = 'Date')
colnames(CumRetStrategAll.tot) <- c('Date','MV','MV.SR','IdX','Investor','MV.LR','MV.MR')
CumRetStrategAll.tot <- merge(CumRetStrategAll.tot, CumRetStrateg2HR.tot[,c('Date','MV.clust')], all.x = TRUE, 
                              by.x = 'Date', by.y = 'Date')
colnames(CumRetStrategAll.tot) <- c('Date','MV','MV.SR','IdX','Investor','MV.LR','MV.MR','MV.HR')

CumRetStrategAll.tot <- CumRetStrategAll.tot[order(CumRetStrategAll.tot$Date),]
CumRetStrategAll.long <- CumRetStrategAll.tot %>%
  gather(key='variable',value = 'value',c(-Date,-Investor))

#By Strategy
p <- ggplot(CumRetStrategAll.long, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = variable), size=0.5) +
  stat_smooth(se=FALSE,aes(fill = variable, color = variable), size=0.5)
p + labs(title = 'Monthly Cumulative Returns: All strategy')


###########################################################################################
####                                 Tables                                      ##########
###########################################################################################

#Cumulative investor
write.table(InvestStrategAll.df,file = "Tables3/InvestStrategAll.csv" ,sep = ',')
InvestStrategAll.tab <- InvestStrategAll.df
names(InvestStrategAll.tab)[names(InvestStrategAll.tab) == "Date"] <- "Investor"

InvestStrategAll.tab$Investor <- seq(1:30)
InvestStrategAll.long <- InvestStrategAll.tab %>% 
  gather(key='variable',value = 'value',c(-Investor))

InvestStrategAll.xts <- as.xts(InvestStrategAll.df[,-1], InvestStrategAll.df[,1])
InvestStrategAll.Stat <- table.Stats(InvestStrategAll.xts)
InvestStrategAll.Stat.red <- InvestStrategAll.Stat[c('Median','Arithmetic Mean','LCL Mean', 'UCL Mean','Stdev'),]

InvestStrategAll.RetAcc <- InvestStrategAll.Stat.red[,c('MVRetAcc','IdXRetAcc','ClustRetAcc.SR',
                                                     'ClustRetAcc.LR','ClustRetAcc.MR','ClustRetAcc.HR')]
colnames(InvestStrategAll.RetAcc) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.RetAnn <- InvestStrategAll.Stat.red[,c('MVRetAnn','IdXRetAnn','ClustRetAnn.SR',
                                                        'ClustRetAnn.LR','ClustRetAnn.MR','ClustRetAnn.HR')]
colnames(InvestStrategAll.RetAnn) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.Sharpe <- InvestStrategAll.Stat.red[,c('MVSharpeR','IdXSharpeR','ClustSharpeR.SR',
                                                        'ClustSharpeR.LR','ClustSharpeR.MR','ClustSharpeR.HR')]
colnames(InvestStrategAll.Sharpe) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.VaR <- InvestStrategAll.Stat.red[,c('MVVaR','IdXVaR','ClustVaR.SR',
                                                        'ClustVaR.LR','ClustVaR.MR','ClustVaR.HR')]
colnames(InvestStrategAll.VaR) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.DD <- InvestStrategAll.Stat.red[,c('MVMaxDD','IdXMaxDD','ClustMaxDD.SR',
                                                     'ClustMaxDD.LR','ClustMaxDD.MR','ClustMaxDD.HR')]
colnames(InvestStrategAll.DD) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.Cal <- InvestStrategAll.Stat.red[,c('MVCalmar','IdXCalmar','ClustCalmar.SR',
                                                    'ClustCalmar.LR','ClustCalmar.MR','ClustCalmar.HR')]
colnames(InvestStrategAll.Cal) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.Ome <- InvestStrategAll.Stat.red[,c('MVOmega','IdXOmega','ClustOmega.SR',
                                                     'ClustOmega.LR','ClustOmega.MR','ClustOmega.HR')]
colnames(InvestStrategAll.Ome) <- c('MV','IdX','SR','LR','MR','HR')

InvestStrategAll.ETL <- InvestStrategAll.Stat.red[,c('MVETL','IdXETL','ClustETL.SR',
                                                     'ClustETL.LR','ClustETL.MR','ClustETL.HR')]
colnames(InvestStrategAll.ETL) <- c('MV','IdX','SR','LR','MR','HR')

xtable(InvestStrategAll.RetAcc,digits = 5)
xtable(InvestStrategAll.RetAnn,digits = 5)
xtable(InvestStrategAll.Sharpe,digits = 5)
xtable(InvestStrategAll.VaR,digits = 5)
xtable(InvestStrategAll.DD,digits = 5)
xtable(InvestStrategAll.Cal,digits = 5)
xtable(InvestStrategAll.Ome,digits = 5)
xtable(InvestStrategAll.ETL,digits = 5)

#Calmar and Omega ratios: Inf values
ind <-is.finite(InvestStrateg2HR.df$ClustCalmar.HR)
clustCalmar.HR <- InvestStrateg2HR.df$ClustCalmar.HR[ind]
Cal.HR <- stat.desc(clustCalmar.HR)

ind <-is.finite(InvestStrateg2HR.df$ClustOmega.HR)
clustOmega.HR <- InvestStrateg2HR.df$ClustOmega.HR[ind]
Ome.HR <- stat.desc(clustOmega.HR)

#Daily investor
write.table(StrategAll.df,file = "Tables3/StrategAll.csv" ,sep = ',')
xtable(table.Stats(tail(StrategAll.xts,-10)),digits = 5)

kk <- table.Stats(StrategAll.xts)

#Mean - StdDev
kkk <- kk[c('Stdev','Arithmetic Mean'),]
rownames(kkk) <- c('Stdev','Mean')
kkk <- t(kkk)
kkk <- data.frame(kkk)

ggplot(kkk, aes(x=Stdev, y=Mean)) + geom_point(size=1) +
  geom_text(label=rownames(kkk), size = 3) + ggtitle('Risk - Mean Daily Returns') +
  geom_smooth(method = lm, se=FALSE)


##########################################################################################
#####                              Plots                                ##################
##########################################################################################
###############################################
### All strategies ###########
###############################################


### All strategies: Monthly returns by portfolio-Day
## Way 1
#stratAll <- data.frame(InvestStrateg1.mtx[,c('SRetAcc','XRetAcc','BRetAcc')])
#strat1$Date <- NewInvestDays[1:(newinvdays-29)]

# StrategAll.long <- StrategAll.df %>%
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(StrategAll.long, aes(x = Date, y = value)) +
#   geom_line(aes(color = variable), size=0.5) +
#   stat_smooth(se=FALSE,aes(fill = variable, color = variable), size=0.5)
# 
# p + labs(title = '')

### For the daily investor
chart.CumReturns(StrategAll.xts, colorset=rich6equal, wealth.index = TRUE, geometric = FALSE, 
                 main = 'Daily Cumulative Returns: All strategies',legend.loc = 'left')
chart.Drawdown(StrategAll.xts, colorset=rich6equal, 
               main = 'Draw Down: All strategies', legend.loc = 'right')

StrategAll.long <- tail(StrategAll.df,-10) %>%
   gather(key='Model',value = 'Return',-Date)

StrategAll.long$Model <- factor(StrategAll.long$Model,c('MV','IdX','SR','LR','MR','HR'))

p<-ggplot(StrategAll.long, aes(x=Model, y=Return, color=Model)) + ylim(-1,4) +
  geom_boxplot()
p + stat_summary(fun=mean, geom="point", shape=23, size=4)

#chart.Boxplot(StrategAll.xts, main = 'Daily Return Distribution', legend.loc ='bottonright')



### FINANCIAL RATIOS BY INVESTOR ###############

### RetAcc
InvStr.RetAcc <- InvestStrategAll.df[,c('Date','MVRetAcc','IdXRetAcc','ClustRetAcc.SR',
                                        'ClustRetAcc.LR','ClustRetAcc.MR','ClustRetAcc.HR')]
colnames(InvStr.RetAcc) <- c('Date','MV','IdX','SR','LR',
                             'MR','HR')


InvStr.RetAcc.xts <-as.xts(InvStr.RetAcc[,-1],InvStr.RetAcc[,1])
chart.TimeSeries(InvStr.RetAcc.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: All strategies',
                 legend.loc = 'topleft')
chart.StackedBar(InvStr.RetAcc.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Returns: All strategies',
                 ylab = 'Returns',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')

InvStr.RetAcc.xts <-log10(as.xts(InvStr.RetAcc[,-1],InvStr.RetAcc[,1])) 
chart.TimeSeries(InvStr.RetAcc.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Log(Returns): All strategies',
                 legend.loc = 'topright')

chart.StackedBar(InvStr.RetAcc.xts, colorset = rich6equal,lwd = 2,
                 main = 'Total Cumulative Log(Returns): All strategies',
                 ylab = 'Log(Returns)',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')


# invStr.long <- InvStr.RetAcc %>%  
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(invStr.long, aes(x = Date, y = log10(value))) +
#   geom_line(aes(color = variable), size=0.5) + 
#   stat_smooth(se=FALSE,aes(fill = variable, color = variable), size=0.3)
# p + labs(title='Monthly Cumulative Log(Returns): All strategies')

### Sharpe Ratio
InvStr.SR <- InvestStrategAll.df[,c('Date','MVSharpeR','IdXSharpeR','ClustSharpeR.SR',
                                        'ClustSharpeR.LR','ClustSharpeR.MR','ClustSharpeR.HR')]
colnames(InvStr.SR) <- c('Date','MV','IdX','SR','LR',
                         'MR','HR')
# Idx <- InvStr.SR[InvStr.SR$Idx > 50,]
# Idx <- InvStr.SR[is.numeric(InvStr.SR$MV.HR)& InvStr.SR$MV.HR > 50,]
# rownames(Idx)
# InvStr.SR <- InvStr.SR[c(-795,-797,-801, -802),]

InvStr.SR.xts <-as.xts(InvStr.SR[,-1],InvStr.SR[,1])
chart.TimeSeries(InvStr.SR.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized Sharpe Ratio: All strategies',
                 legend.loc = 'bottomleft')

chart.StackedBar(InvStr.SR.xts, colorset = rich6equal,lwd = 2,
                 main = 'Annualized Sharped Ratio: All strategies',
                 ylab = 'Sharpe Ratio',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')

### DrawDown
InvStr.DD <- InvestStrategAll.df[,c('Date','MVMaxDD','IdXMaxDD','ClustMaxDD.SR',
                                    'ClustMaxDD.LR','ClustMaxDD.MR','ClustMaxDD.HR')]
colnames(InvStr.DD) <- c('Date','MV','IdX','SR','LR',
                         'MR','HR')

InvStr.DD.xts <- as.xts(InvStr.DD[,-1],InvStr.DD[,1])
chart.TimeSeries(-InvStr.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: All strategies',
                 legend.loc = 'topright')

chart.StackedBar(InvStr.DD.xts, colorset = rich6equal,lwd = 2,
                 main = 'Max. Draw Down: All strategies',
                 ylab = 'Max. Draw Down',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')

### Calmar ratio: we remove HR startegy
InvStr.Calmar <- InvestStrategAll.df[,c('Date','MVCalmar','IdXCalmar','ClustCalmar.SR',
                                    'ClustCalmar.LR','ClustCalmar.MR')]
colnames(InvStr.Calmar) <- c('Date','MV','IdX','SR','LR',
                         'MR')

InvStr.Calmar.xts <- as.xts(InvStr.Calmar[,-1],InvStr.Calmar[,1])
chart.TimeSeries(InvStr.Calmar.xts, colorset = rich6equal,lwd = 2,
                 main = 'Calmar ratio: All strategies (except HR)',
                 legend.loc = 'topright')
chart.StackedBar(InvStr.Calmar.xts, colorset = rich6equal,lwd = 2,
                 main = 'Calmar ratio: All strategies (except HR)',
                 ylab = 'Calmar ratio',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')

### VaR:
InvStr.VaR <- InvestStrategAll.df[,c('Date','MVVaR','IdXVaR','ClustVaR.SR',
                                        'ClustVaR.LR','ClustVaR.MR','ClustVaR.HR')]
colnames(InvStr.VaR) <- c('Date','MV','IdX','SR','LR',
                             'MR','HR')

InvStr.VaR.xts <- as.xts(InvStr.VaR[,-1],InvStr.VaR[,1])
chart.TimeSeries(InvStr.VaR.xts, colorset = rich6equal,lwd = 2,
                 main = 'VaR: All strategies',
                 legend.loc = 'topright')
chart.StackedBar(InvStr.VaR.xts, colorset = rich6equal,lwd = 2,
                 main = 'VaR: All strategies',
                 ylab = 'VaR',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')
### ETL:
InvStr.ETL <- InvestStrategAll.df[,c('Date','MVETL','IdXETL','ClustETL.SR',
                                     'ClustETL.LR','ClustETL.MR', 'ClustETL.HR')]
colnames(InvStr.ETL) <- c('Date','MV','IdX','SR','LR',
                          'MR','HR')

InvStr.ETL.xts <- as.xts(InvStr.ETL[,-1],InvStr.ETL[,1])
chart.TimeSeries(InvStr.ETL.xts, colorset = rich6equal,lwd = 2,
                 main = 'ETL: All strategies',
                 legend.loc = 'topright')
chart.StackedBar(InvStr.ETL.xts, colorset = rich6equal,lwd = 2,
                 main = 'ETL: All strategies',
                 ylab = 'ETL',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')

### Omega: We remove HR
InvStr.Omega <- InvestStrategAll.df[,c('Date','MVOmega','IdXOmega','ClustOmega.SR',
                                     'ClustOmega.LR','ClustOmega.MR')]
colnames(InvStr.Omega) <- c('Date','MV','IdX','SR','LR',
                          'MR')

InvStr.Omega.xts <- as.xts(InvStr.Omega[,-1],InvStr.Omega[,1])
chart.TimeSeries(InvStr.Omega.xts, colorset = rich6equal,lwd = 2,
                 main = 'Omega ratio: All strategies (except HR)',
                 legend.loc = 'topright')
chart.StackedBar(InvStr.Omega.xts, colorset = rich6equal,lwd = 2,
                 main = 'Omega ratio: All strategies (except HR)',
                 ylab = 'Omega ratio',xlab = 'Investor',xaxis = FALSE,
                 legend.loc = 'under')
# invStr.long <- InvStr.DD %>%  
#   gather(key='variable',value = 'value',-Date)
# 
# 
# p <- ggplot(invStr.long, aes(x = Date, y = value)) +
#   geom_line(aes(color = variable), size=0.5)
# p + labs(title = 'Max. Draw Down of Monthly Cumulative Portfolios: All strategies')


###############################################
### Daily portfolio                 ###########
###############################################
# Add regression lines
# ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
#   geom_point() + 
#   geom_smooth(method=lm)
# # Remove confidence intervals
# # Extend the regression lines
# ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
#   geom_point() + 
#   geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
# 
# gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)
# 
# InvestStrategAll.long <- InvestStrategAll.df[,c(-1,2,3,4,] %>%
#   gather(condition, measure, )

# Cardinality

aaa <- CardAll.df
colnames(aaa) <- c('Date','MV','SR','LR','MR','HR')
CardAll.long <- aaa %>%  
  gather(key='Model',value = 'Cardinality',-Date)

CardAll.long$Model <- factor(CardAll.long$Model, c('MV','SR','LR','MR','HR'))

p <- ggplot(CardAll.long, aes(x = Date, y = Cardinality)) +
  geom_line(aes(color = Model), size=0.5) + 
  stat_smooth(se=FALSE,aes(fill = Cardinality, color = Model), size=0.3)
p + labs(title='Portfolio cardinality: All strategies')

p <- ggplot(CardAll.long, aes(x = Date, y = Cardinality)) +
  geom_line(aes(color = Model), size=0.5) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2),aes(fill = Model, color = Model), size=0.3)
p + labs(title='Portfolio cardinality: All strategies')


#Compute correlations
cor(Strateg1.df$cardGP.clust,Strateg1.df$card.clust,method = c("pearson"))
cor.test(Strateg1.df$cardGP.clust,Strateg1.df$card.clust,method = c("pearson"))

cor(Strateg2LR.df$cardGP.clust,Strateg2LR.df$card.clust,method = c("pearson"))
cor.test(Strateg2LR.df$cardGP.clust,Strateg2LR.df$card.clust,method = c("pearson"))

cor(Strateg2MR.df$cardGP.clust,Strateg2MR.df$card.clust,method = c("pearson"))
cor.test(Strateg2MR.df$cardGP.clust,Strateg2MR.df$card.clust,method = c("pearson"))

cor(Strateg2HR.df$cardGP.clust,Strateg2HR.df$card.clust,method = c("pearson"))
cor.test(Strateg2HR.df$cardGP.clust,Strateg2HR.df$card.clust,method = c("pearson"))
aaa <- (Strateg2HR.df$cardGP.clust != 0)
cor(Strateg2HR.df$cardGP.clust[aaa],Strateg2HR.df$card.clust[aaa],method = c("pearson"))
cor.test(Strateg2HR.df$cardGP.clust[aaa],Strateg2HR.df$card.clust[aaa],method = c("pearson"))

#cor(x, y, method = c("pearson", "kendall", "spearman"))
#cor.test(x, y, method=c("pearson", "kendall", "spearman"))

###############################################
### One portfolio detail            ###########
###############################################
## February 2019
StrategDetail <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',]
StrategDetail.xts <-as.xts(StrategDetail[,-1],StrategDetail[,1])
chart.TimeSeries(StrategDetail.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Return Portfolio (detail of February 2019)',
                 legend.loc = 'topleft')


# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(StrategAll.long, aes(x = Date, y = value)) +
#   geom_line(aes(color = variable), size=0.5)
# 
# p + labs(title='Daily Return Portfolio (detail of February 2019)')

portfolio.sample <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] 
portfolio.sample.xts <- as.xts(portfolio.sample[,-1],portfolio.sample[,1])
chart.CumReturns(portfolio.sample.xts[,c(1:5)])
Return.cumulative(portfolio.sample.xts[,c(1:5)])
(1+0.02659208)*(1+0.04189696)*(1+0.02170318)*(1+0.03578174)*(1+0.03042784)*(1+0.04245241)*(1+0.04744380)-1


## April 2020
StrategDetail <- StrategAll.df[StrategAll.df$Date>='2020-04-08'&StrategAll.df$Date<='2020-04-14',]
StrategDetail.xts <-as.xts(StrategDetail[,-1],StrategDetail[,1])
chart.TimeSeries(StrategDetail.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Return Portfolio (detail of April 2020)',
                 legend.loc = 'topleft')

# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2020-04-08'&StrategAll.df$Date<='2020-04-14',] %>% 
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(StrategAll.long, aes(x = Date, y = value)) +
#   geom_line(aes(color = variable), size=0.5)
# 
# p + labs(title='Daily Return Portfolio (detail of April 2020)')

portfolio.sample <- StrategAll.df[StrategAll.df$Date>='2020-04-08'& StrategAll.df$Date<='2020-04-14',]



## January 2021
StrategDetail <- StrategAll.df[StrategAll.df$Date>='2021-01-01'&StrategAll.df$Date<='2021-01-07',]
StrategDetail.xts <-as.xts(StrategDetail[,-1],StrategDetail[,1])
chart.TimeSeries(StrategDetail.xts, colorset = rich6equal,lwd = 2,
                 main = 'Daily Return Portfolio (detail of January 2021)',
                 legend.loc = 'topleft')

# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2021-01-01'&StrategAll.df$Date<='2021-01-07',] %>% 
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(StrategAll.long, aes(x = Date, y = value)) +
#   geom_line(aes(color = variable), size=0.5)
# 
# p + labs(title='Daily Return Portfolio (detail of January 2021)')

portfolio.sample <- StrategAll.df[StrategAll.df$Date=='2021-01-04',] 

##########################################
## Density plots                        ##
##########################################

# StrategAll.long <- StrategAll.df %>%
#   gather(key='variable',value = 'value',-Date)
# p <- ggplot(StrategAll.long, aes(x = value, color = variable)) +
#   geom_density()
# p + labs(title = 'Daily Return Portfolio: density plot')
# 

## Detail
StrategAll.long <- StrategAll.df %>%
  gather(key='variable',value = 'value',-Date)
StrategAll.long <- StrategAll.long[StrategAll.long$value<1.5,]
p <- ggplot(StrategAll.long, aes(x = value, color = variable)) +
  geom_density()
p + labs(title = 'Density of Daily Returns: All strategies')

# Strategies
#retGP.mtx.SR <- retGP.clust.mtx.red
nfich1 <- c("Datasets/Strategiesmin13TC3.RData")
# save(StrategAll.df,StrategAll.xts,
#      InvestStrateg1.df, InvestStrateg2LR.df, InvestStrateg2MR.df, InvestStrateg2HR.df,
#      InvestStrategAll.df,
#      retGP.mtx.MV, retGP.mtx.IdX,retGP.mtx.SR, retGP.mtx.LR, retGP.mtx.MR, retGP.mtx.HR,
#      retCUM.mtx.MV, retCUM.mtx.IdX,retCUM.mtx.SR, retCUM.mtx.LR, retCUM.mtx.MR, retCUM.mtx.HR,
#     file=nfich1)

