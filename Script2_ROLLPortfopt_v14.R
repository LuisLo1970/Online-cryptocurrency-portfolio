##############################################################################################
#### CLUSTERING MODELs (K-Means, Hist DAWass, TADpole) #######################################
##############################################################################################
#####               Training along 2018, clustering rolling window                        ####
##############################################################################################               
#We take acocunt Transaction Costs from V14
#Sampling and Optimization techniques
#From version05 we considered hier market cap cryptoassets
#We consider all partictions (no only those that beat the standard one)
#Libraires
library(data.table)
library(reshape2)
library(xtable)
library(xts)

#Clustering libraries
library(cluster)
library(factoextra)
library(NbClust)
library(ggplot2)
library(FactoMineR)

library(quadprog)
library(MASS) #Robust cov estimation
library(SharpeR)
library(PerformanceAnalytics)

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/FasePortOpt/')
source('CryptFunctPortf.R')

#Dataset
nfich1 <- c("Datasets/CRYPT_DS_TOT3.RData")
load(nfich1)

dates<-unique(CRYP.RT$time)
dates <- sort(dates)

#Training window parameters
initWidth <- 365 #Initial Training window width is one year and increasing one day per day up to finalWidth param
finalWidth<- 730 #two years

#Transaction Costs
TC <- 0.0005 #5 BPS

#Testing window parameters
TestWidth <- 30  #Out-of-sample period (i month)
nitermax=50
#nclust=14
i <- c(1)
dateTestEnd <- dates[initWidth + 30]
portf.lst <- list()
#portfAcumRet.lst <- list()
while (dateTestEnd != last(dates)){
  dateTrainStart<- dates[i]  #Starting date of Training window
  dateTrainEnd  <- dates[i + initWidth - 1]
  dateTestStart <- dates[i + initWidth]
  dateTestEnd   <- dates[i + initWidth + 30] 
  
  print(dateTestEnd)
  
  AllWindow <- CRYP.RT[CRYP.RT$time>=dateTrainStart & CRYP.RT$time<=dateTestEnd]
  AllWindow.wDT.Rt <- dcast.data.table(AllWindow, time ~ SYM, value.var ="Rt")
  kk<- AllWindow.wDT.Rt[ ,colSums(is.na(AllWindow.wDT.Rt)) == 0] #We remove columns with at least one NA
  AllWindow.wDT.Rt <- subset(AllWindow.wDT.Rt,select = kk)
  AllWindow <- AllWindow[AllWindow$SYM %in% names(AllWindow.wDT.Rt)]
  
  TrainWindow <- AllWindow[AllWindow$time <= dateTrainEnd]
  TrainCRYP.wDT.Rt <- AllWindow.wDT.Rt[AllWindow.wDT.Rt$time <= dateTrainEnd]
  TrainCRYP.wDT.Rt <- as.data.frame(TrainCRYP.wDT.Rt)
  TrainDates <- as.Date(TrainCRYP.wDT.Rt$time)
  
  #TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,-c('time')]
  
  TestWindow <- AllWindow[AllWindow$time >=dateTestStart & AllWindow$time<=dateTestEnd]
  TestCRYP.wDT.Rt <- AllWindow.wDT.Rt[AllWindow.wDT.Rt$time >=dateTestStart & AllWindow.wDT.Rt$time<=dateTestEnd]
  TestCRYP.wDT.Rt <- as.data.frame(TestCRYP.wDT.Rt)
  TestDates <-as.Date(TestCRYP.wDT.Rt$time)
  
  #TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,-c('time')]
  
  ## Risk-free interest rate ##################################################
  DTB90Train<- as.data.frame(DTB90.DT[DTB90.DT$Date>=dateTrainStart & DTB90.DT$Date<=dateTrainEnd, c('Date', 'dTB')])
  DTB90Test <- as.data.frame(DTB90.DT[DTB90.DT$Date>=dateTestStart & DTB90.DT$Date<=dateTestEnd, c('Date', 'dTB')] )
  
  DTB90Train.ext <- merge(TrainCRYP.wDT.Rt[,c(1,2)],DTB90Train, by.x = 'time', by.y = 'Date',all.x = TRUE)
  DTB90Test.ext <- merge(TestCRYP.wDT.Rt[,c(1,2)],DTB90Test,by.x = 'time', by.y = 'Date',all.x = TRUE)
  DTB90Train.ext$dTB <- na.spline(DTB90Train.ext$dTB)
  DTB90Test.ext$dTB <- na.spline(DTB90Test.ext$dTB)
  DTB90Train.ext <- DTB90Train.ext[,c(-2)]
  DTB90Test.ext <- DTB90Test.ext[,c(-2)]
  ## Adding Risk-free column
  #TrainCRYP.wDT.Rt <- merge(TrainCRYP.wDT.Rt, DTB90Train, by.x = 'time', by.y = 'Date',all.x = TRUE)
  #TestCRYP.wDT.Rt <- merge(TestCRYP.wDT.Rt, DTB90Test, by.x = 'time', by.y = 'Date',all.x = TRUE)
  
  #TrainCRYP.mtx <- as.matrix(TrainCRYP.wDT.Rt)
  #TestCRYP.mtx <- as.matrix(TestCRYP.wDT.Rt)
  
  ### Imputation for missing 90 days treasury bill values (interpolation)
  #library(zoo)
  #df1$price <- na.locf(df1$price)
  #TrainCRYP.wDT.Rt$dTB <- na.spline(TrainCRYP.wDT.Rt$dTB)
  #TestCRYP.wDT.Rt$dTB <- na.spline(TestCRYP.wDT.Rt$dTB)
  
  ## Portfolio CCI30 ########################################################
  CCI30Train <- as.matrix(CCI30Index.DT[CCI30Index.DT$time>=dateTrainStart & CCI30Index.DT$time<=dateTrainEnd,RtCCI30])
  CCI30Test <- as.matrix(CCI30Index.DT[CCI30Index.DT$time>=dateTestStart & CCI30Index.DT$time<=dateTestEnd,RtCCI30])
  benchTrain <- pftPerf(CCI30Train,1)
  benchTest <- pftPerf(CCI30Test,1)
  
  ## Portfolio Optimiz #########################################################
  #portfAll <- funcOptPortf(TrainCRYP.mtx,TestCRYP.mtx)
  TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,c(-1)]
  TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,c(-1)]
  #portfAll <- funcOptPortf21(TrainCRYP.wDT.Rt,TestCRYP.wDT.Rt, DTB90Train.ext, DTB90Test.ext, TC)
  portfAll <- funcOptPortf21(TrainCRYP.wDT.Rt,TestCRYP.wDT.Rt, DTB90Train.ext, DTB90Test.ext, BaseInv = 1.0, 
                             TC = 0.0005)
  
  #portfClus <- funcCryptSpaceAll(TrainWindow, TrainCRYP.mtx, TestCRYP.mtx, nitermax)
  # portfClus <- funcCryptSpaceAll21(TrainWindow, TrainCRYP.wDT.Rt, TestCRYP.wDT.Rt, 
  #                                DTB90Train.ext, DTB90Test.ext, nitermax, TC)
  # 
  portfClus <- funcCryptSpaceAll21(TrainWindow, TrainCRYP.wDT.Rt, TestCRYP.wDT.Rt, 
                                   DTB90Train.ext, DTB90Test.ext, LDAG.all=0, 
                                   nitermax=50, samps=100, mincard = 30, TC = 0.0005)
  
  portf.lst <- append(portf.lst,list(dateTestStart=dateTestStart, dateTestEnd=dateTestEnd, 
                                     dateTrainStrat=dateTrainStart, dateTrainEnd=dateTrainEnd,
                                     list(portAll=portfAll, portfClus=portfClus, 
                                     becnhTrain=last(benchTrain), benchTest=last(benchTest))))
  #portfAcumRet.lst <- append(portfAcumRet.lst,list(portfAll[[9]], portfClus[[9]]))
  
  if (initWidth < finalWidth){
    initWidth = initWidth + 1 #Increasing training window width 
  } else {i <- i + 1}
}


# Dataset 2018-21
nfich1 <- c("Datasets/CRYPT_RollPortfMinTC3.RData")
# save(portf.lst,
#      file=nfich1)
