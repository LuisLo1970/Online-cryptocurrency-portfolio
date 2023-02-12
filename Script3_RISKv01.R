#Dataset
nfich1 <- c("Datasets/CRYPT_DS_TOT3.RData")
#nfich1 <- c("Datasets/CRYPT_DS_TOT4.RData") #When Vol=0
load(nfich1)

dates<-unique(CRYP.RT$time)
dates <- sort(dates)

#Training window parameters
initWidth <- 365 #Initial Training window width is one year and increasing one day per day up to finalWidth param
finalWidth<- 730 #two years


#Testing window parameters
TestWidth <- 30  #Out-of-sample period (i month)

#nclust=14
i <- c(1)
dateTestEnd <- dates[initWidth + 30]
#risk.lst <- list() 
RiskQ.tab<-c()
as.data.frame(RiskQ.tab)
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
  
  #Volatility percentiles
  CRYPRisk.Rt <- TrainWindow[,.(volat=sd(Rt)), by=SYM]
  Risk.quantil<- summary(CRYPRisk.Rt$volat)
  
  #Risk.quantil<- list(dateTrainEnd = dateTrainEnd, RiskQuant = Risk.quantil)
  
  df <- data.frame(dateTrainEnd=dateTrainEnd, 
                   quart1st=as.numeric(Risk.quantil[2]), quart2nd=as.numeric(Risk.quantil[3]), 
                   quart3rd=as.numeric(Risk.quantil[4]))
  RiskQ.tab <- rbind(RiskQ.tab, df)
  
  #risk.lst <- append(risk.lst, Risk.quantil)
  
    #portfAcumRet.lst <- append(portfAcumRet.lst,list(portfAll[[9]], portfClus[[9]]))
  
  if (initWidth < finalWidth){
    initWidth = initWidth + 1 #Increasing training window width 
  } else {i <- i + 1}
}


# Dataset 2018-21
nfich1 <- c("Datasets/TrainingRisk.RData")
#nfich1 <- c("Datasets/TrainingRisk2.RData") #When Vol=0
save(RiskQ.tab,
     file=nfich1)
