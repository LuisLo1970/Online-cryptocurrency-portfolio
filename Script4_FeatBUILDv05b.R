########################################################
### Feature builder for predictive models ###############
########################################################

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/FasePortOpt/')
source('CryptFunctPortf.R')


# Dataset 2018-21
nfich1 <- c("Datasets/CRYPT_RollPortfMinTC3.RData")
load(nfich1)

getwd()

#sink("Printouts/portflst21.txt")

k=20 #k=769 --> i=3846 /// k=642 --> k maximum values is 849
k=770#"2021-02-09" accumGainTest 94077.86; Portfolio={"FUEL","GSE"}; primer registro con accumGain disparado; GSE --> Eliminar
k=830
k=0
i = 1 + 5*k #i maximum value is 4246 --> "2021-05-01" 

sink("Printouts/portflstTest2.txt")
portf.lst[[i]]
portf.lst[[i+1]]
portf.lst[[i+2]]
portf.lst[[i+3]]
portf.lst[[i+4]]
sink()

portf.lst[[i+6]]


#i=3841
#i=3846
#i=3211 First NULL: k=642 (2020-10-04)
portf.lst[[i]]
portf.lst[[i+4]]$portAll[[2]] #NULL with k=642 then i=3211 (2020-10-04)
portf.lst[[i+4]]$portAll$weightPortf
portf.lst[[i+4]]$portAll$accumGainTest
portf.lst[[i+4]]$portAll$QPSolVal
portf.lst[[3846+4]]$portAll[[1]]
portf.lst[[4256]]
portf.lst[[5]]$portfClus[[7]]

#sink()

sink("Printouts/portNULLs.txt")
for(k in 1:849){
  i=1+5*k
  print(i)
  print(portf.lst[[i+4]]$portAll[[2]])
  }
sink()

sink("Printouts/portflst1.txt")
portf.lst[[1]]
portf.lst[[2]]
portf.lst[[3]]
portf.lst[[4]]
portf.lst[[5]]
sink()

portf.lst[[6]]

length(portf.lst)

#Field description of portf.lst[[]] 4260 object-list ####
# portf.lst[[1]]         dateTestStart
# portf.lst[[2]]         dateTestEnd
# portf.lst[[3]]         dateTrainStart
# portf.lst[[4]]         dateTrainEnd
# portf.lst[[5]]$portAll$QPSolVal     Optimal value for the optimal solution
# portf.lst[[5]]$portAll[[2]]         Portfolio for the complete space
# portf.lst[[5]]$portAll$weightPortf  Portfolio allocation weights for the complete space
# portf.lst[[5]]$portAll$WBaseInv     Weighted Base Investment: Part of the budget actually invested 

# Centroids computed during Test period on weighted Portfolios taken the whole data space
# portf.lst[[5]]$portAll$portfTestCentroid[1] Portfolio Test centroid for the complete space --> Volatility dim
# portf.lst[[5]]$portAll$portfTestCentroid[2] Portfolio Test centroid for the complete space --> Mean Ret. value dim

# portf.lst[[5]]$portAll$portfTest    Daily accumulated returns in Test Period for the complete space
# portf.lst[[5]]$portAll$accumGainTest Accumulated return at the end of Test Period for the complete space
# portf.lst[[5]]$portAll$retGP        Return Gain Test-Portfolio

# Centroids computed during Train period on weighted Portfolios taken the whole data space
# portf.lst[[5]]$portAll$portfTrainCentroid[1] Portfolio Train Centroid for the complete space--> Volatility dim
# portf.lst[[5]]$portAll$portfTrainCentroid[2] Portfolio Train Centroid for the complete space--> Mean Ret. dim

# portf.lst[[5]]$portAll$accumGainTrain Accumulated return at the end of the Training Period for the complete space
# portf.lst[[5]]$portAll$retGPTrain        Return Gain Train-Portfolio

# portf.lst[[5]]$portAll$SRTrain  Sharpe Ratio for the Train for complete space
### portf.lst[[5]]$portAll$SRInfoTrain  Sharpe Info Ratio for the Train for complete space
# portf.lst[[5]]$portAll$SRTest  Sharpe Ratio for the Test for complete space
### portf.lst[[5]]$portAll$SRInfoTest  Sharpe Info Ratio for the Test for complete space


# portf.lst[[5]]$portfClus$TrainMedoid[1] Medoid (crypto) of the Training data set for the complete space --> Volatility dim.
# portf.lst[[5]]$portfClus$TrainMedoid[2] Medoid (crypto) of the Training data set for the complete space --> Return dim.

# portf.lst[[5]]$portfClus$cluster  Cluster (k)
# portf.lst[[5]]$portfClus[[3]]$nitermax
# portf.lst[[5]]$portfClus[[3]]$sampsize
# portf.lst[[5]]$portfClus[[3]]$medoid[1] RtVol:Volatility of Cluster-Medoid (k)
# portf.lst[[5]]$portfClus[[3]]$medoid[2] RtMean: Mean Ret. of Cluster-Medoid (k)
# portf.lst[[5]]$portfClus[[3]]$portfCluster$QPSolVal Opt. value for the Opt. Sol on Training
# portf.lst[[5]]$portfClus[[3]]$portfCluster[[2]]         List of crypto if the Cluster-portfolio
# portf.lst[[5]]$portfClus[[3]]$portfCluster$weightPortf  Weights of the cluster-portfolio
# portf.lst[[5]]$portfClus[[3]]$portfCluster$WBaseInv  Weighted Base Investment: Part of the budget actually invested   
# portf.lst[[5]]$portfClus[[3]]$portfCluster$portfTestCentroid[1] vo:Test-Volatility of Cluster-Centroid (k)
# portf.lst[[5]]$portfClus[[3]]$portfCluster$portfTestCentroid[2] mv:Test-Mean Ret. of Cluster-Centroid (k)
# portf.lst[[5]]$portfClus[[3]]$portfCluster$accumGainTest
# portf.lst[[5]]$portfClus[[3]]$portfCluster$retGP  Return Gain cluster Test-Potfolio
# portf.lst[[5]]$portfClus[[3]]$portfCluster$portfTrainCentroid[1] vo:Centroid-Volatility of Training
# portf.lst[[5]]$portfClus[[3]]$portfCluster$portfTrainCentroid[2] mv:Centroid-Mean Ret. of Training
# portf.lst[[5]]$portfClus[[3]]$portfCluster$accumGainTrain Accumulated return during training
# portf.lst[[5]]$portfClus[[3]]$portfCluster$retGPTrain Return Gain Cluster Train-Portfolio
# portf.lst[[5]]$portfClus[[3]]$portfCluster$SRTrain
### portf.lst[[5]]$portfClus[[3]]$portfCluster$SRInfoTrain
# portf.lst[[5]]$portfClus[[3]]$portfCluster$SRTest
### portf.lst[[5]]$portfClus[[3]]$portfCluster$SRInfoTest

# portf.lst[[6]] dateTestStart (2nd time-frame)

# portf.lst[[4251]] Last    dateTestStart (2021-04-30)
# portf.lst[[4252]]         dateTestEnd
# portf.lst[[4253]]         dateTrainStart
# portf.lst[[4254]]         dateTrainEnd

#Total 4260 object-list
# portf.lst[[1]]
# portf.lst[[6]]
# portf.lst[[4251]]
# 
# length(portf.lst[[5]]$portfClus) #275 obj
# portf.lst[[5]]$portfClus[[2]] #Cluster 1
# portf.lst[[5]]$portfClus[[4]] #Cluster 3
##################################################################
Feat.tab<-c()
as.data.frame(Feat.tab)
portf.legth <- length(portf.lst)
i <- 1
while (i < portf.legth - 8){
  dateTestStart <- portf.lst[[i]]       #dateTestStart
  dateTestEnd <- portf.lst[[i+1]]         #dateTestEnd
  dateTrainStart <- portf.lst[[i+2]]         #dateTrainStart
  dateTrainEnd <- portf.lst[[i+3]]         #dateTrainEnd
  QPSol <- portf.lst[[i+4]]$portAll$QPSolVal     #Optimal value for the optimal solution
  
  #Gain
  WBaseInv <- portf.lst[[i+4]]$portAll$WBaseInv  #Weighted Base Investion
  accumGainTest <- portf.lst[[i+4]]$portAll$accumGainTest #Accumulated return at the end of Test Period for the complete space
  retGP <- portf.lst[[i+4]]$portAll$retGP  #Return Gain of Test-Portfolio
  cardGP <- length(portf.lst[[i+4]]$portAll$weightPortf) #NUmber of allocated cryptocurrencies
  
  #Medoids computed on Train Period
  RtVolTrain.med <- portf.lst[[i+4]]$portfClus$TrainMedoid[[1]] #Volatility Train-Medoid of Cluster (k)
  RtMeanTrain.med <- portf.lst[[i+4]]$portfClus$TrainMedoid[[2]] #Mean Ret. Train-Medoid of Cluster (k)
  
  #Centroids computed during Test period on weighted Portfolios taken the whole data space
  portfTestCentrVol <- portf.lst[[i+4]]$portAll$portfTestCentroid[1] #Portfolio Test centroid for the complete space --> Volatility dim
  portfTestCentrRet <- portf.lst[[i+4]]$portAll$portfTestCentroid[2] #Portfolio Test centroid for the complete space --> Mean Ret. value dim

  #Centroids computed during Train period on weighted Portfolios taken the whole data space
  portfTrainCentrVol <- portf.lst[[i+4]]$portAll$portfTrainCentroid[1] #Portfolio Train Centroid for the complete space--> Volatility dim
  portfTrainCentrRet <- portf.lst[[i+4]]$portAll$portfTrainCentroid[2] #Portfolio Train Centroid for the complete space--> Mean Ret. dim

  accumGainTrain <- portf.lst[[i+4]]$portAll$accumGainTrain #Accumulated return at the end of the Training Period for the complete space
  retGPTrain <- portf.lst[[i+4]]$portAll$retGPTrain  #Return Gain of Train-Portfolio
  
  SRTrain <- portf.lst[[i+4]]$portAll$SRTrain  #Sharpe Ratio for the Train for complete space
  #SRInfoTrain <- portf.lst[[i+4]]$portAll$SRInfoTrain  #Sharpe Info Ratio for the Train for complete space
  SRTest <- portf.lst[[i+4]]$portAll$SRTest  #Sharpe Ratio for the Train for complete space
  #SRInfoTest <- portf.lst[[i+4]]$portAll$SRInfoTest  #Sharpe Info Ratio for the Train for complete space
  
  
  clust.iter <- length(portf.lst[[i+4]]$portfClus)
  j <- 2
  while (j < clust.iter - 1){
    clust <- portf.lst[[i+4]]$portfClus[[j]]  #Cluster (k)
    card.clust <- length(portf.lst[[i+4]]$portfClus[[j+1]]$cryptos) #New
    nitermax.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$nitermax
    sampsize.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$sampsize
    RtVolTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$medoid[[1]] #Volatility of Cluster (k)
    RtMeanTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$medoid[[2]] #Mean Ret. of Cluster (k)
    QPSolVal.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$QPSolVal #Opt. value for the Opt. Sol on Training
    
    #Gain
    WBaseInv.clust <-  portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$WBaseInv
    accumGainTest.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$accumGainTest
    retGP.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$retGP
    cardGP.clust <- length(portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$weightPortf) #NUmber of allocated cryptocurrencies
    
    #Cluster-Centroids computed during Test period on weighted Portfolios taken the CLUSTER data space
    portfTestCentroVol.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$portfTestCentroid[1] #Centroid-Volatility on Test
    portfTestCentroRet.clust <-  portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$portfTestCentroid[2]#Centroid-Mean Ret. on Test
    
    #Cluster-Centroids computed during Train period on weighted Portfolios taken the CLUSTER data space
    portfTrainCentroVol.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$portfTrainCentroid[1] #Centroid-Volatility of Training
    portfTrainCentroRet.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$portfTrainCentroid[2] #Centroid-Mean Ret. of Training
    accumGainTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$accumGainTrain #Accumulated return during training
    retGPTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$retGPTrain #Return Gain of Cluster Train-Portfolio 
    
    SRTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$SRTrain
    #SRInfoTrain.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$SRInfoTrain
    SRTest.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$SRTest
    #SRInfoTest.clust <- portf.lst[[i+4]]$portfClus[[j+1]]$portfCluster$SRInfoTest
    
    benchTrain <- portf.lst[[i+4]]$becnhTrain
    benchTest <- portf.lst[[i+4]]$benchTest
    
    df <- data.frame(dateTestStart=dateTestStart,
                     dateTestEnd=dateTestEnd,
                     dateTrainStart=dateTrainStart,
                     dateTrainEnd=dateTrainEnd,
                     QPSol=QPSol,
                     WBaseInv=WBaseInv,
                     accumGainTest=accumGainTest,
                     retGP=retGP,
                     cardGP=cardGP,
                     RtVolTrain.med = RtVolTrain.med,
                     RtMeanTrain.med=RtMeanTrain.med,
                     portfTestCentrVol=portfTestCentrVol,
                     portfTestCentrRet=portfTestCentrRet,
                     portfTrainCentrVol=portfTrainCentrVol,
                     portfTrainCentrRet=portfTrainCentrRet,
                     accumGainTrain=accumGainTrain,
                     retGPTrain=retGPTrain,
                     SRTrain=SRTrain,
                     #SRInfoTrain=SRInfoTrain,
                     SRTest=SRTest,
                     #SRInfoTest=SRInfoTest,
                     clust=clust,
                     card.clust=card.clust,
                     RtVolTrain.clust=RtVolTrain.clust,
                     RtMeanTrain.clust=RtMeanTrain.clust,
                     nitermax.clust=nitermax.clust, 
                     sampsize.clust=sampsize.clust, 
                     QPSolVal.clust=QPSolVal.clust,
                     WBaseInv.clust=WBaseInv.clust,
                     accumGainTest.clust=accumGainTest.clust,
                     retGP.clust=retGP.clust,
                     cardGP.clust=cardGP.clust,
                     portfTestCentroVol.clust=portfTestCentroVol.clust,
                     portfTestCentroRet.clust=portfTestCentroRet.clust, 
                     portfTrainCentroVol.clust=portfTrainCentroVol.clust, 
                     portfTrainCentroRet.clust=portfTrainCentroRet.clust,
                     accumGainTrain.clust=accumGainTrain.clust,
                     retGPTrain.clust=retGPTrain.clust,
                     SRTrain.clust=SRTrain.clust, 
                     #SRInfoTrain.clust=SRInfoTrain.clust,
                     SRTest.clust=SRTest.clust,
                     #SRInfoTest.clust=SRInfoTest.clust,
                     benchTrain =benchTrain,
                     benchTest=benchTest)
    
    Feat.tab <- rbind(Feat.tab,df)
    
    j <- j + 2
  }
  
  i <- i + 5
}

str(portf.lst)
length(portf.lst) #4260 objects

## Only Returns ###
RetSR.tab <- Feat.tab[,c('dateTestEnd','retGP','retGP.clust','SRTest','SRTest.clust')]

#Dataset #########################################
nfich1 <- c("Datasets/CRYPT_DS_TOT3.RData")
load(nfich1)
##################################################


## Portfolio details: dateTestEnd = 2021-02-04
Feat.sample <- Feat.tab[Feat.tab$dateTestEnd=='2021-01-04',]
k=704 #--> index associated to the DateTestEnd = 2021-01-04
i = 1 + 5*k #i maximum value is 4246 --> "2021-05-01" 
sink("Printouts/portf20210104.txt")
portf.lst[[i]]
portf.lst[[i+1]]
portf.lst[[i+2]]
portf.lst[[i+3]]
portf.lst[[i+4]]
sink()

#Whole market
portf.lst[[i+4]]$portAll[[2]] #Portfolio for the complete space
portf.lst[[i+4]]$portAll$weightPortf  #Portfolio allocation weights for the complete space
portf.lst[[i+4]]$portAll$WBaseInv     #Weighted Bas
portf.lst[[i+4]]$portAll$retGP#Return gain Test-Portfolio

#### Other case
k=20
i = 1 + 5*k #i maximum value is 4246 --> "2019-02-20" 
sink("Printouts/portf20190220.txt")
portf.lst[[i]]
portf.lst[[i+1]]
portf.lst[[i+2]]
portf.lst[[i+3]]
portf.lst[[i+4]]
sink()

portf.lst[[i+4]]$portAll[[2]] #Portfolio for the complete space
portf.lst[[i+4]]$portAll$weightPortf  #Portfolio allocation weights for the complete space
portf.lst[[i+4]]$portAll$WBaseInv     #Weighted Bas
portf.lst[[i+4]]$portAll$retGP#Return gain Test-Portfolio

OneDay <- list(date = portf.lst[[i+1]], portf = portf.lst[[i+4]]$portAll[[2]], 
                 portfWeights=portf.lst[[i+4]]$portAll$weightPortf,
                 crypWeight=portf.lst[[i+4]]$portAll$WBaseInv, retGain=portf.lst[[i+4]]$portAll$retGP)

#We choose one day more (2019-02-16) to have a complete week of returns
OneWeek <- CRYP.RT[CRYP.RT$SYM %in% OneDay$portf & CRYP.RT$time>='2019-02-16' & CRYP.RT$time<='2019-02-23',]
OneWeek.wide <- dcast.data.table(OneWeek, time ~ SYM, value.var = "close")
Return.calculate(OneWeek.wide)
matplot(OneWeek.wide, type = 'l')


#### Other case
k=436
i = 1 + 5*k #i maximum value is 4246 --> "2020-04-11" 
sink("Printouts/portf20200411.txt")
portf.lst[[i]]
portf.lst[[i+1]]
portf.lst[[i+2]]
portf.lst[[i+3]]
portf.lst[[i+4]]
sink()

portf.lst[[i+4]]$portAll[[2]] #Portfolio for the complete space
portf.lst[[i+4]]$portAll$weightPortf  #Portfolio allocation weights for the complete space
portf.lst[[i+4]]$portAll$WBaseInv     #Weighted Bas
portf.lst[[i+4]]$portAll$retGP#Return gain Test-Portfolio

# Dataset 2018-21
nfich1 <- c("Datasets/FeatBUILDminTC3.RData")
save(Feat.tab, RetSR.tab,
      file=nfich1)




