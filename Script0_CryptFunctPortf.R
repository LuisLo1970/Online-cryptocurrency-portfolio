#### MATH FUNCTIONS #######
funcClara <- function(train, nclust=1, samps=2){
  trainSum.ord <- train[,.(RtVol = sd(Rt), RtMean = mean(Rt)), by = SYM] #We aggregate by SYM
  #trainSum.sca <- scale(trainSum.ord[,c('RtVol','RtMean')]) #Scalated values of df[SYM,Rt] dataframe
  
  trainMtx.ord <- as.matrix(trainSum.ord[,c('RtVol','RtMean')])
  rownames(trainMtx.ord) <- trainSum.ord$SYM
  
  set.seed(123)
  if (nclust == 1){
    TrainClust.clara <- clara(trainMtx.ord, k=nclust,metric=c('euclidean'),stand = FALSE, 
                              samples = 50, sampsize = samps, 
                              medoids.x = TRUE, pamLike = TRUE)
    return(TrainClust.clara$medoids)
  }
  nb_max <- NbClust(trainMtx.ord, distance = 'euclidean',index='ch',min.nc = 2, max.nc = 15, method = 'centroid')
  nclust <- nb_max$Best.nc[1]
  #samps <- min(nrow(trainMtx.ord), samps)
  
  TrainClust.clara <- clara(trainMtx.ord, k=nclust, metric=c('euclidean'),stand = FALSE, 
                            samples = 50, sampsize = samps, 
                            medoids.x = TRUE, pamLike = TRUE)

  #TrainClust.clara$clusinfo
  clustsize <- TrainClust.clara[[6]][,1]
  
  temp <- TrainClust.clara$clustering
  temp <- cbind(temp,names(temp))
  temp <- as.data.frame(temp)
  rownames(temp)<-c()
  colnames(temp)<-c('Cluster','SYM')
  train.clust <- merge(train, temp)
  #return(list(TrainClust.clara,train.clust, nclust))
  return(list(TrainCLust=train.clust, nclust=nclust, clsize=clustsize, clustlabels=TrainClust.clara$clustering,
              medoids=TrainClust.clara$medoids))
}

funcCentroid <- function(MyData){
  mv <- apply(MyData,2, mean)
  vo <- apply(MyData,2, sd)
  df <- cbind(vo,mv)
  c <- as.matrix(df)
  centroid <- kmeans(c,1)
  return(centroid$centers)
}


funcCryptSpaceAll2 <- function(TrainWindow, TrainCRYP.wDT.Rt, TestCRYP.wDT.Rt, 
                               DTB90Train.ext, DTB90Test.ext, LDAG.all=0, 
                               nitermax=50, samps=100, mincard = 30){
  #LDAG.all <- last(portfAll[[9]])
  #dTE: day Test End
  # We remove Risk-free column before launch clustering
  #TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,-c('dTB')]
  #TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,-c('dTB')]
  centroids.lst <- list()
  TrainWhole.medoid <- funcClara(TrainWindow[,c('SYM','Rt')]) #when nclust=1 funcClara return a medoid
  centroids.lst <- append(centroids.lst,list(TrainMedoid=TrainWhole.medoid)) #Medoid of the Train DS (whole Train DS  = 1 cluster)
  j <- c(0)
  nclust <- 14 #Maximum number of clusters considered
  while (j <= nitermax & (samps < length(unique(TrainWindow$SYM)))){
    #j <- j + 1
    samps <- 100 + 2*j #Sample size
    ## Clustering part ###########################################################
    # CLARA algorithm ######
    #Train window clusters for CLARA with sampsize 100+j
    #TrainWindow.red <- TrainWindow[TrainWindow$SYM %in% colnames(TrainCRYP.mtx)]
    TrainClust.clara <- funcClara(TrainWindow[,c('SYM', 'Rt')], nclust, samps) #Indeed nclust is only required to be different to 1
    #centroids.lst <- append(centroids.lst,list(TrainClust.clara[[1]]$medoids))
    #TrainClustLab <- TrainClust.clara[[1]]$clustering #Cluster labels
    TrainClustLab <- TrainClust.clara$TrainCLust$Cluster
    
    ## Portfolio Optimiz model ###################################################
    #We interate throw clusters with the higher cardinality
    #nclust <- nrow(TrainClust.clara[[1]]$medoids)
    nclust <- TrainClust.clara$nclust
    for (k in 1:nclust){
      #if (TrainClust.clara[[1]][[6]][k] < mincard){next} 
      if (TrainClust.clara$clsize[k] < mincard){next} #We only consider cluster with a minimun cardinality (30)
      #cl <- TrainClustLab[TrainClustLab == as.character(k)]
      cl <- TrainClust.clara$clustlabels[TrainClust.clara$clustlabels==k]
      #portfClus <- funcOptPortf(TrainCRYP.mtx[,names(cl)],TestCRYP.mtx[,names(cl)])
      # head(dataf[, (colnames(dataf) %in% c('Depr1', 'Depr2', 
      #                                      'Depr4', 'Depr7'))])
      #tickers <- as.character(names(cl))
      #tickers <- append(tickers,'dTB')
      TrainCRYP.wDT.Rt <- as.data.frame(TrainCRYP.wDT.Rt)
      TestCRYP.wDT.Rt <- as.data.frame(TestCRYP.wDT.Rt)
      #TrainCRYP.wDT.Rt[,c(nombre)]
      #xxx <- TrainCRYP.wDT.Rt[,colnames(TrainCRYP.wDT.Rt) %in% nombre]
      #select(TrainCRYP.wDT.Rt,xxx)
      
      portfClus <- try(funcOptPortf2(TrainCRYP.wDT.Rt[,names(cl)],TestCRYP.wDT.Rt[,names(cl)],
                                     DTB90Train.ext, DTB90Test.ext), silent = TRUE)
      if("try-error" %in% class(portfClus)) {next}
      centroids.lst <- append(centroids.lst, list(cluster=k, list(nitermax=nitermax, sampsize=samps, 
                                                                  cryptos=names(cl), 
                                                                  medoid=TrainClust.clara$medoids[k,], 
                                                                  portfCluster = portfClus)))
    }
    j <- j + 1
  }
  return(centroids.lst)
}

funcCryptSpaceAll21 <- function(TrainWindow, TrainCRYP.wDT.Rt, TestCRYP.wDT.Rt, 
                               DTB90Train.ext, DTB90Test.ext, LDAG.all=0, 
                               nitermax=50, samps=100, mincard = 30, TC = 0.0005){
  #Include transaction costs
  #LDAG.all <- last(portfAll[[9]])
  #dTE: day Test End
  # We remove Risk-free column before launch clustering
  #TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,-c('dTB')]
  #TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,-c('dTB')]
  centroids.lst <- list()
  TrainWhole.medoid <- funcClara(TrainWindow[,c('SYM','Rt')]) #when nclust=1 funcClara return a medoid
  centroids.lst <- append(centroids.lst,list(TrainMedoid=TrainWhole.medoid)) #Medoid of the Train DS (whole Train DS  = 1 cluster)
  j <- c(0)
  nclust <- 14 #Maximum number of clusters considered
  while (j <= nitermax){
    #j <- j + 1
    samps <- 100 + 2*j #Sample size
    if (samps >= length(unique(TrainWindow$SYM))){break}
    ## Clustering part ###########################################################
    # CLARA algorithm ######
    #Train window clusters for CLARA with sampsize 100+j
    #TrainWindow.red <- TrainWindow[TrainWindow$SYM %in% colnames(TrainCRYP.mtx)]
    TrainClust.clara <- funcClara(TrainWindow[,c('SYM', 'Rt')], nclust, samps) #Indeed nclust is only required to be different to 1
    #centroids.lst <- append(centroids.lst,list(TrainClust.clara[[1]]$medoids))
    #TrainClustLab <- TrainClust.clara[[1]]$clustering #Cluster labels
    TrainClustLab <- TrainClust.clara$TrainCLust$Cluster
    
    ## Portfolio Optimiz model ###################################################
    #We interate throw clusters with the higher cardinality
    #nclust <- nrow(TrainClust.clara[[1]]$medoids)
    nclust <- TrainClust.clara$nclust
    for (k in 1:nclust){
      #if (TrainClust.clara[[1]][[6]][k] < mincard){next} 
      if (TrainClust.clara$clsize[k] < mincard){next} #We only consider cluster with a minimun cardinality (30)
      #cl <- TrainClustLab[TrainClustLab == as.character(k)]
      cl <- TrainClust.clara$clustlabels[TrainClust.clara$clustlabels==k]
      #portfClus <- funcOptPortf(TrainCRYP.mtx[,names(cl)],TestCRYP.mtx[,names(cl)])
      # head(dataf[, (colnames(dataf) %in% c('Depr1', 'Depr2', 
      #                                      'Depr4', 'Depr7'))])
      #tickers <- as.character(names(cl))
      #tickers <- append(tickers,'dTB')
      TrainCRYP.wDT.Rt <- as.data.frame(TrainCRYP.wDT.Rt)
      TestCRYP.wDT.Rt <- as.data.frame(TestCRYP.wDT.Rt)
      #TrainCRYP.wDT.Rt[,c(nombre)]
      #xxx <- TrainCRYP.wDT.Rt[,colnames(TrainCRYP.wDT.Rt) %in% nombre]
      #select(TrainCRYP.wDT.Rt,xxx)
      
      portfClus <- try(funcOptPortf21(TrainCRYP.wDT.Rt[,names(cl)],TestCRYP.wDT.Rt[,names(cl)],
                                     DTB90Train.ext, DTB90Test.ext,BaseInv = 1.0, TC = 0.0005), silent = TRUE)
      if("try-error" %in% class(portfClus)) {next}
      centroids.lst <- append(centroids.lst, list(cluster=k, list(nitermax=nitermax, sampsize=samps, 
                                                                  cryptos=names(cl), 
                                                                  medoid=TrainClust.clara$medoids[k,], 
                                                                  portfCluster = portfClus)))
    }
    j <- j + 1
  }
  return(centroids.lst)
}

funcCryptSpaceAll3 <- function(TrainWindow, TrainCRYP.wDT.Rt, TestCRYP.wDT.Rt, 
                               DTB90Train.ext, DTB90Test.ext, LDAG.all=0, 
                               nitermax=50, samps=100, mincard = 30){
  #LDAG.all <- last(portfAll[[9]])
  #dTE: day Test End
  # We remove Risk-free column before launch clustering
  #TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,-c('dTB')]
  #TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,-c('dTB')]
  centroids.lst <- list()
  TrainWhole.medoid <- funcClara(TrainWindow[,c('SYM','Rt')]) #when nclust=1 funcClara return a medoid
  centroids.lst <- append(centroids.lst,list(TrainMedoid=TrainWhole.medoid)) #Medoid of the Train DS (whole Train DS  = 1 cluster)
  j <- c(0)
  nclust <- 14 #Maximum number of clusters considered
  while (j <= nitermax & (samps < length(unique(TrainWindow$SYM)))){
    #j <- j + 1
    samps <- 100 + 2*j #Sample size
    ## Clustering part ###########################################################
    # CLARA algorithm ######
    #Train window clusters for CLARA with sampsize 100+j
    #TrainWindow.red <- TrainWindow[TrainWindow$SYM %in% colnames(TrainCRYP.mtx)]
    TrainClust.clara <- funcClara(TrainWindow[,c('SYM', 'Rt')], nclust, samps) #Indeed nclust is only required to be different to 1
    #centroids.lst <- append(centroids.lst,list(TrainClust.clara[[1]]$medoids))
    #TrainClustLab <- TrainClust.clara[[1]]$clustering #Cluster labels
    TrainClustLab <- TrainClust.clara$TrainCLust$Cluster
    
    ## Portfolio Optimiz model ###################################################
    #We interate throw clusters with the higher cardinality
    #nclust <- nrow(TrainClust.clara[[1]]$medoids)
    nclust <- TrainClust.clara$nclust
    for (k in 1:nclust){
      #if (TrainClust.clara[[1]][[6]][k] < mincard){next} 
      if (TrainClust.clara$clsize[k] < mincard){next} #We only consider cluster with a minimun cardinality (30)
      #cl <- TrainClustLab[TrainClustLab == as.character(k)]
      cl <- TrainClust.clara$clustlabels[TrainClust.clara$clustlabels==k]
      #portfClus <- funcOptPortf(TrainCRYP.mtx[,names(cl)],TestCRYP.mtx[,names(cl)])
      # head(dataf[, (colnames(dataf) %in% c('Depr1', 'Depr2', 
      #                                      'Depr4', 'Depr7'))])
      #tickers <- as.character(names(cl))
      #tickers <- append(tickers,'dTB')
      TrainCRYP.wDT.Rt <- as.data.frame(TrainCRYP.wDT.Rt)
      TestCRYP.wDT.Rt <- as.data.frame(TestCRYP.wDT.Rt)
      #TrainCRYP.wDT.Rt[,c(nombre)]
      #xxx <- TrainCRYP.wDT.Rt[,colnames(TrainCRYP.wDT.Rt) %in% nombre]
      #select(TrainCRYP.wDT.Rt,xxx)
      
      portfClus <- try(funcOptPortf3(TrainCRYP.wDT.Rt[,names(cl)],TestCRYP.wDT.Rt[,names(cl)],
                                     DTB90Train.ext, DTB90Test.ext), silent = TRUE)
      if("try-error" %in% class(portfClus)) {next}
      centroids.lst <- append(centroids.lst, list(cluster=k, list(nitermax=nitermax, sampsize=samps, 
                                                                  cryptos=names(cl), 
                                                                  medoid=TrainClust.clara$medoids[k,], 
                                                                  portfCluster = portfClus)))
    }
    j <- j + 1
  }
  return(centroids.lst)
}

#####
#Programacion cuadratica (Computational Actuarial Sciences with R)
# We arrange the terms of the Optimization function solve.QP


#TrainCRYP.mtx <- TrainCRYP.mtx[,names(cl)]
#TestCRYP.mtx <- TestCRYP.mtx[,names(cl)]
#Fin prueba
funcOptPortf2 <- function(TrainCRYP.wDT.Rt,TestCRYP.wDT.Rt, DTB90Train.ext, DTB90Test.ext, BaseInv = 1.0){
  set.seed(123)
  result <- list()  ## Result that the function will return
  TrainCRYP.mtx <- as.matrix(TrainCRYP.wDT.Rt)
  TestCRYP.mtx <- as.matrix(TestCRYP.wDT.Rt)
  
  # Atributes: Setting the weight of the portfolio
  wmax <- 0.50      ## maximum weight-holding size
  wmin <- 0.001     ## minimum weight-holding sizeess
  nc <- ncol(TrainCRYP.mtx)
  m <- colMeans(tail(TrainCRYP.mtx,30))
  rd <- mean(m)
  
  #A <- rbind(rep(1,nc))
  A <- t(rep(1,nc))
  a <- 1
  # B <- rbind(-diag(nc),
  #            diag(nc))
  B <- rbind(t(m),
             -diag(nc),
             diag(nc))
  # b <- rbind(array(-wmax, dim = c(nc, 1)),
  #            array( wmin, dim = c(nc, 1)))
  b <- rbind(rd, array(-wmax, dim = c(nc, 1)),
             array( wmin, dim = c(nc, 1)))
  MASSrob <- cov.trob(TrainCRYP.mtx, cor = FALSE, nu = 5,
                      maxit = 150, tol = 0.001)
  
  Q <- 2 * MASSrob$cov
  ##### Optimization model ################
  QPSol <- solve.QP(Dmat = Q,
                    dvec = rep(0, nc),
                    Amat = t(rbind(A,B)),
                    bvec = rbind(a,b),
                    meq  = 1)
  ## check budget constraint and solution
  w <- QPSol$solution
  wHigher <- (round(w, digits = 4) > 0.01)
  wPortf <- as.matrix(w[wHigher])
  WBaseInv <- sum(wPortf*BaseInv) #Amount invested in cryptocurrencies every period
  #wPortf.ext <- wPortf
  wPortf <- rbind(wPortf,1-sum(wPortf)) #We add the percentage for risk-free assets
  #wPortf[nrow(wPortf)+1,1]<- (1-sum(wPortf)) #Last position of array is percentage for risk-free
  
  
  crypPortfNames <- colnames(TestCRYP.mtx)[wHigher]
  crypPortf      <- as.matrix(TestCRYP.mtx[,wHigher])
  crypPortfTrain <- as.matrix(TrainCRYP.mtx[,wHigher])
  
  #We add risk-free rate column
  crypPortf <- cbind(crypPortf,DTB90Test.ext$dTB)
  crypPortfTrain <- cbind(crypPortfTrain,DTB90Train.ext$dTB)
  
  crypPortfNames.ext <- append(crypPortfNames,'RFA')
  colnames(crypPortf) <- crypPortfNames.ext
  colnames(crypPortfTrain) <- crypPortfNames.ext
  
  #Centroid of Portfolios ####################################
  #Test Portfolio
  portfWeitghted <- matrix(t(wPortf),nrow(crypPortf),nrow(wPortf), byrow = TRUE) * crypPortf
  portfCentroid <- funcCentroid(portfWeitghted) #We compute the centroid of the Test monthly portfolio computed in daily basis
  
  #Train Portfolio
  portfTrainWeitghted <- matrix(t(wPortf),nrow(crypPortfTrain),nrow(wPortf), byrow = TRUE) * crypPortfTrain
  portfTrainCentroid <- funcCentroid(portfTrainWeitghted)
  
  ##Accumulated wealth: Mode 1 ################################
  # portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
  # #retGP <- (last(portf) - WBaseInv)/WBaseInv
  # retGP <- (last(portf) - BaseInv)/BaseInv
  # #portfTrain <- pftPerf(crypPortfTrain, wPortf, WBaseInv) #Accumulated wealth of the Train portfolio
  # portfTrain <- pftPerf(crypPortfTrain, wPortf, BaseInv) #Accumulated wealth of the Train portfolio
  # #retGPTrain <- (last(portfTrain) - WBaseInv)/WBaseInv
  # retGPTrain <- (last(portfTrain) - BaseInv)/BaseInv
  
  ##Accumulated wealth: Mode 2 ################################
  ## Inicio traceo

  # kk <- apply(portfWeitghted,1,sum)
  # yy <-   cumprod(c(1, 1 + kk))
  # (1+0.012171989)*(1+0.006984881)*(1+0.002605090) #1.021897
  # 
  # ThreeDays <- CRYP.RT[CRYP.RT$SYM %in% crypPortfNames & CRYP.RT$time>='2019-01-01' & CRYP.RT$time<='2019-01-03',]
  # ThreeDays.wide <- dcast.data.table(ThreeDays, time ~ SYM, value.var = "close")
  # Return.calculate(ThreeDays.wide)
  # (0.003746 - 0.003642)/0.003642 #Return(2019-01-02) --> BOLI: 0.02855574
  # TestCRYP.wDT.Rt[1:3,c('BOLI')]
  # matplot(ThreeDays.wide, type = 'l')
  # CRYP.DS.ThreeDays <- CRYP.DS[CRYP.DS$Symbol=='REV' & CRYP.DS$time>='2019-01-01' & CRYP.DS$time<='2019-01-03',]
  # 
  # ThreeDaysRt.wide <- dcast.data.table(ThreeDays, time ~ SYM, value.var = "Rt")
  # matplot(ThreeDaysRt.wide, type = 'l')
  
  ## Fin traceo
  
  portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
  portfTrain <- pftPerf(crypPortfTrain, wPortf, BaseInv) #Accumulated wealth of the Train portfolio
  
  #TrainDays <- TrainCRYP.wDT.Rt$time
  #TestDays <- TestCRYP.wDT.Rt$time
  TrainDays <- DTB90Train.ext$time
  TestDays <- DTB90Test.ext$time
  crypPortfTrain.xts <- as.xts(crypPortfTrain %*% wPortf,TrainDays)
  crypPortf.xts <- as.xts(crypPortf %*% wPortf,TestDays)
  
  retGPTrain <- Return.cumulative(crypPortfTrain.xts)[1]
  retGP <- Return.cumulative(crypPortf.xts)[1]
  
  ### Yearly Sharpe ratio computation for Training and Test ###########################
  #asroTrain <- as.sropt(portfTrainWeitghted, ope = 365, epoch = "yr")
  #asroTest <- as.sropt(portfWeitghted, ope = 365, epoch = "yr")

  RFA.train <- as.xts(DTB90Train.ext$dTB,DTB90Train.ext$time) #Risk-free asset
  RFA.test <- as.xts(DTB90Test.ext$dTB, DTB90Test.ext$time)
  
  asroTest <- as.sr(crypPortf.xts, c0 = mean(RFA.test), ope = 365, epoch = "yr")
  asroTrain <- as.sr(crypPortfTrain.xts, c0 = mean(RFA.train), ope = 365, epoch = "yr")
  # AdjustedSharpeRatio(crypPortf.xts, Rf = mean(RFA.test))
  # SharpeRatio(crypPortf.xts, Rf = mean(RFA.test), FUN="StdDev", annualize = TRUE)
  # (mean(crypPortf.xts)-mean(RFA.test))/StdDev(crypPortf.xts)
  
  result <- append(result, list(QPSolVal=QPSol$value, crypPortfNames,weightPortf=wPortf,WBaseInv=WBaseInv, 
                                portfTestCentroid=portfCentroid, portfTest=portf, 
                                accumGainTest=last(portf), retGP=retGP,
                                portfTrainCentroid=portfTrainCentroid,
                                accumGainTrain=last(portfTrain), retGPTrain=retGPTrain,
                                SRTrain=asroTrain$sr[1], SRTest=asroTest$sr[1]))
  
  return(result)
}

# # #Pruebas
# TrainCRYP.wDT.Rt <- TrainCRYP.wDT.Rt[,names(cl)]
# TestCRYP.wDT.Rt <- TestCRYP.wDT.Rt[,names(cl)]

#This function include Transaction Costs (TC)
funcOptPortf21 <- function(TrainCRYP.wDT.Rt,TestCRYP.wDT.Rt, DTB90Train.ext, DTB90Test.ext, BaseInv = 1.0, TC = 0.0005){
  set.seed(123)
  result <- list()  ## Result that the function will return
  
  TrainCRYP.mtx <- as.matrix(TrainCRYP.wDT.Rt)
  TestCRYP.mtx <- as.matrix(TestCRYP.wDT.Rt)
  
  # Atributes: Setting the weight of the portfolio
  wmax <- 0.50      ## maximum weight-holding size
  wmin <- 0.001     ## minimum weight-holding size
  nc <- ncol(TrainCRYP.mtx)
  m <- colMeans(tail(TrainCRYP.mtx,30))
  rd <- mean(m)
  
  # We apply TC
  mtc <- m - TC
  
  #A <- rbind(rep(1,nc))
  A <- t(rep(1,nc))
  a <- 1
  # B <- rbind(-diag(nc),
  #            diag(nc))
  B <- rbind(t(mtc),
             -diag(nc),
             diag(nc))
  # b <- rbind(array(-wmax, dim = c(nc, 1)),
  #            array( wmin, dim = c(nc, 1)))
  b <- rbind(rd, array(-wmax, dim = c(nc, 1)),
             array( wmin, dim = c(nc, 1)))
  MASSrob <- cov.trob(TrainCRYP.mtx, cor = FALSE, nu = 5,
                      maxit = 200, tol = 0.001)
  
  Q <- 2 * MASSrob$cov
  ##### Optimization model ################
  QPSol <- solve.QP(Dmat = Q,
                    dvec = rep(0, nc),
                    Amat = t(rbind(A,B)),
                    bvec = rbind(a,b),
                    meq  = 1)
  ## check budget constraint and solution
  w <- QPSol$solution
  wHigher <- (round(w, digits = 4) > 0.001)
  wPortf <- as.matrix(w[wHigher])
  WBaseInv <- sum(wPortf*BaseInv) #Amount invested in cryptocurrencies every period
  #wPortf.ext <- wPortf
  wPortf <- rbind(wPortf,1-sum(wPortf)) #We add the percentage for risk-free assets
  #wPortf[nrow(wPortf)+1,1]<- (1-sum(wPortf)) #Last position of array is percentage for risk-free
  
  
  crypPortfNames <- colnames(TestCRYP.mtx)[wHigher]
  crypPortf      <- as.matrix(TestCRYP.mtx[,wHigher]) - TC
  crypPortfTrain <- as.matrix(TrainCRYP.mtx[,wHigher])- TC
  
  #We add risk-free rate column
  crypPortf <- cbind(crypPortf,DTB90Test.ext$dTB)
  crypPortfTrain <- cbind(crypPortfTrain,DTB90Train.ext$dTB)
  
  crypPortfNames.ext <- append(crypPortfNames,'RFA')
  colnames(crypPortf) <- crypPortfNames.ext
  colnames(crypPortfTrain) <- crypPortfNames.ext
  
  #Centroid of Portfolios ####################################
  #Test Portfolio
  portfWeitghted <- matrix(t(wPortf),nrow(crypPortf),nrow(wPortf), byrow = TRUE) * crypPortf
  portfCentroid <- funcCentroid(portfWeitghted) #We compute the centroid of the Test monthly portfolio computed in daily basis
  
  #Train Portfolio
  portfTrainWeitghted <- matrix(t(wPortf),nrow(crypPortfTrain),nrow(wPortf), byrow = TRUE) * crypPortfTrain
  portfTrainCentroid <- funcCentroid(portfTrainWeitghted)
  
  ##Accumulated wealth: Mode 1 ################################
  # portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
  # #retGP <- (last(portf) - WBaseInv)/WBaseInv
  # retGP <- (last(portf) - BaseInv)/BaseInv
  # #portfTrain <- pftPerf(crypPortfTrain, wPortf, WBaseInv) #Accumulated wealth of the Train portfolio
  # portfTrain <- pftPerf(crypPortfTrain, wPortf, BaseInv) #Accumulated wealth of the Train portfolio
  # #retGPTrain <- (last(portfTrain) - WBaseInv)/WBaseInv
  # retGPTrain <- (last(portfTrain) - BaseInv)/BaseInv
  
  ##Accumulated wealth: Mode 2 ################################
  ## Inicio traceo
  
  # kk <- apply(portfWeitghted,1,sum)
  # yy <-   cumprod(c(1, 1 + kk))
  # (1+0.012171989)*(1+0.006984881)*(1+0.002605090) #1.021897
  # 
  # ThreeDays <- CRYP.RT[CRYP.RT$SYM %in% crypPortfNames & CRYP.RT$time>='2019-01-01' & CRYP.RT$time<='2019-01-03',]
  # ThreeDays.wide <- dcast.data.table(ThreeDays, time ~ SYM, value.var = "close")
  # Return.calculate(ThreeDays.wide)
  # (0.003746 - 0.003642)/0.003642 #Return(2019-01-02) --> BOLI: 0.02855574
  # TestCRYP.wDT.Rt[1:3,c('BOLI')]
  # matplot(ThreeDays.wide, type = 'l')
  # CRYP.DS.ThreeDays <- CRYP.DS[CRYP.DS$Symbol=='REV' & CRYP.DS$time>='2019-01-01' & CRYP.DS$time<='2019-01-03',]
  # 
  # ThreeDaysRt.wide <- dcast.data.table(ThreeDays, time ~ SYM, value.var = "Rt")
  # matplot(ThreeDaysRt.wide, type = 'l')
  
  ## Fin traceo
  
  portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
  portfTrain <- pftPerf(crypPortfTrain, wPortf, BaseInv) #Accumulated wealth of the Train portfolio
  
  #TrainDays <- TrainCRYP.wDT.Rt$time
  #TestDays <- TestCRYP.wDT.Rt$time
  TrainDays <- DTB90Train.ext$time
  TestDays <- DTB90Test.ext$time
  crypPortfTrain.xts <- as.xts(crypPortfTrain %*% wPortf,TrainDays)
  crypPortf.xts <- as.xts(crypPortf %*% wPortf,TestDays)
  
  retGPTrain <- Return.cumulative(crypPortfTrain.xts)[1]
  retGP <- Return.cumulative(crypPortf.xts)[1]
  
  ### Yearly Sharpe ratio computation for Training and Test ###########################
  #asroTrain <- as.sropt(portfTrainWeitghted, ope = 365, epoch = "yr")
  #asroTest <- as.sropt(portfWeitghted, ope = 365, epoch = "yr")
  
  RFA.train <- as.xts(DTB90Train.ext$dTB,DTB90Train.ext$time) #Risk-free asset
  RFA.test <- as.xts(DTB90Test.ext$dTB, DTB90Test.ext$time)
  
  asroTest <- as.sr(crypPortf.xts, c0 = mean(RFA.test), ope = 365, epoch = "yr")
  asroTrain <- as.sr(crypPortfTrain.xts, c0 = mean(RFA.train), ope = 365, epoch = "yr")
  # AdjustedSharpeRatio(crypPortf.xts, Rf = mean(RFA.test))
  # SharpeRatio(crypPortf.xts, Rf = mean(RFA.test), FUN="StdDev", annualize = TRUE)
  # (mean(crypPortf.xts)-mean(RFA.test))/StdDev(crypPortf.xts)
  
  result <- append(result, list(QPSolVal=QPSol$value, crypPortfNames,weightPortf=wPortf,WBaseInv=WBaseInv, 
                                portfTestCentroid=portfCentroid, portfTest=portf, 
                                accumGainTest=last(portf), retGP=retGP,
                                portfTrainCentroid=portfTrainCentroid,
                                accumGainTrain=last(portfTrain), retGPTrain=retGPTrain,
                                SRTrain=asroTrain$sr[1], SRTest=asroTest$sr[1]))
  
  return(result)
}


funcOptPortf3 <- function(TrainCRYP.wDT.Rt,TestCRYP.wDT.Rt, 
                          DTB90Train.ext, DTB90Test.ext, BaseInv = 1){
  TrainCRYP.mtx <- as.matrix(TrainCRYP.wDT.Rt)
  TestCRYP.mtx <- as.matrix(TestCRYP.wDT.Rt)
  n <- ncol(TrainCRYP.mtx) #Number of cryptocurrencies
  w      <- 1.0; #Full invested condition: sum(weights)=1    
  mu <- apply(tail(TrainCRYP.wDT.Rt,30), 2, mean)   #We compute mu on the last 30 training-days
  #mu <- apply(TrainCRYP.wDT.Rt, 2, mean)   #We compute mu on the last 30 training-days
  x0 <- rep(0,n)
  gammas <- apply(TrainCRYP.wDT.Rt, 2, sd)
  gamma <-quantile(gammas)[1] #Mean% quantile
  
  MASSrob <- cov.trob(TrainCRYP.mtx, cor = FALSE, nu = 5,
                      maxit = 150, tol = 0.01)
  #endtime <- Sys.time()
  #endtime - startime
  GT <- MASSrob$cov #Covariance matrix
  
  f = rep(0.01,n) #https://bitcoinvisuals.com/chain-fees-tx-usd
  g = rep(0.005,n) #https://www.investopedia.com/tech/how-much-does-it-cost-buy-cryptocurrency-exchanges/
  
  k = 20 #Maximum number of cryptocurrencies int o the portfolio
  
  result <- list()
  set.seed(123)
  
  r <- MarkowitzWithTransaCard(n,mu,GT,x0,w,gamma, f, g, k)
  
  # Cryptocurrencies into the portfolio
  wHigher <- r$card
  wPortf <- r$x[wHigher]
  WBaseInv <- sum(wPortf*BaseInv)
  crypPortfNames <- colnames(TestCRYP.wDT.Rt)[wHigher]
  crypPortf <- as.matrix(TestCRYP.mtx[,wHigher])
  crypPortfTrain <- as.matrix(TrainCRYP.mtx[,wHigher])
  
  colnames(crypPortf) <- crypPortfNames
  colnames(crypPortfTrain) <- crypPortfNames
  
  
  # Centroid of Portfolios #################
  # Test Portfolio
  portfWeighted <- matrix(wPortf, nrow(crypPortf),length(wPortf), byrow = TRUE) * crypPortf
  portfCentroid <- funcCentroid(portfWeighted) #Centroid of the Test monthly portfolio computed in daily basis
  
  # Train Portfolio
  portfTrainWeitghted <- matrix(wPortf,nrow(crypPortfTrain),length(wPortf), byrow = TRUE) * crypPortfTrain
  portfTrainCentroid <- funcCentroid(portfTrainWeitghted)
  
  ## Accumulated wealth: Mode 2 ################################
  portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
  portfTrain <- pftPerf(crypPortfTrain, wPortf) #Accumulated wealth of the Train portfolio
  
  ## Benchmark Index ###########################################
  TrainDays <- DTB90Train.ext$time
  TestDays <- DTB90Test.ext$time
  
  ## Cumulative returns of the Portfolio
  crypPortfTrain.xts <- as.xts(crypPortfTrain %*% wPortf,TrainDays)
  crypPortf.xts <- as.xts(crypPortf %*% wPortf,TestDays)
  
  retGPTrain <- Return.cumulative(crypPortfTrain.xts)[1]
  retGP <- Return.cumulative(crypPortf.xts)[1]
  
  ### Yearly Sharpe ratio computation for Training and Test #########
  RFA.train <- as.xts(DTB90Train.ext$dTB,DTB90Train.ext$time) #Risk-free asset
  RFA.test <- as.xts(DTB90Test.ext$dTB, DTB90Test.ext$time)
  
  asroTest <- as.sr(crypPortf.xts, c0 = mean(RFA.test), ope = 365, epoch = "yr")
  asroTrain <- as.sr(crypPortfTrain.xts, c0 = mean(RFA.train), ope = 365, epoch = "yr")
  
  result <- append(result, list(QPSolVal=r$expret, crypPortfNames,weightPortf=wPortf,WBaseInv=WBaseInv, 
                                portfTestCentroid=portfCentroid, portfTest=portf, 
                                accumGainTest=last(portf), retGP=retGP,
                                portfTrainCentroid=portfTrainCentroid,
                                accumGainTrain=last(portfTrain), retGPTrain=retGPTrain,
                                SRTrain=asroTrain$sr[1], SRTest=asroTest$sr[1]))
}

MarkowitzWithTransaCard <- function(
  n,          # Number of assets
  mu,         # An n-dimmensional vector of expected returns
  GT,         # A matrix with n columns so (GT')*GT  = covariance matrix
  x0,         # Initial holdings 
  w,          # Initial cash holding
  gamma,      # Maximum risk (=std. dev) accepted
  f,          # Fixed transaction cost
  g,          # Linear part of transaction cost
  k)          # Cardinality bound
{
  # Upper bound on the traded amount
  u <- w+sum(x0)
  
  prob <- list(sense="max")
  prob$c <- c(mu, rep(0,2*n))
  
  # Specify linear constraints
  # [ e'  g'  f' ]   [ x ]  =   w + e'*x0
  # [ I  -I   0  ] * [ z ]  <=  x0
  # [ I   I   0  ]   [ y ]  >=  x0
  # [ 0   I  -U  ]          <=  0
  # [ 0   0   e' ]          <=  k
  prob$A <- rbind(cbind(Matrix(1.0,ncol=n), t(g),              t(f)),
                  cbind(Diagonal(n, 1.0),   -Diagonal(n, 1.0), Matrix(0,n,n)),
                  cbind(Diagonal(n, 1.0),   Diagonal(n, 1.0),  Matrix(0,n,n)),
                  cbind(Matrix(0,n,n),      Diagonal(n, 1.0),  Diagonal(n, -u)),
                  cbind(Matrix(0.0,ncol=2*n), Matrix(1.0,ncol=n)))
  prob$bc <- rbind(blc=c(w+sum(x0), rep(-Inf,n), x0, rep(-Inf,n), 0.0),
                   buc=c(w+sum(x0), x0, rep(Inf,n), rep(0.0,n), k))
  # No shortselling and the linear bound 0 <= y <= 1     
  prob$bx <- rbind(blx=c(rep(0.0,n), rep(-Inf,n), rep(0.0,n)),
                   bux=c(rep(Inf,n), rep(Inf, n), rep(1.0,n)))
  
  # Specify the affine conic constraints for risk
  prob$F <- rbind(
    Matrix(0.0,nrow=1,ncol=3*n), 
    cbind(GT, Matrix(0.0,nrow=n,ncol=2*n))
  )
  prob$g <- c(gamma,rep(0,n))
  prob$cones <- matrix(list("QUAD", 1+n, NULL), nrow=3, ncol=1)
  rownames(prob$cones) <- c("type","dim","conepar")
  
  # Demand y to be integer (hence binary)
  prob$intsub <- (2*n+1):(3*n);
  
  # Solve the problem
  #opts <- list(getinfo=TRUE)
  r <- mosek(prob,list(verbose=1))
  stopifnot(identical(r$response$code, 0))
  
  # Return the solution
  x <- r$sol$int$xx[1:n]
  z <- r$sol$int$xx[(n+1):(2*n)]
  y <- r$sol$int$xx[(2*n+1):(3*n)]
  
  crd <- (round(x, digits = 4) > 0.001)
  
  list(card=crd, expret=drop(mu %*% x), stddev=gamma, 
       cost = drop(f %*% y)+drop(g %*% z), x=x)
}

#Ganancia acumulada de la cartera
pftPerf <- function(x, w, W0 = 1.0) {
  W0 * cumprod(c(1, 1 + x %*% w))
}

#Annualized returns for monthly returns
funcRetCum <- function(ret.cum, months=21){ 
  ret.ann <-  (1+ret.cum)^(12/months) - 1
}

# funcOptPortf <- function(TrainCRYP.mtx, TestCRYP.mtx, BaseInv = 1000){
#   set.seed(123)
#   result <- list()  ## Result that the function will return
#   # Atributes: Setting the weight of the portfolio
#   wmax <- 0.75      ## maximum weight-holding size
#   wmin <- 0.001     ## minimum weight-holding size
#  
#   nc <- ncol(TrainCRYP.mtx)
#   #m <- colMeans(TrainCRYP.mtx)
#   
#   A <- rbind(rep(1,nc))
#   a <- 1
#   
#   #Inic Mio
#   B <- rbind(-diag(nc),
#              diag(nc))
#   b <- rbind(array(-wmax, dim = c(nc, 1)),
#              array( wmin, dim = c(nc, 1)))
#   
#   ###### Cov estimations #################################
#   # ## 0) Standard cov estimation (wrapped MASS::cov.trob function)
#   # covStd <- cov(TrainCRYP.mtx.red) 
#   # Q <- 2 * covStd
#   # 
#   # # 1) Robust cov estimation: Heavy Tails robust covariance estimator
#   # library(fitHeavyTail)
#   # HTrob <- fit_mvt(TrainCRYP.mtx.red,max_iter = 150)
#   # Q <- 2 * HTrob$cov
#   
#   #2) Robust cov estimation: Multivariate t-distribution of MASS library
#   #cov.trob(x, wt = rep(1, n), cor = FALSE, center = TRUE, nu = 5,
#   #         maxit = 25, tol = 0.01)
#   
#   MASSrob <- cov.trob(TrainCRYP.mtx, cor = TRUE, nu = 5,
#                       maxit = 150, tol = 0.01)
#   
#   Q <- 2 * MASSrob$cov
#   ##### Optimization model ################
#   QPSol <- solve.QP(Dmat = Q,
#                      dvec = rep(0, nc),
#                      Amat = t(rbind(A,B)),
#                      bvec = rbind(a,b),
#                      meq  = 1)
#   ## check budget constraint and solution
#   w <- QPSol$solution
#   #sum(w)  ## budget constraint: should be 1
#   #all.equal(as.numeric(var(R %*% w)),result$value)
#   #barplot(w, ylim = c(0,1), las = 2,
#   #        main = "Solucion del modelo MV: mean(returns)")
#   
#   wHigher <- (round(w, digits = 4) > 0.001)
#   wPortf <- as.matrix(w[wHigher])
#   
#   WBaseInv <- sum(wPortf*BaseInv) #Amount invested in every period
#   crypPortfNames <- colnames(TestCRYP.mtx)[wHigher]
#   crypPortf      <- as.matrix(TestCRYP.mtx[,wHigher])
#   crypPortfTrain <- as.matrix(TrainCRYP.mtx[,wHigher])
#   
#   #Centroid of Portfolios
#   #Test Portfolio
#   portfWeitghted <- matrix(t(wPortf),nrow(crypPortf),nrow(wPortf), byrow = TRUE) * crypPortf
#   portfCentroid <- funcCentroid(portfWeitghted) #We compute the centroid of the Test monthly portfolio computed in daily basis
#   
#   #Train Portfolio
#   portfTrainWeitghted <- matrix(t(wPortf),nrow(crypPortfTrain),nrow(wPortf), byrow = TRUE) * crypPortfTrain
#   portfTrainCentroid <- funcCentroid(portfTrainWeitghted)
# 
#   portf      <- pftPerf(crypPortf, wPortf) #Accumulated wealth of the Test portfolio
#   #retGP <- (last(portf) - WBaseInv)/WBaseInv
#   retGP <- (last(portf) - BaseInv)/BaseInv
#   #portfTrain <- pftPerf(crypPortfTrain, wPortf, WBaseInv) #Accumulated wealth of the Train portfolio
#   portfTrain <- pftPerf(crypPortfTrain, wPortf, BaseInv) #Accumulated wealth of the Train portfolio
#   #retGPTrain <- (last(portfTrain) - WBaseInv)/WBaseInv
#   retGPTrain <- (last(portfTrain) - BaseInv)/BaseInv
#   
#   # Sharpe ratio computation for Training and Test
#   asroTrain <- as.sropt(portfTrainWeitghted, ope = 365, epoch = "yr")
#   asroTest <- as.sropt(portfWeitghted, ope = 365, epoch = "yr")
#   
#   result <- append(result, list(QPSolVal=QPSol$value, crypPortfNames,weightPortf=wPortf,WBaseInv=WBaseInv, 
#                                 portfTestCentroid=portfCentroid, portfTest=portf, 
#                                 accumGainTest=last(portf), retGP=retGP,
#                                 portfTrainCentroid=portfTrainCentroid,
#                                 accumGainTrain=last(portfTrain), retGPTrain=retGPTrain,
#                                 SRTrain=asroTrain$sropt, SRInfoTrain=sric(asroTrain),
#                                 SRTest=asroTest$sropt, SRInfoTest=sric(asroTest)))
#   
#   return(result)
# }

# funcOptPortfSymple <- function(MyData.mtx){
#   set.seed(123)
#   # Attributes: Setting the weight of the portfolio
#   wmax <- 0.75      ## maximum weight-holding size
#   wmin <- 0.001     ## minimum weight-holding size
#   result <- list()  ## Result that the function will return
#   
#   nc <- ncol(MyData.mtx)
#   A <- rbind(rep(1,nc))
#   a <- 1
#   
#   #Inic Mio
#   B <- rbind(-diag(nc),
#              diag(nc))
#   b <- rbind(array(-wmax, dim = c(nc, 1)),
#              array( wmin, dim = c(nc, 1)))
#   
#   MASSrob <- cov.trob(MyData.mtx, cor = TRUE, nu = 5,
#                       maxit = 150, tol = 0.01)
#   Q <- 2 * MASSrob$cov
#   ##### Optimization model ################
#   result <- solve.QP(Dmat = Q,
#                      dvec = rep(0, nc),
#                      Amat = t(rbind(A,B)),
#                      bvec = rbind(a,b),
#                      meq  = 1)
#   ## check budget constraint and solution
#   w <- result$solution
# 
#   wHigher <- (round(w, digits = 4) > 0.001)
#   wPortf <- as.matrix(w[wHigher])
#   
#   crypPortf <- as.matrix(MyData.mtx[,wHigher])
#   portf <- pftPerf(crypPortf, wPortf) #Accumulated gain of the portfolio
#   result <- append(result, list(colnames(crypPortf),wPortf,portf))
#   
#   return(result)
# }

##########################################################################



QP_solver <- function(c, Q, cstr = list(), trace = FALSE) {
  Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
  aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
  A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
  a <- Reduce(c, cstr[names(cstr) %in% "a"])
  
  sol <- try(solve.QP(Dmat = Q,
                      dvec = -2 * c,
                      Amat = t(rbind(Aeq, A)),
                      bvec = c(aeq, a),
                      meq = nrow(Aeq)),
             silent = TRUE)
  if (trace) cat(sol)
  if (inherits(sol, "try-error"))
    list(solution = rep(NA, length(c)), status = 1)
  else
    list(solution = sol$solution, status = 0)
}

#Restricciones
#Target Reward

targetReturn <- function(x, target){
  list(Aeq = rbind(colMeans(x)), aeq = target)
}

#Full investment
fullInvest <- function(x){
  list(Aeq = matrix(1,nrow = 1, ncol = ncol(x)), aeq = 1)
}

#Long Only
longOnly <- function(x){
  list(A = diag(1, ncol(x)), a = rep(0, ncol(x)))
}

#MV Portfolio
MV_QP <- function(x, target, Sigma = cov(x),...,
                  cstr = c(fullInvest(x),
                           targetReturn(x, target),
                           longOnly(x),...),
                  trace = FALSE) {
  #Quadratic cofficients
  size <- ncol(x)
  c <- rep(0, size)
  Q <- Sigma
  
  #Optimizacion
  sol <- QP_solver(c, Q, cstr, trace)
  
  #Extract weights
  weights <- sol$solution
  names(weights) <- colnames(x)
  weights
  
}

#Rentabilidad anual cartera
#Devuelve un vector con el rendimiento medio para cada una de las carteras
# x: Matriz de rendimientos (tantas columnas como acciones, las filas son dias cotizados)
# w: pesos de cada accion en la cartera (cada columna es una cartera con los pesos de cada accion)
rendAnu <- function(x, w){
  apply(x %*% w, 2, mean) * 252
}

sdAnu <- function(x, w){
  apply(x %*% w, 2, sd) * sqrt(252)
}


rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}


###################################################
### code chunk number 2: random-returns (Numerical Methods and Optimization in Finance)
###################################################
random_returns <- function(na, ns, sd, mean = 0, rho = 0) {
  ## sd   = vol of returns
  ## mean = means of returns
  ##      ==> both may be scalars or vectors of length na
  
  ans <- rnorm(ns*na)
  dim(ans) <- c(na, ns)
  
  if (rho != 0) {
    C <- array(rho, dim = c(na, na))
    diag(C) <- 1
    ans <- t(chol(C)) %*% ans
  }
  ans <- ans*sd
  ans <- ans + mean
  t(ans)
}



###########################################################################################################
#Creation of a list of cryptocurrencies with  daily quotation, returns and marketcap
# <funcCrypto> return a list with following objects:
#1) Dataframe1: CRYPT ticket, first and last day of quotation, traded days
#2) Dataframe2: time-series of quotation (close, timestamp)
#3) Dataframe3: time-series of marketcap (marketcap, timestamp)
#4) Dataframe4: time-sries of daily returns
funCrypto <- function(n1){
  nz <- CRYP.wDF.close[,colnames(CRYP.wDF.close)[n1]]
  nz2 <- CRYP.wDF.volume[,colnames(CRYP.wDF.volume)[n1]]
  nz3 <- CRYP.wDF.rend[,colnames(CRYP.wDF.rend)[n1]]
  
  rg.fech <- CRYP.wDF.close$time[!is.na(nz)]
  first_fech <- rg.fech[1]
  last_fech <- tail(rg.fech, n=1)
  
  basicInf.df <- data.frame(colnames(CRYP.wDF.close)[n1], first_fech, last_fech, length(rg.fech))
  colnames(basicInf.df) <- c('CRYP','First','Last','Samples')
  serieClose.df <- cbind.data.frame(rg.fech,na.omit(nz))
  colnames(serieClose.df) <- c('Fecha','Close')
  serieVolume.df <- cbind.data.frame(rg.fech,na.omit(nz2))
  colnames(serieVolume.df) <- c('Fecha','Volume')
  serieRend.df <- cbind.data.frame(rg.fech,na.omit(nz3))
  colnames(serieRend.df) <- c('Fecha','Rend')
  result <- list(Crypt=colnames(CRYP.wDF.close)[n1], BasicInfo=basicInf.df,
                 serieclose=serieClose.df, serieVolume=serieVolume.df, serieRend=serieRend.df)        
  
  return(result)
}
#Quantile categories
quantFunc <- function(n,q){
  if(is.na(n)) return ('NA')
  if(n<q[2]) return ('Low')
  if(n>q[5]) return ('High')
  return ('Average')
}

quantFunc2 <- function(n,q1,q2,q3){
  if(is.na(n)) return ('NA')
  if(n<q1) return ('Low')
  if(n>q3) return ('High')
  return ('Average')
}

#Deciles categories
decFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[4]) return ('D4')
  if(n<=p[5] & n>p[4]) return ('D5')
  if(n<=p[6] & n>p[5]) return ('D6')
  if(n<=p[7] & n>p[6]) return ('D7')
  if(n<=p[8] & n>p[7]) return ('D8')
  if(n<=p[9] & n>p[8]) return ('D9')
  return ('D10')
}

#Percentile categories
percFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[70]) return ('P70')
  if(n<=p[80] & n>p[70]) return ('P80')
  if(n<=p[90] & n>p[80]) return ('P90')
  if(n<=p[99] & n>p[90]) return ('P99')
  return ('P100')
}

riskAversFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[20]) return ('CONS')
  if(n<=p[50] & n>p[20]) return ('AVE')
  return ('RISKY')
}

#Specific Beta categories
betaFunc <- function(b){
  if(is.na(b)) return ('NA')
  if(b< -0.01) return ('NegBeta') #Inverse relation with the market Negative Beta
  if((b>= -0.01) && (b<0.01)) return ('CashLike') #cash like
  if((b>= 0.01) && (b<0.95)) return ('LowVol') #Volatilidad Lower than market
  if((b>= 0.95) && (b<1.05)) return ('Indexlike')
  if((b>= 1.05) && (b<100)) return ('HighVol') #High volatility -Techno like-
  if(b> 100) return('Extreme') #Extreme price swing
}

#Specific Sharp categories
sharpeFunc <- function(s){
  if(is.na(s)) return ('NA')
  if(s<0) return('SRF') #Smaller Risk-free
  if(s>=0.0 && s< 0.5) return('ERP') #Excess return positive
  if(s>=0.5 && s<1.0) return ('Acc') #Acceptable for investment but risky
  if(s>=1.0)  return ('GOOD') #Acceptable to Good by investor
}


## Sigmoide ##
sigmoid <- function(x,a=0){
  return(1/(1+exp(-(x-a))))
}

Norm <- function(x){
  return((x-mean(x))/sd(x))
}

##### FEATURE QUALIFICATION
#Qualy1: sign of the step
QSignFunc <- function(n){
  if(is.na(n)) return ('NA')
  if(n<0) return (-1)
  return (+1)
}

QSignFunc2 <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[2]) return (-1)
  if(n>p[2] & n<=p[3]) return (0)
  return (+1)
}

#Qualy2: absolute value of the step
#We qualify the absolut value of returns (ccrend): 4 levels
#Quantile categories
QStepFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[2]) return (0)
  if(n>p[2] & n<=p[3]) return (1)
  if(n>p[3] & n<=p[4]) return (2)
  return (3)
}

#Deciles: absolute value of the step
#We qualify the absolut value of returns: 9 levels
#Quantile categories
QDecFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[1]) return (0)
  if(n>p[2] & n<=p[3]) return (1)
  if(n>p[3] & n<=p[4]) return (2)
  if(n>p[4] & n<=p[5]) return (3)
  if(n>p[5] & n<=p[6]) return (4)
  if(n>p[6] & n<=p[7]) return (5)
  if(n>p[7] & n<=p[8]) return (6)
  if(n>p[8] & n<=p[9]) return (7)
  return (8)
}

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Confussion matrix qualifications
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
PPV <- function(x){x[4]/(x[4]+x[2])} #Positive Predictive Value PPv=TP/(TP+FP)
Pgain<-function(x){(x[3]+x[4])/sum(x)} #Gain probability = (TP+FN)/(TP+FP+TN+FN)=(TP+FN)/N

##Return table computation (Continous Compound Multi-Period Interest Return)
##Sequence:
## We purchase a crypto if we predict is going up tomorrow
## We hold a crypto of we predict that is 
CCIRFun <- function(result.mod){ #result.mod is a data.frame (Date, Ticker, Rt, ccReturn, PRED, TEST,..)
  result.mod$PRED <- as.character(result.mod$PRED)
  result.mod$TEST <- as.character(result.mod$TEST)
  aaa <- (result.mod$PRED=='1')
  result.mod$ccReturnBuy <- c(0)
  result.mod$RtBuy <- c(0)
  result.mod[aaa,]$ccReturnBuy <- result.mod[aaa,]$ccReturn
  result.mod[aaa,]$RtBuy <- result.mod[aaa,]$Rt
  #result.mod <- result.mod[result.mod$ccReturnBuy!=0,]
  result.mod$acumccReturn <- ave(result.mod$ccReturnBuy, result.mod$Date, FUN = cumsum)
  return (result.mod)
}

createTimeSlices <- function(y, initialWindow, horizon = 1, fixedWindow = TRUE, skip = 0) {
  ## initialwindow = initial number of consecutive values in each training set sample
  ## horizon = number of consecutive values in test set sample
  ## fixedwindow = FALSE if we use the maximum possible length for the training set
  ## Ensure that initialwindow + horizon <= length(y)
  
  #stops <- seq(initialWindow, (length(y) - horizon), by = skip + 1)
  stops <- seq(initialWindow, (nrow(y) - horizon), by = skip + 1)
  
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  } else {
    starts <- rep(1, length(stops)) # all start at 1
  }
  
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops+1, stops+horizon, SIMPLIFY = FALSE)
  nums <- gsub(" ", "0", format(stops))
  names(train) <- paste("Training", nums, sep = "")
  names(test) <- paste("Testing", nums, sep = "")
  
  out <- list(train = train, test = test)
  
  return(out)
}

#Funtion for detailed analysis of the association for the different values of categoriacal variables
residPearson.mtx <- function(chiQres.mtx, chiQobs.mtx){ #The parameter are the residuals of the contigency table
  maxcol <- max.col(abs(chiQres.mtx)) #We filter by the maxium residual values
  nr <- dim(chiQres.mtx)[1]
  i <- 1:nr
  #techno <- data.frame(Alg=character(), Cons=character(), res=numeric(), stringsAsFactors = FALSE)
  temp.res <- vapply(1:nr,function(x) {chiQres.mtx[x,maxcol[x]]}, numeric(1))
  temp.obs<- vapply(1:nr,function(x) {chiQobs.mtx[x,maxcol[x]]}, numeric(1))
  techno <-cbind(rownames(chiQres.mtx)[i], colnames(chiQres.mtx)[maxcol[i]], temp.res, temp.obs)        
  colnames(techno) <- c('Var1','Var2','res','obs')
  row.names(techno)<-c()
  
  techno <- as.data.frame(techno, stringsAsFactors = FALSE)
  techno$res <- as.numeric(techno$res)
  techno$obs <- as.integer(techno$obs)
  techno <- techno[order(techno$res, decreasing = TRUE),]
  techno <- techno[order(-abs(techno$res)),] #Dataframe ordenado por residuo
  return(techno)
} 

#Function to compute the bascket value (Portfolio) with fKelly criterion
MyPortK <- function(Basket,budgInic=100000,Rf=0L,fr=0L){#Rf: return free-risk; fr: market friction
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return
  Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    #print(as.Date(day))
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4,11)) #Date, Ticker, PriceYest, Rt, fKelly1Y
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    longPos.day<-data.frame()
    shortPos.day<-data.frame()
    Equity.day <- Equity
    MyBudg.day <- data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      #print(i)
      nc <- floor(obs$fKelly1Y*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      if (nc==0){next}
      longPos <- nc*obs$PriceYest
      shortPos <- (obs$Rt + 1) * longPos
      Rt <- (shortPos-longPos)/longPos
      Equity.day <- Equity.day - longPos
      longPos.day <- rbind(longPos.day,data.frame(obs$Ticker,longPos)) #Cost of the investment
      shortPos.day <- rbind(shortPos.day,data.frame(obs$Ticker,shortPos)) #Refund of investment at the end of the day
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos,Rt))
    }
    fees <- fr*(sum(longPos.day[2]) + sum(shortPos.day[2])) #Fee charged per day
    #MyBudg.day$Reff <- (sum(shortPos.day[2])-sum(longPos.day[2]-fees))/sum(shortPos.day[2])
    #W <- MyBudg.day$longPos / sum(longPos.day[2]) #W is a vector with all the weights
    W <- MyBudg.day$longPos / Equity #W is a vector with all the weights
    Reff <- (sum(shortPos.day[2])-sum(longPos.day[2])-fees)/sum(longPos.day[2])
    MyBudg.day$Rt <- W*(MyBudg.day$Rt) #We compute the weight nominal exceed risk-free interest rate
    Equity <- Equity - sum(longPos.day[2]) + sum(shortPos.day[2]) - fees #We refund at the end of the day every day (we sell the portfolio)
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, Equity))
  }
  #names(MyEquity)<-c('Date','EquityEoD')
  #portf <- merge(MyBudg,MyEquity, all.x = TRUE,by.x='obs.Date',by.y = 'Date')
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','Equity')
  return(MyBudg)
}

#Function to compute the bascket value (Portfolio) with Naive criterion
MyPortN <- function(Basket,budgInic=100000L,Rf=0L,fr=0L){
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return (Market Risk Premium)
  Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4)) #Date, Ticker, PriceYest, Rt
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    N <- length(unique(OneDay.df$Ticker))
    budgN <- Equity/N #With Naive criterion, we allocate same budget for each cryptocoin
    W <- 1/N #Weight of each cryptocoin
    longPos.day<-c(0)
    shortPos.day<-c(0)
    Equity.day <- Equity
    MyBudg.day<-data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      nc <- floor(W*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      if (nc==0){next}
      longPos <- nc*obs$PriceYest
      longPos.day <- longPos.day + longPos #Cost of the investment
      #Equity.day <- Equity.day - longPos
      shortPos <- (obs$Rt + 1) * longPos #Refund when we sell at the end of the day
      shortPos.day <- shortPos.day +  shortPos #Accumulated refund at the end of the day
      #Rt <- W*(shortPos-longPos)/longPos #We weight the returns
      Rt <- W*obs$Rt
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos, Rt))
    }
    fees <- fr*(longPos.day+shortPos.day) #Fee charged per day
    Equity <- Equity - longPos.day + shortPos.day - fees #We rebalance at the end of the day
    #MyBudg.day$Reff <- (shortPos.day-longPos.day-fees)/shortPos.day
    Reff <- (shortPos.day-longPos.day-fees)/longPos.day
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, Equity))
  }
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','Equity')
  return(MyBudg)
}

#Function to compute the bascket value (Portfolio) with Index criterion (unlimited budget)
MyPortI <- function(Basket,Rf=0L,fr=0L){
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return
  #Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4)) #Date, Ticker, PriceYest, Rt
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    #N <- length(unique(OneDay.df$Ticker))
    #budgN <- Equity/N #With Naive criterion, we allocate same budget for each cryptocoin
    #W <- 1/N #Weight of each cryptocoin
    longPos.day<-c(0)
    shortPos.day<-c(0)
    Equity.day <- sum(OneDay.df$PriceYest)
    MyBudg.day <- data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      #nc <- floor(W*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      nc <- c(1) #We bought one of each cryptocoin at the beginning of the day 
      #if (nc==0){next}
      longPos <- nc*obs$PriceYest
      longPos.day <- longPos.day + longPos #Cost of the investment
      #Equity.day <- Equity.day - longPos
      shortPos <- (obs$Rt + 1) * longPos
      shortPos.day <- shortPos.day +  shortPos #Refund of investment at the end of the day
      W <- obs$PriceYest / Equity.day
      #Rt <- W*(shortPos-longPos)/longPos #We weight the returns
      Rt <- W*obs$Rt
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos, Rt))
    }
    fees <- fr*(longPos.day+shortPos.day) #Fee charged per day
    #MyBudg.day$Reff <- (shortPos.day-longPos.day-fees)/shortPos.day
    Reff <- (shortPos.day-longPos.day-fees)/longPos.day
    DiffEquity <- Equity.day-shortPos.day-fees
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, DiffEquity))
  }
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','DiffEquity') #WERt: Weight Exceed Return
  return(MyBudg)
}

AggPortfRet <- function(portf){  # Returns of the market
  portf.dt <- as.data.table(portf)
  setkey(portf.dt,Date)
  AggRet <- portf.dt[,.(Rd=sum(WERt)),by=Date]
  AggRet$Reff <- portf.dt[J(unique(Date)),mult='first']$Reff
  names(AggRet)<-c('Date','SumWdER','Reff') #Sum of weight daily Exceed Return
  return(AggRet)
}
