#####################################################################
#                    Data Wrangling
#        Get, Clean and Transform of the Data
#####################################################################
# Source CRPTg3_DSv3.R
# We incorporate MOMENTUM Variables


library(reshape2)
library(data.table)
library(xtable)
library(TTR)   #Momentum variables

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/FasePortOpt/')
source('CryptFunctPortf.R')

CRYP.DS <- read.table("Datasets/DS_CRYP_TOT.csv",header=TRUE, sep=",",stringsAsFactors = FALSE) #1999954 obs
#xxx <- CRYP.DS[CRYP.DS$Symbol =='BTC',]
# We exclude coins that haven't traded in last 24 hours
# TOTALVOLUME24H is the amount the coin has been traded
# in 24 hours against ALL its trading pairs (8/6/2021)

#Format and datatype conversions:
CRYP.DS$time<-as.Date(CRYP.DS$time) 
CRYP.DS$close<-as.double(CRYP.DS$close)

#CRYP.DS$volumefrom<-as.double(CRYP.DS$volumefrom)
#CRYP.DS$volumeto<-as.double(CRYP.DS$volumeto)
CRYP.DT <- as.data.table(CRYP.DS[,c("time","Symbol","close")])
setnames(CRYP.DT,"Symbol","SYM")

##################################### VARIABLE DESCRIPTIONS ######################################
#Whether a conversion had to be used where a direct market wasn't available and which type of conversion 
#was used. You can learn more about conversion types here: 
#https://blog.cryptocompare.com/historical-data-api-update-4ee44c549a8f
#https://www.cryptocompare.com/coins/guides/glossary-of-trading-terms/
#https://ccc-api.cloudapp.net/faq
# Variables: close quoptation in EUR

#SYM: cryptocurrency ticker

# ##################  DATA TREATMENT ################################

# CLEANING 1: duplicated, NaN observations, Inf
CRYP.DT<- unique(CRYP.DT) #1517232 obs
#Simple Net return: Rt(t)=S(t)/S(t-1) - 1 = exp(Rg(t)) - 1
CRYP.RT <- CRYP.DT[,.(time, close, Rt = (close[-1] - close)/close), by=SYM]
CRYP.RT[,Rt:=c(NA, Rt[-.N]), by=SYM]
CRYP.RT <- CRYP.RT[CRYP.RT$Rt != "NA"] #1516016 obs
CRYP.RT <- na.omit(CRYP.RT) #994378 obs.
CRYP.RT <- CRYP.RT[is.finite(CRYP.RT$Rt),] #992477 / 4 variables

# HEAVY TAIL TESTS ###################################################
library(poweRlaw)

allcrypt <- unique(CRYP.RT$SYM) #1211 cryptocurrencies
PLD <- data.frame()
i=0
for (crypt in allcrypt){
        rend <- CRYP.RT[SYM %in% crypt]$Rt
        rend.nor <- Norm(rend)
        if ((sum(rend==0)/length(rend)) > 0.95) next
        rend.pos <- rend.nor[rend.nor>0] #Right tail
        rend.neg <- abs(rend.nor[rend.nor<0]) #Left tail
        
        if ((length(rend.pos) < 3) | (length(rend.neg) < 3)) next
        if ((sum(rend.pos==0)/length(rend.pos)) > 0.95 | (sum(rend.neg==0)/length(rend.neg)) > 0.95) next
        
        # Hill2 estimator
        #Positive tail
        m_bl =conpl$new(rend.pos)
        est = estimate_xmin(m_bl)
        m_bl$setXmin(est)
        hill2.p <- estimate_pars(m_bl)
        sd.p <- (hill2.p$pars - 1)/sqrt(length(rend.pos))
        
        #Negative tail
        m_bl =conpl$new(rend.neg)
        est = estimate_xmin(m_bl)
        m_bl$setXmin(est)
        hill2.n <- estimate_pars(m_bl)
        sd.n <- (hill2.n$pars - 1)/sqrt(length(rend.neg))
        
        #Power Law Distribution (PLD)
        PLD <- rbind(PLD,data.frame(crypt, hill2.p$pars, sd.p, hill2.n$pars, sd.n))
}

names(PLD)<-c('SYM','AlphaP','Sd.P','AlphaN','Sd.N')

# temporal2 <- PLD
# df1 <-temporal2[order(-temporal2$AlphaP),]
# df2 <-temporal2[order(-temporal2$Sd.P),]
# df3 <-temporal2[order(-temporal2$AlphaN),]
# df4 <-temporal2[order(-temporal2$Sd.N),]
# head(df1,10)
# head(df2,10)
# head(df3,10)
# head(df4,10)

# PLD.HQ <- PLD[PLD$AlphaP>2 & PLD$AlphaN>2 
#               & PLD$Sd.P<5 & PLD$Sd.N<5,] #988

PLD.HQ <- PLD[PLD$AlphaP>2.3 & PLD$AlphaN>2.3 
              & PLD$Sd.P<4 & PLD$Sd.N<4,] #756 high quality cryptocurrencies

nrow(PLD.HQ)

#################################################################################
#Filter-in of HQ cryptocurrencies
#################################################################################
CRYP.RT <- CRYP.RT[CRYP.RT$SYM %in% PLD.HQ$SYM] #657687 obs

CRYPSum.RT <- CRYP.RT[,.(Rtmean = mean(Rt), Rtmin = min(Rt), Rtmax=max(Rt), Rt90 = quantile(Rt,probs = c(0.90)), 
                         Rt75 = quantile(Rt,probs = c(0.75)), Rt25 = quantile(Rt,probs = c(0.25) ), 
                         Rt10 = quantile(Rt,probs = c(0.1)), Rtmedian = median(Rt), iqr = IQR(Rt), 
                         volatility = sd(Rt), n=.N),
                      by=SYM]
CRYPSum.RT$MkCRank <- as.numeric(row.names(CRYPSum.RT)) #756

CRYP.RT <- merge(CRYP.RT, CRYPSum.RT[,c('SYM','MkCRank')], all.x = TRUE)
CRYP.RT <- setorderv(CRYP.RT, cols=c('MkCRank','time'), order=c(1,1))

#################################################################################
#                         CCI30 INDEX TREATMENT                                 #
#################################################################################
#library('xts')
#library('PerformanceAnalytics')

# Data Transformation of time-series to use Portfolio library
CCI30Index <- read.table("Datasets/cci30_OHLCV.csv",header=TRUE, sep=",")
# CCI30Index_zoo <- read.csv.zoo('Datasets/cci30_OHLCV.csv', header=TRUE, sep=',', format='%Y-%m-%d')
# CCI30Index_xts <- as.xts(CCI30Index_zoo)
# 
# CCI30Index_xts$RtSP <- Return.calculate(CCI30Index_xts$Close)

CCI30Index$Date <- as.Date(CCI30Index$Date)
CCI30Index.DT <- as.data.table(CCI30Index)
CCI30Index.DT <- CCI30Index.DT[order(-rank(Date))]


# Transform daily returns to continous daily returns transforming by Log (ln) function
CCI30Index.DT <- CCI30Index.DT[,.(Date, Close, RtCCI30 = (Close - Close[-1])/Close[-1])]
setnames(CCI30Index.DT,'Date','time')
setorderv(CCI30Index.DT, cols = c('time'))

###################################################################################################
####             Table transformations: from long-format to wide-format                        ####
####             Variables: CLOSE, VOLUME and REND                                             ####
####Comments: some of the following tables will be used by the different clustering R functions####
###################################################################################################


# CRYP.wDT.close <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var = "close")
# CRYP.wDT.volume <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var ="volume")
CRYP.wDT.Rt <- dcast.data.table(CRYP.RT, time ~ SYM, value.var ="Rt")

# CRYP.wDF.close <- as.data.frame(CRYP.wDT.close) 
# CRYP.wDF.volume <- as.data.frame(CRYP.wDT.volume) 
CRYP.wDF.Rt <- as.data.frame(CRYP.wDT.Rt) 

CRYP.timeWise.Rt <- t(CRYP.wDT.Rt)
colnames(CRYP.timeWise.Rt) <- CRYP.timeWise.Rt[1,]
CRYP.timeWise.Rt <- CRYP.timeWise.Rt[-1,]
# CRYP.timeWise.price <- t(CRYP.wDT.close)
# colnames(CRYP.timeWise.price) <- CRYP.timeWise.price[1,]
# CRYP.timeWise.price <- CRYP.timeWise.price[-1,]

temp1 <- unlist(CRYP.timeWise.Rt)
temp2 <- as.numeric(temp1)
temp3 <- matrix(temp2, byrow = TRUE, nrow = nrow(temp1))
colnames(temp3) <- colnames(CRYP.timeWise.Rt)
rownames(temp3) <- rownames(CRYP.timeWise.Rt) 
CRYP.timeWise.Rt <- temp3

### End of CCI30 Index treatment ################################################

########### Fixed rate investment instruments ############################
############## U.S. Treasury Bill 90 days #####################################
#Continous Compound Daily Risk Free interest (Annualized over90 days U.S T-Bill)
#Under the assumption that on Treasury 90 days bill yields are quoted as Effective Annual Rate (EAR)
DTB90.DS <- read.table("Datasets/DTB3.csv",header=TRUE, sep=",")
DTB90.DT <- as.data.table(DTB90.DS)
names(DTB90.DT) <- c('Date', 'TBEAR')
DTB90.DT$Date <- as.Date(DTB90.DT$Date, tryFormats=c("%Y-%m-%d"))
DTB90.DT$TBEAR <- as.numeric(DTB90.DT$TBEAR)
DTB90.DT <- DTB90.DT[complete.cases(DTB90.DT),]
DTB90.DT <- DTB90.DT[,.(Date, TBEAR, dTB=(1+TBEAR/100)**(1/365) - 1)]

######## VARIABLES DE USO POR OTROS SCRIPTS #####################################

# Dataset 2018-21
nfich1 <- c("Datasets/CRYPT_DS_TOT3.RData")
# save(CRYP.DS, CRYP.DT,CRYP.RT, CRYPSum.RT,
#      PLD, PLD.HQ,
#      CCI30Index.DT, DTB90.DT,
#      CRYP.wDT.Rt, CRYP.wDF.Rt, CRYP.timeWise.Rt,
#        file=nfich1)
#load(nfich1)

