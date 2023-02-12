########################################################
#        Charts of the different strategies            #
########################################################
#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/FasePortOpt/')

# Dataset 2018-21 (851 investing days)
nfich1 <- c("Datasets/FeatBUILDminTC3.RData")
load(nfich1)

# Risl label
nfich2 <- c("Datasets/TrainingRisk.RData") #RiskQ.tab
load(nfich2)

#Strategies
nfich3 <- c("Datasets/Strategiesmin13TC3.RData")
load(nfich3)

#Functions
source('CryptFunctPortf.R')

#install.packages('ggridges')
#install.packages('ggplot2movies')
library(data.table)
library(ggplot2)
library(ggridges)
library(ggplot2movies)
library(gtools)

#############################################################################
#                          Distribution by Investor (30)                    #
#############################################################################

# Warning: We have to change the model labels (MV, SR, LR, MR and HR)
#prueba1 <- retGP.mtx.red
prueba1 <- retGP.mtx.MV
prueba1.df <- data.frame(prueba1)
colnames(prueba1.df) <- seq(1:ncol(prueba1.df))
prueba1.df$investor <- rownames(prueba1.df)

prueba1T.mtx <- t(prueba1.df[,1:ncmax])
prueba1T.df <- data.frame(prueba1T.mtx)
colnames(prueba1T.df)<- seq(1:30)
prueba1T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba1T.long <- prueba1T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
prueba1T.long$model <- 'MV'

##
#prueba2 <- retGP.bench.mtx.red
prueba2 <- retGP.mtx.IdX
prueba2.df <- data.frame(prueba2)
colnames(prueba2.df) <- seq(1:ncol(prueba2.df))
prueba2.df$investor <- rownames(prueba2.df)

prueba2T.mtx <- t(prueba2.df[,1:ncmax])
prueba2T.df <- data.frame(prueba2T.mtx)
colnames(prueba2T.df)<- seq(1:30)
prueba2T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba2T.long <- prueba2T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
prueba2T.long$model <- 'IdX'

#################################################
### SR Strategy #################################
#################################################
#prueba3 <- retGP.clust.mtx.red
prueba3 <- retGP.mtx.SR
prueba3.df <- data.frame(prueba3)
colnames(prueba3.df) <- seq(1:ncol(prueba3.df))
prueba3.df$investor <- rownames(prueba3.df)

prueba3T.mtx <- t(prueba3.df[,1:ncmax])
prueba3T.df <- data.frame(prueba3T.mtx)
colnames(prueba3T.df)<- seq(1:30)
prueba3T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba3T.long <- prueba3T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
#prueba3T.long$model <- 'Clust'
#prueba3T.long$model <- 'MV.SR'
prueba3T.long$model <- 'SR'

pruebaT.long <- data.frame()
pruebaT.long <- rbind(pruebaT.long, prueba1T.long)
pruebaT.long <- rbind(pruebaT.long, prueba2T.long)
pruebaT.long <- rbind(pruebaT.long, prueba3T.long)

colnames(pruebaT.long) <- c('Investor','Ret','Model')

pruebaT.long$Model <- factor(pruebaT.long$Model, levels = c('IdX','MV','SR'))
pruebaT.long$Investor <- as.numeric(pruebaT.long$Investor)
#prueba.long$Period<- chr(prueba.long$Period + 64)
#prueba.long$Period<- as.character(prueba.long$Period)
ind <- (pruebaT.long$Investor < 10)
aaa <- paste0('0',pruebaT.long$Investor)
pruebaT.long[ind,]$Investor <- aaa[ind]

# labels = c("Clust", "Idx","MV.Clust")
# ggplot(pruebaT.long, aes(x = Ret, y = Investor, color = Model, point_color = Model, fill = Model)) +
#   geom_density_ridges(
#     jittered_points = TRUE, scale = .95, rel_min_height = .01,
#     point_shape = "|", point_size = 3, size = 0.25,
#     position = position_points_jitter(height = 0)
#   ) +
#   scale_y_discrete(expand = c(0.01, 0)) +
#   scale_x_continuous(expand = c(0, 0), name = "returns") +
#   scale_fill_manual(values = c("#32248D","#FFB5C5", "#996CA9"),labels = c("Clust", "Idx","MV")) +
#   scale_color_manual(values = c("#32248D","#FFB5C5", "#996CA9"), guide = "none") +
#   scale_discrete_manual("point_color", values = c("#32248D","#FFB5C5", "#996CA9"), guide = "none") +
#   coord_cartesian(clip = "off") +
#   guides(fill = guide_legend(
#     override.aes = list(
#       fill = c("#32248D","#FFB5C5", "#996CA9"),
#       color = NA, point_color = NA)
#   )
#   ) +
#   ggtitle("Density Returns by Investor: SR strategy (Strategy 2c)")  +
#   theme_ridges(center = TRUE)

#"#FFB5C5", "#996CA9","#32248D"
#"#32248D","#FFB5C5", "#996CA9"

# Detailed
ggplot(pruebaT.long, aes(x = Ret, y = Investor, fill = Model)) +
  geom_density_ridges(
    aes(point_color = Model, point_fill = Model, point_shape = Model),
    alpha = .2, point_alpha = 1, jittered_points = TRUE 
  ) + 
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

#################################################
### LR Strategy #################################
#################################################
#prueba3 <- retGP.clust.mtx.red
prueba3 <- retGP.mtx.LR
prueba3.df <- data.frame(prueba3)
colnames(prueba3.df) <- seq(1:ncol(prueba3.df))
prueba3.df$investor <- rownames(prueba3.df)

prueba3T.mtx <- t(prueba3.df[,1:ncmax])
prueba3T.df <- data.frame(prueba3T.mtx)
colnames(prueba3T.df)<- seq(1:30)
prueba3T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba3T.long <- prueba3T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
#prueba3T.long$model <- 'Clust'
#prueba3T.long$model <- 'MV.LR'
prueba3T.long$model <- 'LR'

pruebaT.long <- data.frame()
pruebaT.long <- rbind(pruebaT.long, prueba1T.long)
pruebaT.long <- rbind(pruebaT.long, prueba2T.long)
pruebaT.long <- rbind(pruebaT.long, prueba3T.long)

colnames(pruebaT.long) <- c('Investor','Ret','Model')

pruebaT.long$Model <- factor(pruebaT.long$Model,levels = c('IdX','MV','LR'))
pruebaT.long$Investor <- as.numeric(pruebaT.long$Investor)
#prueba.long$Period<- chr(prueba.long$Period + 64)
#prueba.long$Period<- as.character(prueba.long$Period)
ind <- (pruebaT.long$Investor < 10)
aaa <- paste0('0',pruebaT.long$Investor)
pruebaT.long[ind,]$Investor <- aaa[ind]

# Detailed
ggplot(pruebaT.long, aes(x = Ret, y = Investor, fill = Model)) +
  geom_density_ridges(
    aes(point_color = Model, point_fill = Model, point_shape = Model),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

### MR Strategy #################################
#prueba3 <- retGP.clust.mtx.red
prueba3 <- retGP.mtx.MR
prueba3.df <- data.frame(prueba3)
colnames(prueba3.df) <- seq(1:ncol(prueba3.df))
prueba3.df$investor <- rownames(prueba3.df)

prueba3T.mtx <- t(prueba3.df[,1:ncmax])
prueba3T.df <- data.frame(prueba3T.mtx)
colnames(prueba3T.df)<- seq(1:30)
prueba3T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba3T.long <- prueba3T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
#prueba3T.long$model <- 'Clust'
#prueba3T.long$model <- 'MV.MR'
prueba3T.long$model <- 'MR'

pruebaT.long <- data.frame()
pruebaT.long <- rbind(pruebaT.long, prueba1T.long)
pruebaT.long <- rbind(pruebaT.long, prueba2T.long)
pruebaT.long <- rbind(pruebaT.long, prueba3T.long)

colnames(pruebaT.long) <- c('Investor','Ret','Model')

pruebaT.long$Model <- factor(pruebaT.long$Model, levels = c('IdX','MV','MR'))
pruebaT.long$Investor <- as.numeric(pruebaT.long$Investor)
#prueba.long$Period<- chr(prueba.long$Period + 64)
#prueba.long$Period<- as.character(prueba.long$Period)
ind <- (pruebaT.long$Investor < 10)
aaa <- paste0('0',pruebaT.long$Investor)
pruebaT.long[ind,]$Investor <- aaa[ind]

# Detailed
ggplot(pruebaT.long, aes(x = Ret, y = Investor, fill = Model)) +
  geom_density_ridges(
    aes(point_color = Model, point_fill = Model, point_shape = Model),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))


### HR Strategy #################################
#prueba3 <- retGP.clust.mtx.red
prueba3 <- retGP.mtx.HR
prueba3.df <- data.frame(prueba3)
colnames(prueba3.df) <- seq(1:ncol(prueba3.df))
prueba3.df$investor <- rownames(prueba3.df)

prueba3T.mtx <- t(prueba3.df[,1:ncmax])
prueba3T.df <- data.frame(prueba3T.mtx)
colnames(prueba3T.df)<- seq(1:30)
prueba3T.df$Period <- seq(1:ncmax)
# StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
#   gather(key='variable',value = 'value',-Date)

prueba3T.long <- prueba3T.df[,1:30] %>% gather(key='variable', value = 'value')
#prueba1T.long$variable <- as.integer(prueba1T.long$variable)
#prueba3T.long$model <- 'Clust'
#prueba3T.long$model <- 'MV.HR'
prueba3T.long$model <- 'HR'

pruebaT.long <- data.frame()
pruebaT.long <- rbind(pruebaT.long, prueba1T.long)
pruebaT.long <- rbind(pruebaT.long, prueba2T.long)
pruebaT.long <- rbind(pruebaT.long, prueba3T.long)

colnames(pruebaT.long) <- c('Investor','Ret','Model')

pruebaT.long$Model <- factor(pruebaT.long$Model, levels = c('IdX','MV','HR'))
pruebaT.long$Investor <- as.numeric(pruebaT.long$Investor)
#prueba.long$Period<- chr(prueba.long$Period + 64)
#prueba.long$Period<- as.character(prueba.long$Period)
ind <- (pruebaT.long$Investor < 10)
aaa <- paste0('0',pruebaT.long$Investor)
pruebaT.long[ind,]$Investor <- aaa[ind]

# Detailed
ggplot(pruebaT.long, aes(x = Ret, y = Investor, fill = Model)) +
  geom_density_ridges(
    aes(point_color = Model, point_fill = Model, point_shape = Model),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

############################################################################
###                     DAWass: HISTOGRAMS                               ###
############################################################################
# Distribution by investor (30)
library(HistDAWass)

# prueba1 <- retGP.mtx.red
# prueba1.df <- data.frame(prueba1)
# colnames(prueba1.df) <- seq(1:ncol(prueba1.df))
# prueba1.df$investor <- rownames(prueba1.df)
# 
# prueba1T.mtx <- t(prueba1.df[,1:ncmax])
# prueba1T.df <- data.frame(prueba1T.mtx)
# colnames(prueba1T.df)<- seq(1:30)
# prueba1T.df$Period <- seq(1:ncmax)
# # StrategAll.long <- StrategAll.df[StrategAll.df$Date>='2019-02-17'&StrategAll.df$Date<='2019-02-23',] %>% 
# #   gather(key='variable',value = 'value',-Date)
# 
# prueba1T.long <- prueba1T.df[,1:30] %>% gather(key='variable', value = 'value')
# #prueba1T.long$variable <- as.integer(prueba1T.long$variable)
# prueba1T.long$model <- 'MV'

#Function for making a Histogram-list and a Histogram-matrix object (Hist.lst and Hist.mtx)

Mydf.long <- pruebaT.long
Mydf.long$Investor <- as.numeric(Mydf.long$Investor)
idx <- 4
Model <- 'Clus'
funcDistributionH <- function(idx, Mydf){
  #investor = idx
  InvHist <- data2hist(Mydf.long[Mydf.long$Investor==idx,]$Ret, algo = 'FixedQuantiles')
  return(InvHist)
}

m <- ncol(prueba1.df)-1 #Number of investing periods
m <- ncol(prueba1T.df)-1#Number of investors

Hist.lst <- lapply(1:m, funcDistributionH, Mydf = Mydf.long)
Hist.mtx <- MatH(Hist.lst, nrows = m)
plot(Hist.mtx)
WH.vec.mean(Hist.mtx[,1])

############################################################################
###                     Centroid trajectories                            ###
############################################################################

library(plotly)
library(magrittr)
library(RSelenium)

#Feat.tab.filtered <- Feat.tab[Feat.tab$retGP.clust>=-1.5 & Feat.tab$retGP.clust<=1.5,]
BaseInv <- 1.0
Feat.tab.trj <- Feat.tab[,c('dateTestStart','dateTestEnd','dateTrainStart','dateTrainEnd',
                                 'WBaseInv','WBaseInv.clust',
                                 'accumGainTest','accumGainTest.clust',
                                 'retGP','cardGP','retGP.clust','cardGP.clust','benchTest',
                                 'SRTrain','SRTrain.clust',
                                 'SRTest','SRTest.clust',
                                 'RtMeanTrain.clust','RtVolTrain.clust',
                                 'clust',
                                 'RtVolTrain.med','RtMeanTrain.med','portfTestCentrVol',        
                                 'portfTestCentrRet', 'portfTrainCentrVol', 'portfTrainCentrRet',
                                 'RtVolTrain.clust','RtMeanTrain.clust',
                                 'portfTestCentroVol.clust','portfTestCentroRet.clust',
                                 'portfTrainCentroVol.clust','portfTrainCentroRet.clust'
                            
                            )]
Feat.tab.trj <- merge(Feat.tab.trj, RiskQ.tab, all.x = TRUE, by.x = 'dateTrainEnd', by.y = 'dateTrainEnd')

Feat.tab.trj <- data.table(Feat.tab.trj)
Feat.tab.trj$Num <- as.numeric(rownames(Feat.tab.trj))
Feat.tab.trj <- Feat.tab.trj[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                          WBaseInv, WBaseInv.clust,
                                          accumGainTest, accumGainTest.clust, 
                                          retGP, retGP.clust, retGP.bench = (benchTest-BaseInv)/BaseInv, 
                                          SRTrain, SRTrain.clust, SRTest, SRTest.clust,
                                          RtMeanTrain.clust, RtVolTrain.clust, quart1st, quart2nd, quart3rd,
                                          Risk = quantFunc2(RtVolTrain.clust, quart1st, quart2nd, quart3rd),
                                          cardGP, cardGP.clust,
                                          clust,
                                          RtVolTrain.med,RtMeanTrain.med,portfTestCentrVol,        
                                          portfTestCentrRet, portfTrainCentrVol, portfTrainCentrRet,
                                          RtVolTrain.clust,RtMeanTrain.clust,
                                          portfTestCentroVol.clust,portfTestCentroRet.clust,
                                          portfTrainCentroVol.clust,portfTrainCentroRet.clust), 
                             by = Num]

###############
# Strategy 1: #
###############

Strateg1.tab.trj <- Feat.tab.trj

#Strateg1.tab <- data.table(Strateg1.tab)
Strateg1.tab.trj <- Strateg1.tab.trj[Strateg1.tab.trj[,.I[which.max(SRTrain.clust)], by=dateTestStart]$V1]
#Strateg1.tab$retGP.bench <- (Strateg1.tab$benchTest-BaseInv)/BaseInv
Strateg1.tab.trj <- Strateg1.tab.trj[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                        RtVolTrain.med, RtMeanTrain.med, 
                                        portfTestCentrVol, portfTestCentrRet, 
                                        portfTrainCentrVol, portfTrainCentrRet,
                                        RtVolTrain.clust, RtMeanTrain.clust,
                                        portfTestCentroVol.clust, portfTestCentroRet.clust,
                                        portfTrainCentroVol.clust, portfTrainCentroRet.clust,
                                        clust)]

Strateg1.df.trj <- as.data.frame(Strateg1.tab.trj)
StrategAll.df.trj <- Strateg1.tab.trj[,c('dateTestEnd','portfTestCentrVol', 'portfTestCentrRet',
                                         'portfTestCentroVol.clust', 'portfTestCentroRet.clust')]
colnames(StrategAll.df.trj) <- c('Date','MV.vol','MV.ret','Strg1.vol','Strg1.ret')

Strateg1.df.long <- Strateg1.df.trj[,c('dateTestEnd','RtVolTrain.med', 'RtMeanTrain.med')]
Strateg1.df.long$Centroids <- c('MVTrain')
#Strateg1.df.long$Centroids <- c('MV')
colnames(Strateg1.df.long) <- c('Date','Volatility','MeanReturn','Centroids')

temp <- Strateg1.df.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust')]
temp$Centroids <- c('ClustTrain')
#temp$Centroids <- c('SRTrain')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg1.df.long <- rbind(Strateg1.df.long,temp)

temp <- Strateg1.df.trj[,c('dateTestEnd','portfTestCentrVol', 'portfTestCentrRet')]
temp$Centroids <- c('PortfTestMV')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg1.df.long <- rbind(Strateg1.df.long,temp)

temp <- Strateg1.df.trj[,c('dateTestEnd','portfTestCentroVol.clust', 'portfTestCentroRet.clust')]
temp$Centroids <- c('PortfTestClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg1.df.long <- rbind(Strateg1.df.long,temp)

temp <- Strateg1.df.trj[,c('dateTestEnd','portfTrainCentrVol', 'portfTrainCentrRet')]
temp$Centroids <- c('PortfTrainAll')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg1.df.long <- rbind(Strateg1.df.long,temp)

temp <- Strateg1.df.trj[,c('dateTestEnd','portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')]
temp$Centroids <- c('PortfTrainClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg1.df.long <- rbind(Strateg1.df.long,temp)

ggplot(Strateg1.df.long, aes( x = Volatility, y = MeanReturn, color = Centroids))+
  geom_path(aes(group = Centroids)) +
  ggtitle("Centroid trajectories: SR strategy (Strategy 1)")

#################################
## DENSITY PLOTS (Strategy 1)  ##
#################################


### MV and Clust TRAIN
cl1 <- subset(Strateg1.df.long, Centroids == 'MVTrain' | Centroids == 'ClustTrain',select = c(Volatility,MeanReturn, Centroids))
fig <- plot_ly(cl1,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) 
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig

### Portfolio centroids ##########
cl2 <- subset(Strateg1.df.long, Centroids == 'PortfTestClust' | Centroids == 'PortfTestMV' | 
                Centroids == 'PortfTrainAll' | Centroids == 'PortfTrainClust',
              select = c(Volatility, MeanReturn, Centroids))
fig <- plot_ly(cl2,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
  layout(xaxis = list(range = c(0.0,0.01), title = 'Portfolio Volatility'), 
         yaxis = list(range = c(-0.001, 0.002),title = 'Portfolio Return'), 
         legend = list(title=list(text='<b> Strategies </b>'))) 
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=45)
fig



# ###
# cl3 <- subset(Strateg1.df.long, Space == 'PortfTestAll',select = c(Volatility,MeanReturn))
# fig <- plot_ly(x=cl3$Volatility, y=cl3$MeanReturn,showscale = F,autocolorscale=T)
# fig <- fig %>% add_markers(alpha=0.4)
# temp <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=5)
# fig
# 
# cl4 <- subset(Strateg1.df.long, Space == 'PortfTestClust',select = c(Volatility,MeanReturn))
# fig <- plot_ly(x=cl4$Volatility, y=cl4$MeanReturn,showscale = F,autocolorscale=T)
# fig <- fig %>% add_markers(alpha=0.4)
# temp <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
# fig

################
# Strategy 2a: #
################
Strateg2.tab.trj <- Feat.tab.trj
Strateg2.aux <- unique(Feat.tab.trj[,c('dateTestStart', 'dateTestEnd', 'dateTrainStart', 'dateTrainEnd',
                                       'RtVolTrain.med', 'RtMeanTrain.med', 
                                       'portfTestCentrVol', 'portfTestCentrRet', 
                                       'portfTrainCentrVol', 'portfTrainCentrRet'
                                       )])

Strateg2a.tab.trj <- Strateg2.tab.trj[Risk=='Low',]
Strateg2a.tab.trj <- Strateg2a.tab.trj[Strateg2a.tab.trj[,.I[which.max(SRTrain.clust)], by=dateTestStart]$V1]
Strateg2a.tab.trj <- merge(Strateg2.aux, Strateg2a.tab.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust',
                                                              'portfTestCentroVol.clust', 'portfTestCentroRet.clust',
                                                              'portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')], 
                     all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')

Strateg2a.tab.trj <- Strateg2a.tab.trj[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                        RtVolTrain.med, RtMeanTrain.med, 
                                        portfTestCentrVol, portfTestCentrRet, 
                                        portfTrainCentrVol, portfTrainCentrRet,
                                        RtVolTrain.clust, RtMeanTrain.clust,
                                        portfTestCentroVol.clust, portfTestCentroRet.clust,
                                        portfTrainCentroVol.clust, portfTrainCentroRet.clust)]

Strateg2a.df.trj <- as.data.frame(Strateg2a.tab.trj)
StrategAll.df.trj <- merge(StrategAll.df.trj,Strateg2a.df.trj[,c('dateTestEnd',
                                                                 'portfTestCentroVol.clust', 'portfTestCentroRet.clust')], all.x=TRUE,
                       by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df.trj) <- c('Date','MV.vol','MV.ret','Strg1.vol','Strg1.ret',
                                 'Strg2a.vol','Strg2a.ret')

Strateg2a.df.long <- Strateg2a.df.trj[,c('dateTestEnd','RtVolTrain.med', 'RtMeanTrain.med')]
Strateg2a.df.long$Centroids <- c('MVTrain')
colnames(Strateg2a.df.long) <- c('Date','Volatility','MeanReturn','Centroids')

temp <- Strateg2a.df.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust')]
temp$Centroids <- c('ClustTrain')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2a.df.long <- rbind(Strateg2a.df.long,temp)

temp <- Strateg2a.df.trj[,c('dateTestEnd','portfTestCentrVol', 'portfTestCentrRet')]
temp$Centroids <- c('PortfTestMV')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2a.df.long <- rbind(Strateg2a.df.long,temp)

temp <- Strateg2a.df.trj[,c('dateTestEnd','portfTestCentroVol.clust', 'portfTestCentroRet.clust')]
temp$Centroids <- c('PortfTestClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2a.df.long <- rbind(Strateg2a.df.long,temp)

temp <- Strateg2a.df.trj[,c('dateTestEnd','portfTrainCentrVol', 'portfTrainCentrRet')]
temp$Centroids <- c('PortfTrainAll')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2a.df.long <- rbind(Strateg2a.df.long,temp)

temp <- Strateg2a.df.trj[,c('dateTestEnd','portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')]
temp$Centroids <- c('PortfTrainClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2a.df.long <- rbind(Strateg2a.df.long,temp)

ggplot(Strateg2a.df.long, aes( x = Volatility, y = MeanReturn, color = Centroids))+
  geom_path(aes(group = Centroids)) +
  ggtitle("Centroid trajectories: LR strategy (Strategy 2a)")

#################################
## DENSITY PLOTS (Strategy 2a) ##
#################################

### MV and Clust TRAIN
cl1 <- subset(Strateg2a.df.long, Centroids == 'MVTrain' | Centroids == 'ClustTrain',select = c(Volatility,MeanReturn, Centroids))
fig <- plot_ly(cl1,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids)
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig


### Portfolio centroids ##########
cl2 <- subset(Strateg2a.df.long, Centroids == 'PortfTestClust' | Centroids == 'PortfTestMV' | 
                Centroids == 'PortfTrainAll' | Centroids == 'PortfTrainClust',
              select = c(Volatility, MeanReturn, Centroids))
fig <- plot_ly(cl2,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
  layout(xaxis = list(range = c(0.0,0.01),title = 'Portfolio Volatility'),
         yaxis = list(range = c(-0.001, 0.002),title = 'Portfolio Return'),
         legend = list(title=list(text='<b> Strategies </b>')))
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=45)
fig

################
# Strategy 2b: #
################

Strateg2b.tab.trj <- Strateg2.tab.trj[Risk=='Average',]
Strateg2b.tab.trj <- Strateg2b.tab.trj[Strateg2b.tab.trj[,.I[which.max(SRTrain.clust)], by=dateTestStart]$V1]
Strateg2b.tab.trj <- merge(Strateg2.aux, Strateg2b.tab.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust',
                                                              'portfTestCentroVol.clust', 'portfTestCentroRet.clust',
                                                              'portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')], 
                           all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')

Strateg2b.tab.trj <- Strateg2b.tab.trj[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                          RtVolTrain.med, RtMeanTrain.med, 
                                          portfTestCentrVol, portfTestCentrRet, 
                                          portfTrainCentrVol, portfTrainCentrRet,
                                          RtVolTrain.clust, RtMeanTrain.clust,
                                          portfTestCentroVol.clust, portfTestCentroRet.clust,
                                          portfTrainCentroVol.clust, portfTrainCentroRet.clust)]

Strateg2b.df.trj <- as.data.frame(Strateg2b.tab.trj)
StrategAll.df.trj <- merge(StrategAll.df.trj,Strateg2b.df.trj[,c('dateTestEnd',
                                                                 'portfTestCentroVol.clust', 'portfTestCentroRet.clust')], all.x=TRUE,
                           by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df.trj) <- c('Date','MV.vol','MV.ret','Strg1.vol','Strg1.ret',
                                 'Strg2a.vol','Strg2a.ret',
                                 'Strg2b.vol','Strg2b.ret')

Strateg2b.df.long <- Strateg2b.df.trj[,c('dateTestEnd','RtVolTrain.med', 'RtMeanTrain.med')]
Strateg2b.df.long$Centroids <- c('MVTrain')
colnames(Strateg2b.df.long) <- c('Date','Volatility','MeanReturn','Centroids')

temp <- Strateg2b.df.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust')]
temp$Centroids <- c('ClustTrain')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2b.df.long <- rbind(Strateg2b.df.long,temp)

temp <- Strateg2b.df.trj[,c('dateTestEnd','portfTestCentrVol', 'portfTestCentrRet')]
temp$Centroids <- c('PortfTestMV')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2b.df.long <- rbind(Strateg2b.df.long,temp)

temp <- Strateg2b.df.trj[,c('dateTestEnd','portfTestCentroVol.clust', 'portfTestCentroRet.clust')]
temp$Centroids <- c('PortfTestClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2b.df.long <- rbind(Strateg2b.df.long,temp)

temp <- Strateg2b.df.trj[,c('dateTestEnd','portfTrainCentrVol', 'portfTrainCentrRet')]
temp$Centroids <- c('PortfTrainAll')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2b.df.long <- rbind(Strateg2b.df.long,temp)

temp <- Strateg2b.df.trj[,c('dateTestEnd','portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')]
temp$Centroids <- c('PortfTrainClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2b.df.long <- rbind(Strateg2b.df.long,temp)

ggplot(Strateg2b.df.long, aes( x = Volatility, y = MeanReturn, color = Centroids))+
  geom_path(aes(group = Centroids)) +
  ggtitle("Centroid trajectories: MR strategy (Strategy 2b)")

#################################
## DENSITY PLOTS (Strategy 2b) ##
#################################

### MV and Clust TRAIN
cl1 <- subset(Strateg2b.df.long, Centroids == 'MVTrain' | Centroids == 'ClustTrain',select = c(Volatility,MeanReturn, Centroids))
fig <- plot_ly(cl1,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids)
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig


### Portfolio centroids ##########
cl2 <- subset(Strateg2b.df.long, Centroids == 'PortfTestClust' | Centroids == 'PortfTestMV' | 
                Centroids == 'PortfTrainAll' | Centroids == 'PortfTrainClust',
              select = c(Volatility, MeanReturn, Centroids))
fig <- plot_ly(cl2,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
  layout(xaxis = list(range = c(0.0,0.008),title = 'Portfolio Volatility'),
         yaxis = list(range = c(-0.001, 0.002),title = 'Portfolio Return'))
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=45)
fig

################
# Strategy 2c: #
################

Strateg2c.tab.trj <- Strateg2.tab.trj[Risk=='High',]
Strateg2c.tab.trj <- Strateg2c.tab.trj[Strateg2c.tab.trj[,.I[which.max(SRTrain.clust)], by=dateTestStart]$V1]
Strateg2c.tab.trj <- merge(Strateg2.aux, Strateg2c.tab.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust',
                                                              'portfTestCentroVol.clust', 'portfTestCentroRet.clust',
                                                              'portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')], 
                           all.x = TRUE, by.x = 'dateTestEnd', by.y = 'dateTestEnd')

Strateg2c.tab.trj <- Strateg2c.tab.trj[,.(dateTestStart, dateTestEnd, dateTrainStart, dateTrainEnd,
                                          RtVolTrain.med, RtMeanTrain.med, 
                                          portfTestCentrVol, portfTestCentrRet, 
                                          portfTrainCentrVol, portfTrainCentrRet,
                                          RtVolTrain.clust, RtMeanTrain.clust,
                                          portfTestCentroVol.clust, portfTestCentroRet.clust,
                                          portfTrainCentroVol.clust, portfTrainCentroRet.clust)]

Strateg2c.df.trj <- as.data.frame(Strateg2c.tab.trj)
StrategAll.df.trj <- merge(StrategAll.df.trj,Strateg2c.df.trj[,c('dateTestEnd',
                                                                 'portfTestCentroVol.clust', 'portfTestCentroRet.clust')], all.x=TRUE,
                           by.x = 'Date', by.y = 'dateTestEnd')
colnames(StrategAll.df.trj) <- c('Date','MV.vol','MV.ret','Strg1.vol','Strg1.ret',
                                 'Strg2a.vol','Strg2a.ret',
                                 'Strg2b.vol','Strg2b.ret',
                                 'Strg2c.vol','Strg2c.ret')

Strateg2c.df.long <- Strateg2c.df.trj[,c('dateTestEnd','RtVolTrain.med', 'RtMeanTrain.med')]
Strateg2c.df.long$Centroids <- c('MVTrain')
colnames(Strateg2c.df.long) <- c('Date','Volatility','MeanReturn','Centroids')

temp <- Strateg2c.df.trj[,c('dateTestEnd','RtVolTrain.clust', 'RtMeanTrain.clust')]
temp$Centroids <- c('ClustTrain')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2c.df.long <- rbind(Strateg2c.df.long,temp)

temp <- Strateg2c.df.trj[,c('dateTestEnd','portfTestCentrVol', 'portfTestCentrRet')]
temp$Centroids <- c('PortfTestMV')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2c.df.long <- rbind(Strateg2c.df.long,temp)

temp <- Strateg2c.df.trj[,c('dateTestEnd','portfTestCentroVol.clust', 'portfTestCentroRet.clust')]
temp$Centroids <- c('PortfTestClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2c.df.long <- rbind(Strateg2c.df.long,temp)

temp <- Strateg2c.df.trj[,c('dateTestEnd','portfTrainCentrVol', 'portfTrainCentrRet')]
temp$Centroids <- c('PortfTrainAll')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2c.df.long <- rbind(Strateg2c.df.long,temp)

temp <- Strateg2c.df.trj[,c('dateTestEnd','portfTrainCentroVol.clust', 'portfTrainCentroRet.clust')]
temp$Centroids <- c('PortfTrainClust')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

Strateg2c.df.long <- rbind(Strateg2c.df.long,temp)

ggplot(Strateg2c.df.long, aes( x = Volatility, y = MeanReturn, color = Centroids))+
  geom_path(aes(group = Centroids)) +
  ggtitle("Centroid trajectories: HR strategy (Strategy 2c)")

#################################
## DENSITY PLOTS (Strategy 2c) ##
#################################

### MV and Clust TRAIN
cl1 <- subset(Strateg2c.df.long, Centroids == 'MVTrain' | Centroids == 'ClustTrain',select = c(Volatility,MeanReturn, Centroids))
fig <- plot_ly(cl1,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids)
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig


### Portfolio centroids ##########
cl2 <- subset(Strateg2c.df.long, Centroids == 'PortfTestClust' | Centroids == 'PortfTestMV' | 
                Centroids == 'PortfTrainAll' | Centroids == 'PortfTrainClust',
              select = c(Volatility, MeanReturn, Centroids))
fig <- plot_ly(cl2,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
  layout(xaxis = list(range = c(0.0,0.008),title = 'Portfolio Volatility'),
         yaxis = list(range = c(-0.0005, 0.0015), title = 'Portfolio Return'),
         legend = list(title=list(text='<b> Strategies </b>')))
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=45)
fig


###################################
# ALL Strategies: Test Portfolios #
###################################

# StrategAll.df.trj <- merge(StrategAll.df.trj,Strateg2c.df.trj[,c('dateTestEnd',
#                                                                  'portfTestCentroVol.clust', 'portfTestCentroRet.clust')], all.x=TRUE,
#                            by.x = 'Date', by.y = 'dateTestEnd')
# colnames(StrategAll.df.trj) <- c('Date','MV.vol','MV.ret','Strg1.vol','Strg1.ret',
#                                  'Strg2a.vol','Strg2a.ret',
#                                  'Strg2b.vol','Strg2b.ret',
#                                  'Strg2c.vol','Strg2c.ret')

StrategAll.df.long <- StrategAll.df.trj[,c('Date','MV.vol','MV.ret')]
StrategAll.df.long$Centroids <- c('MV')
colnames(StrategAll.df.long) <- c('Date','Volatility','MeanReturn','Centroids')

temp <- StrategAll.df.trj[,c('Date','Strg1.vol','Strg1.ret')]
temp$Centroids <- c('SR')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

StrategAll.df.long <- rbind(StrategAll.df.long,temp)

temp <- StrategAll.df.trj[,c('Date','Strg2a.vol','Strg2a.ret')]
temp$Centroids <- c('LR')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

StrategAll.df.long <- rbind(StrategAll.df.long,temp)

temp <- StrategAll.df.trj[,c('Date','Strg2b.vol','Strg2b.ret')]
temp$Centroids <- c('MR')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

StrategAll.df.long <- rbind(StrategAll.df.long,temp)

temp <- StrategAll.df.trj[,c('Date','Strg2c.vol','Strg2c.ret')]
temp$Centroids <- c('HR')
colnames(temp) <- c('Date','Volatility','MeanReturn','Centroids')

StrategAll.df.long <- rbind(StrategAll.df.long,temp)

ggplot(StrategAll.df.long, aes( x = Volatility, y = MeanReturn, color = Centroids))+
  geom_path(aes(group = Centroids))

ggplot(StrategAll.df.long, aes(x=Date, y=MeanReturn)) +
  geom_line(aes(color=Centroids, group=Centroids))

ggplot(StrategAll.df.long, aes(x=Date, y=MeanReturn)) +
  geom_line(aes(color=Centroids, group=Centroids)) + 
  ylim(-0.005,0.01)

##################################
##  Monthly Returns by investors #
##################################
#https://thenode.biologists.com/visualizing-data-one-more-time/

library(dplyr)

retGP.mtx.MV
retGP.mtx.IdX
retGP.mtx.SR
retGP.mtx.LR
retGP.mtx.MR
retGP.mtx.HR

## Returns per Month
retGP.month.MV <- as.data.frame(t(retGP.mtx.MV))
retGP.month.MV$Month <- seq(1:nrow(retGP.month.MV))
retGP.month.MV$Model <- c('MV')

retGP.month.IdX <- as.data.frame(t(retGP.mtx.IdX))
retGP.month.IdX$Month <- seq(1:nrow(retGP.month.IdX))
retGP.month.IdX$Model <- c('IdX')

retGP.month.SR <- as.data.frame(t(retGP.mtx.SR))
retGP.month.SR$Month <- seq(1:nrow(retGP.month.SR))
retGP.month.SR$Model <- c('SR')

retGP.month.LR <- as.data.frame(t(retGP.mtx.LR))
retGP.month.LR$Month <- seq(1:nrow(retGP.month.LR))
retGP.month.LR$Model <- c('LR')

retGP.month.MR <- as.data.frame(t(retGP.mtx.MR))
retGP.month.MR$Month <- seq(1:nrow(retGP.month.MR))
retGP.month.MR$Model <- c('MR')

retGP.month.HR <- as.data.frame(t(retGP.mtx.HR))
retGP.month.HR$Month <- seq(1:nrow(retGP.month.HR))
retGP.month.HR$Model <- c('HR')

retGP.month.MERG <- bind_rows(retGP.month.MV, retGP.month.IdX, retGP.month.SR, 
                              retGP.month.LR,retGP.month.MR, retGP.month.HR)


# df <- economics %>%
#   select(date, psavert, uempmed) %>%
#   gather(key = "variable", value = "value", -date)
# head(df, 3)

# StrategAll.long <- StrategAll.df %>%
#   gather(key='variable',value = 'value',-Date)

# pruebaT.long$Investor <- as.numeric(pruebaT.long$Investor)
# #prueba.long$Period<- chr(prueba.long$Period + 64)
# #prueba.long$Period<- as.character(prueba.long$Period)
# ind <- (pruebaT.long$Investor < 10)
# aaa <- paste0('0',pruebaT.long$Investor)
# pruebaT.long[ind,]$Investor <- aaa[ind]

retGP.month.LONG <- gather(retGP.month.MERG, Investor, Return, -Month, -Model)
ggplot(retGP.month.LONG, aes(x=Month, y=Return, color = Model)) + 
  geom_line(aes(color=Model, y=Return, color=Model))

prueba <- subset(retGP.month.LONG, Model=='MV',select = c(Month, Investor, Return))
prueba <- subset(retGP.month.LONG, Model == 'SR' , select = c(Month, Investor, Return))
p <- ggplot(prueba, aes(x=Month, y=Return)) + geom_line(aes(color=Investor))
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)


aaa<- retGP.month.LONG
aaa$Model <- factor(aaa$Model, levels = c('MV','IdX','SR','LR','MR','HR'))

df_tidy_mean <- aaa %>%
  group_by(Month, Model) %>%
  summarise(n = n(),
            MonthlyReturn = mean(Return),
            Median = median(Return),
            sd = sd(Return)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = MonthlyReturn + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = MonthlyReturn - qt((1-0.95)/2, n - 1) * sem)

#head(df_tidy_mean)

ggplot(df_tidy_mean, aes(x=Month, y=MonthlyReturn, color = Model)) +
  geom_line(aes(x=Month, y=Median, color=Model)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Model),color="grey70",alpha=0.4)


ggplot(df_tidy_mean, aes(x=Month, y=MonthlyReturn, color = Model)) +
  geom_line(aes(x=Month, y=Median, color=Model)) 


#retGP.mtx.ALL <- prueba %>% gather(key='variable',value='value')

### Cumulative Returns per Month #################################
retCUM.mtx.MV
retCUM.mtx.IdX
retCUM.mtx.SR
retCUM.mtx.LR 
retCUM.mtx.MR 
retCUM.mtx.HR 

retCUM.month.MV <- as.data.frame(t(retCUM.mtx.MV))
retCUM.month.MV$Month <- seq(1:nrow(retCUM.month.MV))
retCUM.month.MV$Model <- c('MV')

retCUM.month.IdX <- as.data.frame(t(retCUM.mtx.IdX))
retCUM.month.IdX$Month <- seq(1:nrow(retCUM.month.IdX))
retCUM.month.IdX$Model <- c('IdX')

retCUM.month.SR <- as.data.frame(t(retCUM.mtx.SR))
retCUM.month.SR$Month <- seq(1:nrow(retCUM.month.SR))
retCUM.month.SR$Model <- c('SR')

retCUM.month.LR <- as.data.frame(t(retCUM.mtx.LR))
retCUM.month.LR$Month <- seq(1:nrow(retCUM.month.LR))
retCUM.month.LR$Model <- c('LR')

retCUM.month.MR <- as.data.frame(t(retCUM.mtx.MR))
retCUM.month.MR$Month <- seq(1:nrow(retCUM.month.MR))
retCUM.month.MR$Model <- c('MR')

retCUM.month.HR <- as.data.frame(t(retCUM.mtx.HR))
retCUM.month.HR$Month <- seq(1:nrow(retCUM.month.HR))
retCUM.month.HR$Model <- c('HR')

retCUM.month.MERG <- bind_rows(retCUM.month.MV, retCUM.month.IdX, retCUM.month.SR, 
                              retCUM.month.LR,retCUM.month.MR, retCUM.month.HR)

retCUM.month.LONG <- gather(retCUM.month.MERG, Investor, Return, -Month, -Model)
# ggplot(retCUM.month.LONG, aes(x=Month, y=Return, color = Model)) + 
#   geom_line(aes(color=Model, y=Return, color=Model))

# prueba <- subset(retCUM.month.LONG, Model=='MV',select = c(Month, Investor, Return))
# prueba <- subset(retCUM.month.LONG, Model == 'SR' , select = c(Month, Investor, Return))
# p <- ggplot(prueba, aes(x=Month, y=Return)) + geom_line(aes(color=Investor))
# p + stat_smooth(
#   color = "#FC4E07", fill = "#FC4E07",
#   method = "loess"
# )

df_tidy_mean <- retCUM.month.LONG %>%
  group_by(Month, Model) %>%
  summarise(n = n(),
            MeanReturn = mean(Return),
            MedianReturn = median(Return),
            sd = sd(Return)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = MeanReturn + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = MeanReturn - qt((1-0.95)/2, n - 1) * sem)

#head(df_tidy_mean)

ggplot(df_tidy_mean, aes(x=Month, y=MeanReturn, color = Model)) +
  geom_line(aes(x=Month, y=MeanReturn, color=Model)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Model),color="grey70",alpha=0.4)

ggplot(df_tidy_mean, aes(x=Month, y=MeanReturn, color = Model)) +
  geom_line(aes(x=Month, y=MeanReturn, color=Model))


###########################################
# Median                              #####
###########################################

aaa<- retCUM.month.LONG
aaa$Model <- factor(aaa$Model, levels = c('MV','IdX','SR','LR','MR','HR'))

df_tidy_median <- aaa %>%
  group_by(Month, Model) %>%
  summarise(n = n(),
            CumReturn = mean(Return),
            MedianReturn = median(Return),
            sd = sd(Return)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = CumReturn + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = CumReturn - qt((1-0.95)/2, n - 1) * sem)

ggplot(df_tidy_median, aes(x=Month, y=CumReturn, color = Model)) +
  geom_line(aes(x=Month, y=MedianReturn, color=Model)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Model),color="grey70",alpha=0.4)

ggplot(df_tidy_median, aes(x=Month, y=CumReturn, color = Model)) +
  geom_line(aes(x=Month, y=MedianReturn, color=Model))
  
# #Detail
# ggplot(df_tidy_median, aes(x=Month, y=CumReturn, color = Model)) +
#   geom_line(aes(x=Month, y=MedianReturn, color=Model)) +
#   geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Model),color="grey70",alpha=0.4) +
#   xlim(0,20) + ylim(0,10)


# Only MV
MV.long <-subset(retCUM.month.LONG,Model=='MV')
df_tidy_mean <- MV.long %>%
  group_by(Month, Model) %>%
  summarise(n = n(),
            CumReturn = mean(Return),
            MedianReturn = median(Return),
            sd = sd(Return)) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = CumReturn + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = CumReturn - qt((1-0.95)/2, n - 1) * sem)

ggplot(df_tidy_mean, aes(x=Month, y=CumReturn, color = Model)) +
  geom_line(aes(x=Month, y=MedianReturn, color=Model)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Model),color="grey70",alpha=0.4)



##################################
## DENSITY PLOTS (Strategy ALL) ##
##################################
library(FactoMineR)
library(factoextra)

### Portfolio centroids: ALL strategies
#cl1 <- subset(StrategAll.df.long, Centroids == 'MVTrain' | Centroids == 'ClustTrain',select = c(Volatility,MeanReturn, Centroids))
fig <- plot_ly(StrategAll.df.long,x= ~Volatility, y=~MeanReturn, showscale = F, autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids)
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig


### Portfolio centroids ##########
fig <- plot_ly(StrategAll.df.long,x= ~Volatility, y=~MeanReturn,showscale = F, autocolorscale=T, 
               colors = c('red','yellow','blue','black','green'))
fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
  layout(xaxis = list(range = c(0.0,0.01),title = 'Portfolio Volatility'),
         yaxis = list(range = c(-0.0006, 0.0013),title = 'Portfolio Return'))
fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25)
fig

###
library(plyr)
df <- StrategAll.df.long
df <- na.omit(df)
df <- subset(df,Volatility < 0.01, select = c(Volatility,MeanReturn,Centroids))
find_hull <- function(df) df[chull(df$Volatility, df$MeanReturn), ]
hulls <- ddply(df, "Centroids", find_hull)
plot <- ggplot(data = df, aes(x = Volatility, y = MeanReturn, colour=Centroids, fill = Centroids)) +
  geom_point() + 
  geom_polygon(data = hulls, alpha = 0.2) +
  labs(x = "Portfolio Volatility", y = "Portfolio Return")
plot


# data(iris)
# df<-iris
# find_hull <- function(df) df[chull(df$Sepal.Length, df$Sepal.Width), ]
# hulls <- ddply(df, "Species", find_hull)
# plot <- ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width, colour=Species, fill = Species)) +
#   geom_point() + 
#   geom_polygon(data = hulls, alpha = 0.5) +
#   labs(x = "Sepal.Length", y = "Sepal.Width")
# plot

### Portfolio Clusters ##########
object = list(data = StrategAll.df.long[,c('Volatility','MeanReturn')], 
              cluster = StrategAll.df.long[,c('Centroids')])

fviz_cluster(object)
fviz_cluster(object, geom = 'text', labelsize = 7, show.clust.cent = TRUE,color = 'dimgray',
                          main='K-means: Volatility-Average return', stand = FALSE)

#First k-means representation
# fviz_cluster(k3_medoid,data = df.ord, geom = 'text', labelsize = 7, show.clust.cent = TRUE,
#              main='K-means: Volatility-Average return', stand = FALSE)


# p <- fviz_cluster(k3_medoid,data = df.ord, geom = 'point',show.clust.cent = TRUE,
#                   main='K-means: Volatility-Mean return',ellipse.alpha=0, stand = FALSE)
# p <- fviz_add(p,df.red, labelsize = 3, geom = 'text', color = 'dimgray', pointsize = 1, 
#               repel = TRUE, ellipse.alpha=0)
# 
# fviz_add(p, df.cap, labelsize = 3, color = 'red',repel = TRUE,stand = FALSE)


# ### Portfolio centroids ##########
# cl2 <- subset(Strateg2c.df.long, Centroids == 'PortfTestClust' | Centroids == 'PortfTestMV' | 
#                 Centroids == 'PortfTrainAll' | Centroids == 'PortfTrainClust',
#               select = c(Volatility, MeanReturn, Centroids))
# fig <- plot_ly(cl2,x= ~Volatility, y=~MeanReturn,showscale = F,autocolorscale=T)
# fig <- fig %>% add_markers(alpha=0.4, color= ~Centroids) %>%
#   layout(xaxis = list(range = c(0.0,0.008),title = 'Portfolio Volatility'),
#          yaxis = list(range = c(-0.0005, 0.0015), title = 'Portfolio Return'),
#          legend = list(title=list(text='<b> Strategies </b>')))
# fig <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=45)
# fig


# temp <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25) %>%
#   layout(xaxis = list(range = c(0.05, 0.5)),
#          yaxis = list(range= c(-0.02,0.0)),
#          showlegend=FALSE)
# temp
# if (!require("processx")) install.packages("processx")
# orca(temp, file = "CentroidKMClust1D2.eps", width = 600, height = 500)
# 
# temp %>%
#   export(file = "D:/DOCTORADO/Scripts/R/Entrega/Datasets/prueba.svg",
#          selenium = RSelenium::rsDriver(browser = "firefox"))
# 
# png('prueba.png', res = 300)
# temp
# dev.off()

# # # Define x, y, and time coordinates
# # coords <- data.frame(x = c(1, 1.5, 2, 2.5, 3, 4), 
# #                      y = c(0, 0, 1, 1, 2, 1), 
# #                      times = c(0, 1, 2, 3, 4, 5))
# # # Create a trajectory from the coordinates
# # trj <- TrajFromCoords(coords)
# # 
# # # Plot it
# # plot(trj)
# 
# 
# Strateg1.trj.RtTrain <- TrajFromCoords(Strateg1.df.trj[,c(2,5,6)],2,3,1)
# plot(Strateg1.trj.RtTrain, xlim = c(0.06,0.16), ylim = c(-0.02,0.02))
# 
# # Strateg1.trj.smooth <- TrajSmoothSG(Strateg1.trj.RtTrain)
# # plot(Strateg1.trj.smooth)
# 
# Strateg1.trj.RtTrain.clust <- TrajFromCoords(Strateg1.df.trj[,c(2,11,12)],2,3,1)
# lines(Strateg1.trj.RtTrain.clust, col="orange")

#https://rpubs.com/JoFrhwld/trajectories



