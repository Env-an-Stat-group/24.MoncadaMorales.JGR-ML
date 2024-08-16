################################################
################################################
### Data Preprocessing and Figure Generation ###
################################################
################################################


#clear environment and load libraries
rm(list = ls()); gc()
library(tidyverse)
library(Matrix)
library(abind)
library(sf)
library(RColorBrewer)
library(ggpubr)
library(caret)
library(pracma)

#load parallel libraries
library(parallel)
library(foreach)
library(doParallel)

#set cores
# options(cores = 10)


###################################
### Convert City Data to Binary ###
###################################


#load hourly data for each city
load('PrecipHourlyCities.RData')

#convert to Binary and save
bin_data_cities = apply(data_cities > 1, 2, as.numeric)
save(bin_data_cities, file = 'BinaryHourlyPrecipCities.RData')

#load anf plot an image of the matrix
load('BinaryHourlyPrecipCities.RData')
image(bin_data_cities)






################
### Figure 1 ###
################


#get city coords
map_data <- maps::map("state", plot = FALSE, fill = TRUE)
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
cities <- data.frame(city = c("Milwaukee","Minneapolis","Chicago","StLouis","Nashville","Atlanta","Charlotte","Indianapolis","Columbus","Detroit"), 
                     lat = c(43.038, 44.978, 41.878, 38.627, 36.162, 33.748, 35.227, 39.791, 39.961, 42.331), 
                     lon = c(-87.906, -93.265, -87.629, -90.199, -86.781, -84.387, -80.843, -86.148, -82.998, -83.045))
cities <- st_as_sf(cities, coords = c("lon", "lat"), remove = FALSE, 
                   crs = 4326, agr = "constant")

colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)


#Read data 
lat=readRDS('lat_matrix_d01.rds')
lon=readRDS('lon_matrix_d01.rds')
data = readRDS('June_PRCP_hourly_D01.rds')


#extract a specific day
d=22
h=1
a<-c(data[,,((d*24)+h)])
ind=which(a>=8)
a[ind]=8
lat <- lat[1,]
lon <- lon[,1]
lat_2 <- rep(lat, each=175)
lon_2 <- rep(lon, 175)
df <- data.frame(lat=lat_2, lon=lon_2, a=a)
map <- df



#make plot
hrly_prcp = ggplot() +
  geom_tile(map, mapping = aes(x = lon, y = lat, fill = a)) +
  geom_sf(data = states,
          fill = "transparent",
          linewidth = 0.5, 
          color = 'black') +
  geom_sf(data = cities,
          shape = 8,
          size = 3) +
  geom_text(
    data = cities,
    aes(x = lon, y = lat+0.4, label = city),
    size = 5,
    col = "black",
    fontface = "bold"
  ) +
  labs(x = '', y = '', title = '', fill =  "Hourly Precipitation (mm)") +
  coord_sf(
    xlim = c(-96.85959, -75.46042),
    ylim = c(29.86606, 48.38952),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20, face = 'bold'),
    legend.position = 'bottom',
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 16, face = 'bold'),
    legend.title = element_text(vjust = 1, size = 18, face = 'bold')
  ) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Blues"),  # Use the Blues color scale with 9 colors and reverse it
    name = "Hourly Precipitation (mm)" #,limits=c(0,10)
  )


hrly_prcp





################
### Figure 2 ###
################


#load data
load("PrecipHourlyCities.RData")
load("BinaryHourlyPrecipCities.RData")
Tp=24*22 #Number of time points
nl=10#Number of places
Y=data_cities
Ybin=bin_data_cities
thr=1




#plot for manuscript
#we use indianpolis
# colnames(Y)
place = which(colnames(Y) == "Indianapolis")
df=data.frame(X=seq(1,Tp),Y=Y[1:Tp,place])
summary(df)
dates = seq(as.POSIXct("2017-06-01 00:00:00"), as.POSIXct("2017-06-22 23:00:00"), by="hour")
df$X = dates

#time series of PRCP
gp2 <- ggplot(df, aes(X, Y)) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = thr, linetype = "dashed", color = "red", linewidth = 1) +
  xlab('Date') +
  ylab('Hourly Precipitation (mm) ') + 
  ggtitle("") +
  ylim(0,60)+
  theme_bw() +  # Set the theme to minimal
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'))  # Set the background color to white

# gp2



#prep data for binary plot
x <- seq(Tp)
y <- Ybin[1:Tp,place]
DF <- data.frame(Date = x, Event = y)
DF$Date = dates

# Create a ggplot
gp5=ggplot(DF, aes(x = Date, y = 1, color = factor(Event))) +
  geom_segment(aes(xend = Date, yend = 0), size = 2) +
  scale_color_manual(values = c("white", "black"), labels = c(" ", "Event Observed")) +
  labs(x = "Date", y = " ", title = "") +
  scale_y_continuous(breaks = c(0, 1), expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'))  # Remove the legend

all_plots = ggarrange(gp2,gp5, nrow=1, ncol=2,labels=c('A','B'))

all_plots





################
### Figure 3 ###
################



#load data
load('100ArrayLorenz96.Rdata')
load('100LorenzBin.Rdata')
Tp=500 #Number of time points
nl=10#Number of places
nsim=1
Y=data[1:Tp,1:nl,nsim]
thr=3.5




#Figure for manuscript
place = 1
df=data.frame(X=seq(1,Tp),Y=Y[,place])


#time series plot
gp2 <- ggplot(df, aes(X, Y)) +
  geom_line(linewidth = 0.55) +
  geom_hline(yintercept = thr, linetype = "dashed", color = "red", linewidth = 1) +
  xlab('Time') +
  ylab(" ") +
  ggtitle("") +
  scale_y_continuous(limits = c(-3, 7)) +
  theme_bw() +  # Set the theme to minimal
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'))  # Set the background color to white

# gp2

#prep data for binary plot
x <- seq(Tp)
y <- Ybin_100[,place,nsim]
DF <- data.frame(Date = x, Event = y)

# Create a ggplot
gp5=ggplot(DF, aes(x = Date, y = 1, color = factor(Event))) +
  geom_segment(aes(xend = Date, yend = 0), size = 2) +
  scale_color_manual(values = c("white", "black"), labels = c(" ", "Event Observed")) +
  labs(x = "Time", y = " ", title = '') +
  theme(panel.background = element_rect(fill = "white"))+  # Set the background color to white
  scale_y_continuous(breaks = c(0, 1), expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'))  # Remove the legend

# gp5

all_plots = ggarrange(gp2,gp5, nrow=1, ncol=2,labels=c('A','B'))


all_plots



################
### Figure 4 ###
################


#short range
load('ROC_bin_100sims.RData')
load('ROC_van_100sims.RData')
load('ROC_log_100sims.RData')
load('ROC_lstm_100sims.RData')
load('ROC_pers_100sims.RData')

cols = c("FPR", "TPR")
colnames(roc_bin) = cols
colnames(roc_van) = cols
colnames(roc_log) = cols
colnames(roc_lstm) = cols
colnames(roc_pers) = cols


p = ggplot() +
  geom_line(data = roc_bin, aes(x = FPR, y = TPR, color = "BinESN"), linewidth = 1.25) +
  geom_line(data = roc_van, aes(x = FPR, y = TPR, color = "ESN"), linewidth = 1.25) +
  geom_line(data = roc_log, aes(x = FPR, y = TPR, color = "Logistic"), linewidth = 1.25) +
  geom_line(data = roc_lstm, aes(x = FPR, y = TPR, color = "LSTM"), linewidth = 1.25) +
  geom_line(data = roc_pers, aes(x = FPR, y = TPR, color = "Persistence"), linewidth = 1.25) +
  geom_abline(linetype=2, linewidth = 1.25)+
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  ggtitle("") +
  labs(color = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16, face = 'bold'),
        legend.title = element_text(size = 18, face = 'bold'),
        legend.position = "bottom") +
  scale_color_manual(values = c("BinESN" = "red", 
                                "ESN" = "orange", 
                                "Logistic" = "green", 
                                "LSTM" = "purple", 
                                "Persistence" = "blue")) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

#print(p)


#####Long-range ROC curves
load('ROC_long_bin_100sims.RData')
load('ROC_long_van_100sims.RData')
load('ROC_long_log_100sims.RData')
load('ROC_long_lstm_100sims.RData')
load('ROC_long_pers_100sims.RData')

cols = c("FPR", "TPR")
colnames(roc_long_bin) = cols
colnames(roc_long_van) = cols
colnames(roc_long_log) = cols
colnames(roc_long_lstm) = cols
colnames(roc_long_pers) = cols


p2 = ggplot() +
  geom_line(data = roc_long_bin, aes(x = FPR, y = TPR, color = "BinESN"), linewidth = 1.25) +
  geom_line(data = roc_long_van, aes(x = FPR, y = TPR, color = "ESN"), linewidth = 1.25) +
  geom_line(data = roc_long_log, aes(x = FPR, y = TPR, color = "Logistic"), linewidth = 1.25) +
  geom_line(data = roc_long_lstm, aes(x = FPR, y = TPR, color = "LSTM"), linewidth = 1.25) +
  geom_line(data = roc_long_pers, aes(x = FPR, y = TPR, color = "Persistence"), linewidth = 1.25) +
  geom_abline(linetype=2, linewidth = 1.25)+
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  ggtitle("") + 
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold')) +
  scale_color_manual(values = c("BinESN" = "red", 
                                "ESN" = "orange", 
                                "Logistic" = "green", 
                                "LSTM" = "purple", 
                                "Persistence" = "blue")) +
  scale_y_continuous(position = 'right') +
  theme(legend.position = "none")#+guides(color = guide_legend(title = "Model"))

#print(p2)

all_plots = ggarrange(p,p2,
                      nrow=1, ncol=2,
                      labels=c('A','B'),
                      common.legend = TRUE, 
                      legend = 'bottom')


all_plots






##############################
### Figure 5 and Figure S2 ###
##############################





#short Range




load("BinESN_histDat_100Sims.RData")
AUC_med_bin=apply(data4hist.bin$ensembAUCs,1,median)
median(AUC_med_bin)
quantile(AUC_med_bin,p=c(0.025,0.975))
BS_med_bin=apply(data4hist.bin$ensembBS,1,median)
median(BS_med_bin)
quantile(BS_med_bin,p=c(0.025,0.975))




load("VanESN_histDat_100Sims.RData")
AUC_med_van=apply(data4hist.van$ensembAUCs,1,median)
median(AUC_med_van)
quantile(AUC_med_van,p=c(0.025,0.975))
BS_med_van=apply(data4hist.van$ensembBS,1,median)
median(BS_med_van)
quantile(BS_med_van,p=c(0.025,0.975))


load("ARLog_histDat_100sims.RData")
AUC_log=data4hist.log$ensembAUCs
median(AUC_log)
quantile(AUC_log,p=c(0.025,0.975))
BS_log=data4hist.log$ensembBS
median(BS_log)
quantile(BS_log,p=c(0.025,0.975))



load("LSTM_histDat_100sims.RData")
AUC_lstm=data4hist.lstm$ensembAUCs
median(AUC_lstm)
quantile(AUC_lstm,p=c(0.025,0.975))
BS_lstm=data4hist.lstm$ensembBS
median(BS_lstm)
quantile(BS_lstm,p=c(0.025,0.975))



load("Pers_histDat_100sims.RData")
AUC_pers=data4hist.pers$ensembAUCs
median(AUC_pers)
quantile(AUC_pers,p=c(0.025,0.975))
BS_pers=data4hist.pers$ensembBS
median(BS_pers)
quantile(BS_pers,p=c(0.025,0.975))




df=data.frame(AUC=c(AUC_med_bin,AUC_med_van,AUC_log,AUC_lstm,AUC_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,100,100)))
#boxplot(AUC~Model, data=df)

# Basic box plot
p <- ggplot(df, aes(x=Model, y=AUC, fill = Model)) + 
  geom_boxplot() +
  labs(x = "", y = 'AUC', fill = '', title = 'Short-Range Forecasts') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 21),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue"))
# p

df2=data.frame(BS=c(BS_med_bin,BS_med_van,BS_log,BS_lstm,BS_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,100,100)))
#boxplot(BS~Model, data=df2)

# Basic box plot
p2 <- ggplot(df2, aes(x=Model, y=BS, fill = Model)) + 
  geom_boxplot() +
  labs(x = "", y = 'Brier Score', fill = '', title = 'Short-Range Forecasts') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 21),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue"))+
  scale_y_continuous(position = 'left')
# p2






#long range




#logistic regression
load("ARLogReg_longDat_100sims.RData")
total_sim=100
AUC_meds_log=LongRangeData.log$LongAUCs
Hp_meds_log=LongRangeData.log$LongHps
BS_meds_log=LongRangeData.log$LongBS

median(AUC_meds_log)
quantile(AUC_meds_log,p=c(0.025,0.975))
median(Hp_meds_log)
quantile(Hp_meds_log,p=c(0.025,0.975))
median(BS_meds_log)
quantile(BS_meds_log,p=c(0.025,0.975))




#BinESN
total_sim=100
ntests=15

AUC_bin=matrix(NA,ncol=total_sim,nrow=ntests)
AUC_meds_bin=rep(0,ntests)
Hp_bin=matrix(NA,ncol=total_sim,nrow=ntests)
BS_bin=matrix(NA,ncol=total_sim,nrow=ntests)
BS_meds_bin=rep(0,ntests)

#Load data
for(i in 1:total_sim){
  load(paste0('BinESN_longDat',i,'.RData'))
  list_name <- paste0('LongRangeData', i, '.bin')
  # Access the list using `get`
  current_list <- get(list_name)
  AUC_bin[,i]=current_list$LongAUCs
  BS_bin[,i]=current_list$LongBS
  AUC_meds_bin[i]=median(AUC_bin[,i])
  BS_meds_bin[i]=median(BS_bin[,i])
}

quantile(as.vector(AUC_bin),p=c(0.025,0.975))
median(AUC_meds_bin)
(median(AUC_meds_bin)-median(AUC_meds_log))/median(AUC_meds_log)
quantile(AUC_meds_bin,p=c(0.025,0.975))

quantile(as.vector(BS_bin),p=c(0.025,0.975))
median(BS_meds_bin)
(median(BS_meds_log)-median(BS_meds_bin))/median(BS_meds_log)
quantile(BS_meds_bin,p=c(0.025,0.975))




#standard ESN
load("VanESN_longDat_100sims.RData")
total_sim=100
AUC_meds_van=rep(NA,total_sim)
Hp_meds_van=rep(NA,total_sim)
BS_meds_van=rep(NA,total_sim)

for(i in 1:total_sim){
  AUC_meds_van[i]=median(LongRangeData.van$LongAUCs[i,])
  Hp_meds_van[i]=median(LongRangeData.van$LongHps[i,])
  BS_meds_van[i]=median(LongRangeData.van$LongBS[i,])
}

median(AUC_meds_van)
quantile(AUC_meds_van,p=c(0.025,0.975))
(median(AUC_meds_van)-median(AUC_meds_log))/median(AUC_meds_log)
median(BS_meds_van)
(median(BS_meds_log)-median(BS_meds_van))/median(BS_meds_log)
quantile(BS_meds_van,p=c(0.025,0.975))


#Persistence
load('100LorenzBin.RData')
total_sim=100 
trainLen=450
future=50
locations=dim(Ybin_100)[2]
AUC_meds_pers=rep(NA,total_sim)
Hp_meds_pers=rep(NA,total_sim)
BS_meds_pers=rep(NA,total_sim)

for(nsim in 1:total_sim){
  Ybin=Ybin_100[,,nsim]
  original_data=Ybin[(trainLen+1):(trainLen+future),]
  yi=as.vector(original_data)
  aux=t(matrix(Ybin[(450),],ncol=future,nrow=locations))
  pi=as.vector(aux)
  
  ###Binary Cross-Entropy
  Hp.pers=-(1/(future*locations))*sum( yi*log(pi) + (1-yi)*log(1-pi) )
  Hp_meds_pers[nsim]=Hp.pers
  
  ####Confusion matrix
  expected.value=factor(as.vector(original_data))
  predicted.value.pers=factor(pi)
  
  #Brier Score
  BS.pers=(1/(future*locations))*sum( (yi-pi)^2 )
  BS_meds_pers[nsim]=BS.pers
  
  ####ROC Curve
  thr=seq(from=0, to=1, by=0.01)
  rocc=matrix(NA,nrow=length(thr),ncol=2)
  rocc=as.data.frame(rocc)
  colnames(rocc)=c('FPR','TPR')
  for(i in 1:length(thr)){
    Y.hat.thr=(pi>thr[i])*1
    predicted.value.thr=factor(as.vector(Y.hat.thr),levels=c('0','1'))
    conf.mat.thr=confusionMatrix(data=predicted.value.thr, reference = expected.value)
    rocc[i,1]=1-conf.mat.thr$byClass["Specificity"]
    rocc[i,2]=conf.mat.thr$byClass["Sensitivity"]
  }
  
  par(mfrow=c(1,1))
  x=rocc$FPR
  x=append(0,x)
  y=rocc$TPR
  y=append(0,y)
  #plot(x,y,xlim=c(0,1),ylim=c(0,1),type='l',col='blue',lwd=2)
  #abline(a = 0, b = 1, lty='dashed',col='blue',lwd=2) 
  AUC.pers = trapz(x,y)
  AUC_meds_pers[nsim]=AUC.pers
  
}

median(AUC_meds_pers)
(median(AUC_meds_pers)-median(AUC_meds_log))/median(AUC_meds_log)
quantile(AUC_meds_pers,p=c(0.025,0.975))

median(BS_meds_pers)
(median(BS_meds_log)-median(BS_meds_pers))/median(BS_meds_log)
quantile(BS_meds_pers,p=c(0.025,0.975))


#LSTM
load("LSTM_longDat_100sims.RData")
total_sim=100
AUC_meds_lstm=rep(NA,total_sim)
Hp_meds_lstm=rep(NA,total_sim)
BS_meds_lstm=rep(NA,total_sim)

for(i in 1:total_sim){
  AUC_meds_lstm[i]=median(LongRangeData.lstm$LongAUCs[i,])
  Hp_meds_lstm[i]=median(LongRangeData.lstm$LongHps[i,])
  BS_meds_lstm[i]=median(LongRangeData.lstm$LongBS[i,])
}

median(AUC_meds_lstm)
(median(AUC_meds_lstm)-median(AUC_meds_log))/median(AUC_meds_log)
quantile(AUC_meds_lstm,p=c(0.025,0.975))
median(BS_meds_lstm)
(median(BS_meds_log)-median(BS_meds_lstm))/median(BS_meds_log)
quantile(BS_meds_lstm,p=c(0.025,0.975))



#boxplots
df3=data.frame(AUC=c(AUC_meds_bin,AUC_meds_van,AUC_meds_log,AUC_meds_lstm,AUC_meds_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,100,100)))
#boxplot(AUC~Model, data=df)

# Basic box plot
p3 <- ggplot(df3, aes(x=Model, y=AUC, fill = Model)) + 
  geom_boxplot() +
  labs(x = "", y = 'AUC', fill = '', title = 'Long-Range Forecasts') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 21),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue"))+
  scale_y_continuous(position = 'right')
#p3

df4=data.frame(BS=c(BS_meds_bin,BS_meds_van,BS_meds_log,BS_meds_lstm,BS_meds_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,100,100)))
#boxplot(BS~Model, data=df2)

# Basic box plot
p4 <- ggplot(df4, aes(x=Model, y=BS, fill = Model)) + 
  geom_boxplot()  +
  labs(x = "", y = 'Brier Score', fill = '', title = 'Long-Range Forecasts') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 21),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue")) +
  scale_y_continuous(position = 'right')
#p4

#all_plots = ggarrange(p,p2,p3,p4, nrow=2, ncol=2, labels=c('A','B','C','D'))
#ggsave('BoxPlot_Panel_100sims.png')



auc_plots = ggarrange(p,p3, nrow=1, ncol=2, labels=c('A','B'))

auc_plots



brier_plots = ggarrange(p2,p4, nrow=1, ncol=2, labels=c('A','B'))

brier_plots


################
### Figure 6 ###
################



load('ROC_bin_App.RData')
load('ROC_van_App.RData')
load('ROC_log_App.RData')
load('ROC_lstm_App.RData')
load('ROC_pers_App.RData')

cols = c("FPR", "TPR")
colnames(roc_bin) = cols
colnames(roc_van) = cols
colnames(roc_log) = cols
colnames(roc_lstm) = cols
colnames(roc_pers) = cols


p = ggplot() +
  geom_line(data = roc_bin, aes(x = FPR, y = TPR, color = "BinESN"), linewidth = 1.25) +
  geom_line(data = roc_van, aes(x = FPR, y = TPR, color = "ESN"), linewidth = 1.25) +
  geom_line(data = roc_log, aes(x = FPR, y = TPR, color = "Logistic"), linewidth = 1.25) +
  geom_line(data = roc_lstm, aes(x = FPR, y = TPR, color = "LSTM"), linewidth = 1.25) +
  geom_line(data = roc_pers, aes(x = FPR, y = TPR, color = "Persistence"), linewidth = 1.25) +
  geom_abline(linetype=2, linewidth = 1.25)+
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  ggtitle("") +
  labs(color = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 16, face = 'bold'),
        legend.title = element_text(size = 18, face = 'bold'),
        legend.position = "bottom") +
  scale_color_manual(values = c("BinESN" = "red", 
                                "ESN" = "orange", 
                                "Logistic" = "green", 
                                "LSTM" = "purple", 
                                "Persistence" = "blue")) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

#print(p)


#####ROC Curve
load('ROC_long_bin_App.RData')
load('ROC_long_van_App.RData')
load('ROC_long_log_App.RData')
load('ROC_long_lstm_App.RData')
load('ROC_long_pers_App.RData')

cols = c("FPR", "TPR")
colnames(roc_long_bin) = cols
colnames(roc_long_van) = cols
colnames(roc_long_log) = cols
colnames(roc_long_lstm) = cols
colnames(roc_long_pers) = cols


p2 = ggplot() +
  geom_line(data = roc_long_bin, aes(x = FPR, y = TPR, color = "BinESN"), linewidth = 1.25) +
  geom_line(data = roc_long_van, aes(x = FPR, y = TPR, color = "ESN"), linewidth = 1.25) +
  geom_line(data = roc_long_log, aes(x = FPR, y = TPR, color = "Logistic"), linewidth = 1.25) +
  geom_line(data = roc_long_lstm, aes(x = FPR, y = TPR, color = "LSTM"), linewidth = 1.25) +
  geom_line(data = roc_long_pers, aes(x = FPR, y = TPR, color = "Persistence"), linewidth = 1.25) +
  geom_abline(linetype=2, linewidth = 1.25)+
  xlab('False Positive Rate') +
  ylab('True Positive Rate') +
  ggtitle("") + 
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold')) +
  scale_color_manual(values = c("BinESN" = "red", 
                                "ESN" = "orange", 
                                "Logistic" = "green", 
                                "LSTM" = "purple", 
                                "Persistence" = "blue")) +
  scale_y_continuous(position = 'right') +
  theme(legend.position = "none")#+guides(color = guide_legend(title = "Model"))
#print(p2)



all_plots = ggarrange(p,p2,
                      nrow=1, ncol=2,
                      labels=c('A','B'),
                      common.legend = TRUE, 
                      legend = 'bottom')


all_plots



##############################
### Figure 7 and Figure S3 ###
##############################


load('Logistic_histDat_App1.RData')
auc_log=store.mat.log[,1]
median(auc_log)
quantile(auc_log,p=c(0.025,0.975))

bs_log=store.mat.log[,3]
median(bs_log)
quantile(bs_log,p=c(0.025,0.975))

load("BinESN_histDat_App1.RData")
auc_bin=data4hist.bin$ensembAUCs
median(auc_bin)
(median(auc_bin)-median(auc_log))/median(auc_log)
quantile(auc_bin,p=c(0.025,0.975))

bs_bin=data4hist.bin$ensembBS
median(bs_bin)
(median(bs_log)-median(bs_bin))/median(bs_log)
quantile(bs_bin,p=c(0.025,0.975))

load("VanESN_histDat_App1.RData")
auc_van=data4hist.van$ensembAUCs
median(auc_van)
(median(auc_van)-median(auc_log))/median(auc_log)
quantile(auc_van,p=c(0.025,0.975))

bs_van=data4hist.van$ensembBS
median(bs_van)
(median(bs_log)-median(bs_van))/median(bs_log)
quantile(bs_van,p=c(0.025,0.975))

load("LSTM_histDat_App1.RData")
auc_lstm=data4hist.lstm$AUC.lstm
median(auc_lstm)
(median(auc_lstm)-median(auc_log))/median(auc_log)
quantile(auc_lstm,p=c(0.025,0.975))

bs_lstm=data4hist.lstm$BS.lstm
median(bs_lstm)
(median(bs_log)-median(bs_lstm))/median(bs_log)
quantile(bs_lstm,p=c(0.025,0.975))


#persistence
source("all_functions.R")
#Load data
load("BinaryHourlyPrecipCities.RData")

Ybin=bin_data_cities

#ON TESTING DATA
trainLen=24*22
validLen=0
testLen=24
locations=dim(Ybin)[2]
tau=1
m=1

rawData=Ybin
input.data=gen.input.data(rawData, m, tau, trainLen, validLen, testLen, Valid=FALSE, Testing=TRUE)
yTrain=input.data$yTrain
xTrain=input.data$xTrain
yValid=input.data$yValid
xValid=input.data$xValid
xTest=input.data$xTest
yTest = input.data$yTest


####Confusion matrix
expected.value=factor(as.vector(yTest))
predicted.value.pers=factor(pi)

####ROC Curve
thr=seq(from=0, to=1, by=0.01)
rocc=matrix(NA,nrow=length(thr),ncol=2)
rocc=as.data.frame(rocc)
colnames(rocc)=c('FPR','TPR')
for(i in 1:length(thr)){
  Y.hat.thr=(pi>thr[i])*1
  predicted.value.thr=factor(as.vector(Y.hat.thr),levels=c('0','1'))
  conf.mat.thr=confusionMatrix(data=predicted.value.thr, reference = expected.value)
  rocc[i,1]=1-conf.mat.thr$byClass["Specificity"]
  rocc[i,2]=conf.mat.thr$byClass["Sensitivity"]
}
x=rocc$FPR
x=append(0,x)
y=rocc$TPR
y=append(0,y)
AUC.pers = trapz(x,y)
AUC.pers
auc_pers=AUC.pers
(auc_pers-median(auc_log))/median(auc_log)

#Brier Score
BS.pers=(1/(testLen*locations))*sum( (yi-pi)^2 )
BS.pers
bs_pers=BS.pers
(bs_pers-median(bs_log))/median(bs_log)
df=data.frame(AUC=c(auc_bin,auc_van,auc_log,auc_lstm,auc_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,50,1)))
#boxplot(AUC~Model, data=df)




# Basic box plot
p <- ggplot(df, aes(x=Model, y=AUC, fill = Model)) + 
  geom_boxplot() +
  labs(x = "", y = 'AUC', fill = '', title = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue"))
p

df2=data.frame(BS=c(bs_bin,bs_van,bs_log,bs_lstm,bs_pers),Model=rep(c('BinESN','ESN','Logistic','LSTM','Persistence'),times=c(100,100,100,50,1)))
#boxplot(BS~Model, data=df2)

# Basic box plot
p2 <- ggplot(df2, aes(x=Model, y=BS, fill = Model)) + 
  geom_boxplot()  +
  labs(x = "", y = 'Brier Score', fill = '', title = '') +
  theme_bw() +
  theme(axis.text = element_text(size = 18, face = 'bold'),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.position = "none") +
  scale_fill_manual(values = c("BinESN" = "red", 
                               "ESN" = "orange", 
                               "Logistic" = "green", 
                               "LSTM" = "purple", 
                               "Persistence" = "blue"))+
  scale_y_continuous(position = 'left')
p2







