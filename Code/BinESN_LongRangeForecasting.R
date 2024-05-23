##############################################
##############################################
### Long-Range Forecasting with the BinESN ###
##############################################
##############################################


#clear environment and load libraries
rm(list = ls())
library(tidyverse)
library(Matrix)
library(caret)
library(ROCR)
library(fda)
library(pracma)
library(abind)
library(sigmoid)

#load parallel libraries
library(parallel)
library(doParallel)
library(foreach)


#load necessary functions and data
source('all_functions.R')
load("BinaryHourlyPrecipCities.RData")
Ybin=bin_data_cities


#set seed
set.seed(1997)

#set number of cores
options(cores = 10)




################################################################################
########################## Binary ESN ##########################################
################################################################################


#Hyper-Parameter specification
tau=1
m=1
n.h=100
pi.win=0.1
pi.w=0.1
nu=0.5
h.in=NULL
coeff=NULL
alpha=0.25 

#Gradient Descent params
learning_rate=0.001
gd_iter=1000
tol=0.001

#Size of training
trainLen=24*22
validLen=0
testLen=1 #Since we will do one by one and iteratively forecast

#Only use training data as raw data for being able to append forecasts
rawData=Ybin[1:trainLen,]
locations=dim(rawData)[2]

#Number of forecasts desired
future=24

#Number of ensembles
n.ensemble=100

#begin iterative loop
prob_means.bin=matrix(NA,nrow=0,ncol=locations)
colnames(prob_means.bin) = colnames(Ybin)
for(i in 1:future){
  print(i)
  #Generate step i input data
  input.data=gen.input.data(rawData, m, tau, trainLen, validLen, testLen, Valid=FALSE, Testing=FALSE)
  yTrain=input.data$yTrain
  xTrain=input.data$xTrain
  xTest=input.data$xTest
  yTest = input.data$yTest
  
  
  #Train with ith input data and ith forecast
  bin.ensemble.esn=ensemble.esn(n.ensemble,
                                yTrain,
                                xTrain,
                                xTest  ,
                                yTest,
                                n.h,
                                nu,
                                pi.w,
                                pi.win,
                                alpha,
                                m,
                                startvalues=NULL,
                                activation='sigmoid',
                                learning_rate,
                                gd_iter,
                                tol,
                                verbose = FALSE,
                                parallel = TRUE,
                                fork = FALSE)
  
  #Generate forecast and append to create new raw data
  bin_forc=(bin.ensemble.esn$probabilitiesmean>0.5)*1
  rawData=rbind(rawData,bin_forc)
  
  #Save probabilities 
  prob_means.bin=rbind(prob_means.bin,(bin.ensemble.esn$probabilitiesmean))
  
  #Update params
  trainLen=trainLen+1
  
}


print(prob_means.bin)

