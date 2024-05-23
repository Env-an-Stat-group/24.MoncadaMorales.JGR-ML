###############################################
###############################################
### Short-Range Forecasting with the BinESN ###
###############################################
###############################################



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
set.seed(1)

#set number of cores
options(cores = 10) #change based on #of cores in machine




################################################################################
########################## Binary ESN ##########################################
################################################################################



#Setting hyper-Parameters
locations=dim(Ybin)[2]
tau = 1 
m=1
nt=dim(Ybin)[1]#time points
n.h=100
pi.win=0.1
pi.w=0.1
nu=0.25
h.in=NULL
coeff=NULL
alpha=0.75 

#gradient descent parameters
learning_rate=0.001
gd_iter=1000
tol=0.001


#set train and test length
rawData=Ybin
trainLen=24*22
validLen=0
testLen=24

input.data=gen.input.data(rawData, m, tau, trainLen, validLen, testLen, Valid=FALSE, Testing=TRUE)
yTrain=input.data$yTrain
xTrain=input.data$xTrain

yValid=input.data$yValid
xValid=input.data$xValid

xTest=input.data$xTest
yTest = input.data$yTest


#begin running BinESN
n.ensemble=100
test.ensemble.esn=ensemble.esn(n.ensemble,
                               yTrain,
                               xTrain,
                               xTest ,
                               yTest,
                               n.h,
                               nu,
                               pi.w,
                               pi.win,
                               alpha,
                               m,
                               startvalues=NULL,
                               activation='tanh',
                               learning_rate,
                               gd_iter,
                               tol,
                               intercept=TRUE,
                               verbose = FALSE,
                               parallel = TRUE,
                               fork = FALSE)

#rename columns and print predicted probabilities
colnames(test.ensemble.esn$probabilitiesmean) = colnames(Ybin)
print(test.ensemble.esn$probabilitiesmean)









