#################
#################
### Functions ###
#################
#################
#load libraries
library(Matrix)
library(caret)
library(foreach)
library(sigmoid)
library(purrr)
library(abind)

set.seed(NULL)
#################################
### Binary Echo State Network ###
#################################
ensemble.esn=function(n.ensemble=1,
                      yTrain,
                      xTrain,
                      xTest,
                      yTest=NULL,#Null when we do not have reference values and just want to forecast
                      n.h,
                      nu,
                      pi.w,
                      pi.win,
                      alpha,
                      m,
                      startvalues = NULL,#for coefficients
                      activation='tanh',
                      learning_rate=0.001,
                      gd_iter=1000,
                      tol=0.001,
                      intercept=TRUE,
                      verbose = FALSE,
                      parallel = FALSE,
                      fork = FALSE){
  
  if(!parallel)
  {
    if(verbose)
    {
      prog.bar = txtProgressBar(min = 0, max = n.ensemble, style = 3)
    }
  }
  
  #Set conditions for iterations
  future=dim(xTest)[1]
  locations=dim(yTrain)[2] #number of locations
  if(is.null(locations)){
    locations=1
  }
  
  if(locations != 1)
  {
    trainLen=dim(yTrain)[1]  #Number of time points in training
  } else if(locations == 1)
  {
    trainLen = length(yTrain)
  }
  
  n.x = locations * m
  
  #Sizes of weight matrices
  samp.win= n.h*n.x 
  samp.w= n.h*n.h
  
  pi.array=array(NA,dim=c(future,locations,n.ensemble))
  
  #Set the activation function
  if(activation == 'identity')
  {
    g.h = function(omega)
    {
      return(omega)
    } 
  } else if(activation == 'tanh') {
    g.h = function(omega)
    {
      placeholder = tanh(omega)
      return(placeholder)
    }
    
  } else if(activation=='sigmoid'){
    g.h = function(omega)
    {
      placeholder = sigmoid::sigmoid(omega)
      return(placeholder)
    }
    
  }
  
  ###########################
  #Begin ensemble iterations
  ###########################
  
  if(!parallel){
    #Non-parallel iterations
    for(i in 1:n.ensemble){
      #print(i)
      #vectors from Ber(pi) determining if entry is 0 or not
      gam.win = rbernoulli(samp.win, p = pi.win)*1
      gam.w = rbernoulli(samp.w, p = pi.w)*1
      
      
      #Entries from desired distribution
      norm.win = rnorm(samp.win, 0, 1)
      norm.w = rnorm(samp.w, 0, 1)
      
      #Sparse weight matrices
      Win=matrix(gam.win*norm.win,nrow=n.h,ncol=n.x)
      W=matrix(gam.w*norm.w,nrow=(n.h),ncol=(n.h))
      
      #Values to rescale W
      lambda.w = max(abs(eigen(W)$values))
      
      #Generate Hidden States 
      #set initial hidden state if not specified
      if(is.null(startvalues)){
        h.in=rep(0, n.h)
      }
      H=matrix(NA,nrow=trainLen,ncol=n.h) #memory to store hidden states
      
      ht=h.in
      
      for(t in 1:trainLen){
        xt=xTrain[t,]
        omega=g.h((nu/lambda.w)*(W %*% ht)+Win %*% xt)
        #omega=sigmoid::sigmoid((nu/lambda.w)*(W %*% ht)+Win %*% xt)
        ht=( (1-alpha) * ht ) + ( alpha*omega )
        H[t,]= as.numeric(ht)
      }
      
      #Given H and Y we estimate coefficients via logistic regression w/gradient descent
      if(intercept){
        ones=rep(1,trainLen)
        H=cbind(ones,H)
      }
      
      Y=yTrain
      
      in.values=runif((dim(H)[2])*locations,min=-2,max=2)#Initial values for beta
      in.values=matrix(in.values,nrow=dim(H)[2],ncol=locations)
     
      
      coeff.est=logistic.gd(H,
                            Y,
                            in.values,
                            learning_rate,
                            gd_iter,
                            tol)
      B.hat=coeff.est$coefficients
      
      #Generate hidden units for forecast
      H.forc=matrix(NA,nrow=future,ncol=n.h)
      for(f in 1:future){
        xt=xTest[f,]
        omega=g.h((nu/lambda.w)*(W %*% ht)+Win %*% xt)
        #omega=sigmoid::sigmoid((nu/lambda.w)*(W %*% ht)+Win %*% xt)
        ht=( (1-alpha) * ht ) + ( alpha*omega )
        H.forc[f,]= as.numeric(ht)
      }
      
      if(intercept){
        ones=rep(1,future)
        H.forc=cbind(ones,H.forc)
        
      }
      
      #Produce Forecasts
      logit.mat=H.forc %*% B.hat
      #Recover Probabilities
      pi.mat=1/(1+exp(-logit.mat))
      
      pi.array[,,i]=pi.mat
      if(verbose)
      {
        setTxtProgressBar(prog.bar, i)
      }
    }
    
  }else if(parallel){
    #Specify number of clusters
    if(fork)
    {
      cl <- parallel::makeForkCluster(getOption('cores'))
    } else if(!fork) {
      #cl <- parallel::detectCores()
      cl <- parallel::makeCluster(getOption('cores'))
    }
    set.seed(NULL)
    # Activate cluster for foreach library
    doParallel::registerDoParallel(cl)
    
    #Parallel Iterations
    pi.array = foreach::foreach(k = 1:n.ensemble,
                                .combine = cbind,
                                .inorder = FALSE) %dopar%
      {
        logistic.gd=function(design.matrix,Y,in.values=NULL,learning_rate,iter=1000,tol=0.001){
          dimension=dim(design.matrix)[2]  
          if(is.null(dim(Y)[2])){
            locations=1
            Y=matrix(Y,nrow=length(Y),ncol=1)
            if( ! is.null(in.values)){
              in.values=matrix(in.values,nrow=dimension,ncol=1)
            }
          }
          else{
            locations=dim(Y)[2]
          }
          
          nt=length(Y)
          
          if(is.null(in.values)){
            in.values=runif((dimension)*locations,min=-1,max=1)#recall to move
            in.values=matrix(in.values, nrow=dimension,ncol=locations)
          }
          
          B.hat=matrix(NaN,nrow=dimension,ncol=locations)
          it=matrix(iter,nrow=1,ncol=locations)
          for(loc in 1:locations){
            y=Y[,loc]
            prev.beta=in.values[,loc]
            for (i in 1:(iter-1)) {
              pi = 1/( 1 + exp( -( design.matrix %*% prev.beta ) ) )
              beta = prev.beta + learning_rate * (t(design.matrix) %*% (y - pi))
              if(norm(beta-prev.beta,type='2') < tol){
                it[,loc]=i
                break
              }
              prev.beta=beta
            }
            B.hat[,loc]=beta
          }
          
          output=list('coefficients'=B.hat,
                      'iterations'=it)
          return(output)
        }
        
        #print(i)
        #print(i)
        #vectors from Ber(pi) determining if entry is 0 or not
        gam.win = purrr::rbernoulli(samp.win, p = pi.win)*1
        gam.w = purrr::rbernoulli(samp.w, p = pi.w)*1
        
        
        #Entries from desired distribution
        norm.win = rnorm(samp.win, 0, 1)
        norm.w = rnorm(samp.w, 0, 1)
        
        #Sparse weight matrices
        Win=matrix(gam.win*norm.win,nrow=n.h,ncol=n.x)
        W=matrix(gam.w*norm.w,nrow=(n.h),ncol=(n.h))
        
        #Values to rescale W
        lambda.w = max(abs(eigen(W)$values))
        
        #Generate Hidden States 
        #set initial hidden state if not specified
        if(is.null(startvalues)){
          h.in=rep(0, n.h)
        }
        H=matrix(NA,nrow=trainLen,ncol=n.h) #memory to store hidden states
        
        ht=h.in
        
        for(t in 1:trainLen){
          xt=xTrain[t,]
          omega=g.h((nu/lambda.w)*(W %*% ht)+Win %*% xt)
          #omega=sigmoid::sigmoid((nu/lambda.w)*(W %*% ht)+Win %*% xt)
          ht=( (1-alpha) * ht ) + ( alpha*omega )
          H[t,]= as.numeric(ht)
        }
        
        #Given H and Y we estimate coefficients via logistic regression w/gradient descent
        if(intercept){
          ones=rep(1,trainLen)
          H=cbind(ones,H)
        }
        
        Y=yTrain
        
        in.values=runif((dim(H)[2])*locations,min=-2,max=2)#Initial values for beta
        in.values=matrix(in.values,nrow=dim(H)[2],ncol=locations)
        
        
        coeff.est=logistic.gd(H,
                              Y,
                              in.values,
                              learning_rate,
                              gd_iter,
                              tol)
        B.hat=coeff.est$coefficients
        
        #Generate hidden units for forecast
        H.forc=matrix(NA,nrow=future,ncol=n.h)
        for(f in 1:future){
          xt=xTest[f,]
          omega=g.h((nu/lambda.w)*(W %*% ht)+Win %*% xt)
          #omega=sigmoid::sigmoid((nu/lambda.w)*(W %*% ht)+Win %*% xt)
          ht=( (1-alpha) * ht ) + ( alpha*omega )
          H.forc[f,]= as.numeric(ht)
        }
        
        if(intercept){
          ones=rep(1,future)
          H.forc=cbind(ones,H.forc)
          
        }
        
        #Produce Forecasts
        logit.mat=H.forc %*% B.hat
        #Recover Probabilities
        pi.mat=1/(1+exp(-logit.mat))
        
        (pi.mat)
      }
    
    
  }
  
  #Close clusters
  if(parallel) {
   parallel::stopCluster(cl)
  }
  
  #Calculate forecast mean
  if(!parallel)
  {
    prob.mean = apply(pi.array, c(1,2), mean)
  } else if(parallel) {
    if(locations > 1 & future == 1)
    {
      prob.mean = sapply(1:locations, function(x) mean(pi.array[,seq(x, ncol(pi.array), locations)]))
    } else if(locations == 1 & future > 1){
      prob.mean = apply(pi.array, 1, mean)
    } else if(locations > 1 & future > 1) {
      prob.mean = sapply(1:locations, function(x) rowMeans(pi.array[,seq(x, ncol(pi.array), locations)]))
    } else if(locations == 1 & future == 1) {
      prob.mean = mean(as.numeric(pi.array))
    } else {
      prob.mean = NULL
    }
  }
  

  
  
  #Compile results
  esn.output = list('probabilitiesmean'=prob.mean,
                    'probabilities'=pi.array)
  return(esn.output)
  
}




########################
### Gradient Descent ###
########################


logistic.gd=function(design.matrix,Y,in.values=NULL,learning_rate,iter=1000,tol=0.001){
  dimension=dim(design.matrix)[2]  
  if(is.null(dim(Y)[2])){
    locations=1
    Y=matrix(Y,nrow=length(Y),ncol=1)
    if( ! is.null(in.values)){
      in.values=matrix(in.values,nrow=dimension,ncol=1)
    }
  }
  else{
    locations=dim(Y)[2]
  }
  
  nt=length(Y)
  
  if(is.null(in.values)){
    in.values=runif((dimension)*locations,min=-1,max=1)#recall to move
    in.values=matrix(in.values, nrow=dimension,ncol=locations)
  }
  
  B.hat=matrix(NaN,nrow=dimension,ncol=locations)
  it=matrix(iter,nrow=1,ncol=locations)
  for(loc in 1:locations){
    y=Y[,loc]
    prev.beta=in.values[,loc]
    for (i in 1:(iter-1)) {
      pi = 1/( 1 + exp( -( design.matrix %*% prev.beta ) ) )
      beta = prev.beta + learning_rate * (t(design.matrix) %*% (y - pi))
      if(norm(beta-prev.beta,type='2') < tol){
        it[,loc]=i
        break
      }
      prev.beta=beta
    }
    B.hat[,loc]=beta
  }
  
  output=list('coefficients'=B.hat,
              'iterations'=it)
  return(output)
}

##################################
###### Generate Input Data ######
######### from raw data #########
##################################


gen.input.data = function(rawData,
                          m,
                          tau,
                          trainLen,
                          validLen=0,
                          testLen,
                          Valid=FALSE,    
                          Testing=FALSE)
{
  nt=dim(rawData)[1]
  nl=dim(rawData)[2]
  
  ######Training data
  yTrain=rawData[(m*tau+1):(trainLen),]
  
  xTrain=matrix(NA,nrow=(trainLen-(m*tau)),ncol=nl*m)
  indices=seq(from=1,by=tau,length.out=m)
  for(i in 1:(trainLen-m*tau))
  {
    xTrain[i,]=as.vector(rawData[indices,])
    indices=indices+1
  }
  
  ########Validation data, only when indicated
  if(Valid==TRUE){
    yValid=rawData[(trainLen+1):(trainLen+validLen), ]
    xValid=matrix(NA,nrow=validLen,ncol=nl*m)
    
    indices=seq(from=trainLen-(m*tau)+1,by=tau,length.out=m)
    for(i in 1:validLen)
    {
      xValid[i,]=as.vector(rawData[indices,])
      indices=indices+1
    }
  }
  else{
    yValid=NULL
    xValid=NULL
  }
  
  ########Testing data, only when indicated 
  if(Testing==TRUE){
    yTest=rawData[(trainLen+validLen+1):(trainLen+validLen+testLen), ]
  }
  else{
    yTest=NULL
  }
  if(testLen>0){
    xTest=matrix(NA,nrow=testLen,ncol=nl*m)
    indices=seq(from=trainLen+validLen-(m*tau)+1,by=tau,length.out=m)
    for(i in 1:testLen)
    {
      xTest[i,]=as.vector(rawData[indices,])
      indices=indices+1
    }
  }
  else{
    xTest=NULL
  }
  
  
  
  output = list('xTrain'=xTrain,
                'yTrain'=yTrain,
                'xValid'=xValid,
                'yValid'=yValid,
                'xTest'=xTest,
                'yTest'=yTest)
  return(output)
}
