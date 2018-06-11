library(MASS)
library(mice)


# impute ------------------------------------------------------------------

createData <- function(data, missingnessType, amount){
  if(missingnessType == "MCAR"){
    a <- 1:length(data$Age)
    generatedData <- data
    #This missingness index can be used to identify those values that go missing
    missingnessIndex <- sample(a,round(length(data$Age)*amount))
    generatedData[missingnessIndex,2] <- NA
    
  }
  if(missingnessType=="MAR"){
    #data <- arrange(data,data$Age)
    data <- data[order(data$Age),]
    data1234 <-data[data$Age==1 |data$Age==2 |data$Age==3 |data$Age==4,]
    data56 <- data[data$Age==5 | data$Age==6,]
    
    #rownames(data56) <- seq(length(data56$Age))
    #rownames(data1234) <- seq(length(data1234$Age))
    a1 <- 1:length(data56$Age)
    a2 <- 1:length(data1234$Age)
    missingnessIndex1234 <- sample(a2,round(length(data1234$Age)*amount))
    if (amount + 0.2 >= 1) amount=0.8
    missingnessIndex56 <- sample(a1,round(length(data56$Age)*(amount+0.2)))
    data56[missingnessIndex56,2] <- NA
    data1234[missingnessIndex1234,2] <- NA
    generatedData <- data.frame(rbind(data1234,data56))
    missingnessIndex <- c(missingnessIndex1234,missingnessIndex56+length(data1234$Age))
  }
  if(missingnessType=="MNAR1"){
    data <- data[order(data$Total),]
    #data <- arrange(data,data$Total)
    
    data0 <-data[data$Total < 120,]
    data120 <- data[data$Total>=120,]
    
    #rownames(data120) <- seq(length(data120$Age))
    #rownames(data0) <- seq(length(data0$Age))
    a1 <- 1:length(data120$Age)
    a2 <- 1:length(data0$Age)
    missingnessIndex0 <- sample(a2,round(length(data0$Age)*amount))
    if (amount + 0.2 >= 1) amount=0.8
    missingnessIndex120 <- sample(a1,round(length(data120$Age)*(amount+0.2)))
    data120[missingnessIndex120,2] <- NA
    data0[missingnessIndex0,2] <- NA
    generatedData <- data.frame(rbind(data0,data120))
    missingnessIndex <- c(missingnessIndex0,missingnessIndex120+length(data0$Age))
  }
  if(missingnessType=="MNAR2") {
    data <- data[order(data$Total),]
    #data <- arrange(data,data$Total)
    
    index <- data$Days>0
    data2 <- data[index,]
    dataLarge<- data2[(data2$Total/data2$Days)>=5,]
    dataSmall1 <-data2[data2$Total/data2$Days < 5,]
    dataSmall2 <- data[index==FALSE,]
    dataSmall <- data.frame(rbind(dataSmall1,dataSmall2))
    
    a1 <- 1:length(dataLarge$Age)
    a2 <- 1:length(dataSmall$Age)
    missingnessIndexSmall <- sample(a2,round(length(dataSmall$Age)*amount))
    if (amount + 0.2 >= 1) amount=0.8
    missingnessIndexLarge <- sample(a1,round(length(dataLarge$Age)*(amount+0.2)))
    dataLarge[missingnessIndexLarge,2] <- NA
    dataSmall[missingnessIndexSmall,2] <- NA
    generatedData <- data.frame(rbind(dataSmall,dataLarge))
    missingnessIndex <- c(missingnessIndexSmall,missingnessIndexLarge+length(dataSmall$Age))
    
  }
  # First is index telling what rows are missing from the original data.
  # Then returns the new data with missingness.
  return(list(missingnessIndex = missingnessIndex, generatedData = generatedData))
}

# Imputation itself. Gets data which has missing values and information
# about what method to use in "mice" function. 
imputointi <- function(generatedData, method, m) {
  imp1 <- mice(generatedData[,1:3],m=m,  meth=method, print=F)
  impPlot <- xyplot(imp1,Total~Days|Age)
  
  #summary(imp1)
  fish1 <- with(imp1,lm(Total~-1+as.factor(Age)))
  fish1_pooled <- summary(pool(fish1))
  #print(fish1_pooled)
  impPARAM<-(fish1_pooled[1:6,1:2])
  impDATA <- imp1$imp$Total
  # Returns first table of estimated values and mean errors, then
  # the imputated values (20 columns since we have 20 imputations)
  # and lastly a plot about imputations. This plot may or may not be
  # used.
  return(list(impPARAM = impPARAM, impDATA = impDATA, impPlot = impPlot))
}


# Returns a table with imputated data mean, mean error and real data mean
tableIMP <- function(impu, aineisto) {
  impTable <- (impu[[1]])
  aineisto <- as.data.frame(aineisto)
  realMean <- NULL
  for (i in 1:6) {
    realMean[i] <-  mean(aineisto[aineisto$Age==i,2])
  }
  impTable <- data.frame(impTable,realMean)
  rownames(impTable) <- paste("Age Group", 1:6)
  impTable
}


# EM ----------------------------------------------------------------------

genData <- function(n, mu1, mu2, sigma1, sigma2, lambda) {
  xa <- mvrnorm(n, mu = mu1, Sigma = sigma1)
  xb <- mvrnorm(n, mu = mu2, Sigma = sigma2)
  p <- rbinom(n, 1, lambda)
  x <- xa
  x[which(p==0),] <- xb [which(p==0),]
  
  # construct data.frame
  isFirst <- rep("Distribution 1",n)
  isFirst[which(p==0)] <- "Distribution 2"
  df <- data.frame(x, isFirst)
  
  return(list(data = x, id = p, df = df))
}

myEM <- function(init, x, maxiter = 1000, eps = 1e-12) {
  library(mvtnorm)
  theta <- list()
  theta[[1]] <- init
  
  for (i in 2:maxiter) {
    theta.p<- theta[[i-1]] # previous theta
    R1 <- numeric(nrow(x))
    R2 <- numeric(nrow(x))
    # use log for accuracy
    for (j in 1:nrow(x)) {
      lf1 <-  dmvnorm((x[j,]),mean = theta.p$mu1, sigma = theta.p$sigma1)
      lf2 <-  dmvnorm((x[j,]),mean = theta.p$mu2, sigma = theta.p$sigma2)
      Ra <- theta.p$lambda * lf1
      Rb <- (1 - theta.p$lambda) * lf2
      R1[j] <- Ra / (Ra + Rb)
      R2[j] <- 1- R1[j]
    }
    
    lambda <- mean(R1)
    # point-wise multiplication
    mu1 <- colSums(R1 * x) / sum(R1)
    mu2 <- colSums(R2 * x) / sum(R2)
    # calculate Rt(xi - mu1)(xi - mu1) !new mu
    rxx1 <- R1[j] * (x[j,] - mu1) %*% t(x[j,] - mu1)
    rxx2 <- R2[j] * (x[j,] - mu2) %*% t(x[j,] - mu2)
    for (j in 2: nrow(x)) {
      rxx1 <- rxx1 + R1[j] * (x[j,] - mu1) %*% t(x[j,] - mu1)
      rxx2 <- rxx2 + R2[j] * (x[j,] - mu2) %*% t(x[j,] - mu2)
    }
    
    theta[[i]] <- list(mu1 = mu1,
                       mu2 = mu2,
                       sigma1 = rxx1 / sum(R1),
                       sigma2 = rxx2 / sum(R2),
                       lambda = lambda)
    # return if close enough
    eDist <- dist(rbind(mu1, theta.p$mu1)) + dist(rbind(mu2, theta.p$mu2))
    if( eDist < 2 * eps)
      return(list(all = theta, result = theta[[i]], reachMax = F))
  }
  return(list(all = theta, result = theta[[maxiter]], reachMax = TRUE))
}

randomInit <- function(mu1, mu2, sigma1, sigma2, lambda, magnitude) {
  p <- length(mu1)
  diag10 <- magnitude * diag(nrow = p)
  mu1.x <- mu1 + mvrnorm(1, rep(0,p), diag10)
  mu2.x <- mu1 + mvrnorm(1, rep(0,p), diag10)
  lambda.x <- runif(1)
  
  return(list(mu1 = mu1.x, 
              mu2 = mu2.x,
              sigma1 = diag10, 
              sigma2 = diag10,
              lambda = lambda.x))
}

