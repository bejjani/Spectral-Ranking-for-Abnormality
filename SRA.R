#load library for calinsky index function "index.G1"
library(clusterSim)

#' Local Maximas of a series
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
#end localMaxima

#' Data transformation by automatic categorization of numeric data using Calinski–Harabasz index
#' Gil David, Amir Averbuch (2012) SpectralCAT: Categorical spectral clustering of numerical and nominal data
#' @param data a vector of numerical variables to cluster. If data is non-numeric, the vector is returned as is
#' @param krange the range of number of clusters to test
#' @param iter.max the max number of iter for K-means
#' @param nstart the number of times K-means is run with the same k
#' @return cluster of the categorized numeric data
catcalinhara <- function (data, krange = 2:30, iter.max = 100, 
                          nstart = 25, ...) 
{
  if (!is(data, "numeric")) {
    return(data)
  }
  #data <- as.matrix(data)
  crit <- numeric(max(krange))
  km <- list()

  #for each k
  for (k in krange) {
    #cluster with k categories
    km[[k]] <- kmeans(data, k, iter.max=iter.max, nstart=nstart, ...)
    #comupte Calinski–Harabasz index
    crit[k] <- index.G1(data, km[[k]]$cluster)
  }

  #Smooth crit w/ a moving average and a span of 5
  #crit.smooth <- filter(c(rep(crit[max(krange)],4),rev(crit[krange])),rep(1/5,5),sides=1)[5:(4+length(krange))]
  #crit.smooth <- rev(crit.smooth)
  #Take first local maxima
  k.best <- krange[localMaxima(crit[krange])[1]]
  #k.best <- krange[localMaxima(crit.smooth)[1]]

  #plot calinski-harabasz indexes for each k
  plot(krange,crit[krange],type="l",lty=5,col=grey(.7),main=paste("k.best: ", k.best),xlab="k",ylab="Calinski-Harabasz index")
  points(k.best,crit[k.best],col="blue",pch=20)

  #return
  cluster=km[[k.best]]
  return(as.factor(cluster$cluster))
}
#end catcalinhara



#' Returns a vector with the nlevels for each nominal column of a data.frame
#' @param df dataframe from which to etract the number of levels for each nominal feature
#' @return vector with factors' cardinalities (nlevels for each factor of the dataframe)
cardinality <- function (df) {
  stopifnot(is.data.frame(df))
  vec <- rep(0, ncol(df))
  for(i in seq_along(df)) {
    vec[i]=nlevels(df[,i])
  }
  return(vec)
}
#end cardinality


#' Hamming distance Kernel Matrix
#' Couto, J. (2005). Kernel k-means for categorical data. In Advances in Intelligent Data Analysis VI, pp. 46–56. Springer.
#' @param df dataframe of nominal features
#' @param lambda the damping parameter in the range (0,1) (.6 and .8 best values???)
#' @return Hamming distance Kernel Matrix
hammingkernel <- function (df, lambda = .6) 
{
  #get factor features cardinalities
  card=cardinality(df)
  n = nrow(df)
  k_j = matrix(1,n,n)
  #for each feature of df, compute the recursive step to get the kernel
  for (j in 1:ncol(df)) {
    #hamming distance of feature j between all the observations
    dhamming_j = outer(df[,j],df[,j],"!=")
    k_j = ( lambda^2*(card[j]-1-dhamming_j) + (2*lambda-1)*dhamming_j + 1 ) * k_j
  }
  #normalize the kernel matrix
  d=1/sqrt(diag(k_j))
  k_j=k_j*(d%*%t(d))
  k_j
}
#end hammingkernel


#' Spectral Ranking for Abnormality (SRA)
#' K. Nian, H. Zhang, A. Tayal, T. F. Coelman, Y. Li, (2014) 'Auto Insurnace Fraud Detection Using Unsupervised Spectral Ranking for Anomaly'
#' @param W Kernel Matrix used as a similarity matrix between the observations
#' @param Xi upper bound of ratio of anomaly
#' @return z a ranking vector with a larger value representing more abnormal
#' @return mFLAG : A flag indicating ranking with respect to multiple major patterns or a single major pattern
#' @return components of the first two non-principal eigenvectors
sra <- function(W, Xi)
{
  n <- nrow(W)
  
  #Let D be the degree matrix of each vertex correponding the the row sum of the similarity matrix W
  d <- rowSums(W)
  #d_sqrt is the sqrt of the diagonal
  d_sqrt <- sqrt(d)
  #L symmetric normalized Laplacian
  L <- diag(n) - (1/d_sqrt) * W %*% diag((1/d_sqrt))
  #the 2 non-principal eigenvectors
  npeigen <- eigen(L)$vectors[,c((n-1),(n-2))]
  colnames(npeigen) <- c("np_Eigenvector_1", "np_Eigenvector_2")
  #components of the first non-principal eigenvectors in the feature space
  z = d_sqrt * npeigen
  colnames(z) <- c("np_Eigenvector_1", "np_Eigenvector_2")
  #Let Cp be the positive class and and Cn the negative class assigned based on the sign of the 1st non-principal eigenvector component of z
  C <- sign(z[,"np_Eigenvector_1"])
  #get the cardinality for each of the classes
  Ccnt = table(C)
  #Compute the Abnormality score
  if (min(Ccnt)/n >= Xi) {
    mFLAG = 1
    f = max(abs(z[,"np_Eigenvector_1"])) - abs(z[,"np_Eigenvector_1"])
  } else if (Ccnt["1"] > Ccnt["-1"]) {
      mFLAG = 0
      f = -z[,"np_Eigenvector_1"]
  } else {
      mFLAG = 0
      f = z[,"np_Eigenvector_1"]
  }
  
  #return
  list("Anomaly"=f, "mFLAG"=mFLAG, "EigenSpace"=as.data.frame(z))
}

########################################################################################################################################
########################################################################################################################################
##########################                                    EXAMPLES                                        ##########################
########################################################################################################################################
########################################################################################################################################

#Example1
#load libraries
library(kernlab)
library(ggplot2)
#load data
data(promotergene)
#transform numeric data to categorical data
df=as.data.frame(sapply(promotergene[,-1],catcalinhara))
#compute hamming distance kernel matrix
hammingkernelMatrix = hammingkernel2(df,lambda = .6)
#Perform spectral ranking 
SpectralAnomaly = sra(hammingkernelMatrix, Xi = .4)
#plot
g = ggplot(SpectralAnomaly$EigenSpace,aes(x=np_Eigenvector_1, y = np_Eigenvector_2,color=ifelse(sign(SpectralAnomaly$Anomaly)==-1,1,SpectralAnomaly$Anomaly+1))) + geom_point() + scale_color_gradient("Anomaly",trans="log",low="blue",high="red")
g = g + ggtitle(paste("mFLAG= ",SpectralAnomaly$mFLAG))
g = g + theme(legend.title = element_text(face="plain"), legend.text = element_text(color = "white"))
g

#Example2
mushroom=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data",   header=FALSE, sep=",")
mush.split=split(mushroom,mushroom$V1)
mush.e=mush.split[[1]]
mush.p=mush.split[[2]]
mush.p=mush.p[sample(nrow(mush.p),300),]
mushroom =rbind(mush.e,mush.p)
#should have 300 poisonous mushrooms
table(mushroom$V1)
#transform numeric data to categorical data
df=as.data.frame(sapply(mushroom[,-1],catcalinhara))
#compute hamming distance kernel matrix
ptm <- proc.time()
hammingkernelMatrix = hammingkernel(df,lambda = .5)
proc.time() - ptm
#Perform spectral ranking 
ptm <- proc.time()
SpectralAnomaly = sra(hammingkernelMatrix, Xi = .1)
proc.time() - ptm
#plot
g = ggplot(SpectralAnomaly$EigenSpace,aes(x=np_Eigenvector_1, y = np_Eigenvector_2,color=ifelse(sign(SpectralAnomaly$Anomaly)==-1,1,SpectralAnomaly$Anomaly+1))) + geom_point() + scale_color_gradient("Anomaly",trans="log",low="black",high="red")
g = g + ggtitle(paste("mFLAG= ",SpectralAnomaly$mFLAG))
g = g + theme(legend.title = element_text(face="plain"), legend.text = element_text(color = "white"))
g
#use both first non-principal eigenvectors for the anomaly score
g = ggplot(SpectralAnomaly$EigenSpace,aes(x=np_Eigenvector_1, y = np_Eigenvector_2,color=ifelse(sign(SpectralAnomaly$Anomaly)==-1,1,SpectralAnomaly$Anomaly+1+max(abs(SpectralAnomaly$EigenSpace[,"np_Eigenvector_2"])) - abs(SpectralAnomaly$EigenSpace[,"np_Eigenvector_2"])))) + geom_point() + scale_color_gradient("Anomaly",low="black",high="red")
g = g + ggtitle(paste("mFLAG= ",SpectralAnomaly$mFLAG))
g = g + theme(legend.title = element_text(face="plain"), legend.text = element_text(color = "white"))
g
# Test set AUC
library(ROCR)
ROCRpred = prediction(SpectralAnomaly$Anomaly, mushroom$V1)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf,colorize=T,print.cutoffs.at=seq(0,1,by=0.05),main=paste("AUC: ",as.numeric(performance(ROCRpred, "auc")@y.values)))

#Example3
breastcancer=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",   header=FALSE, sep=",")
#should have 357 benign cases and 212 malignant
table(breastcancer$V2)
#transform numeric data to categorical data
df=as.data.frame(sapply(breastcancer[,c(-1,-2)],catcalinhara))
#compute hamming distance kernel matrix
ptm <- proc.time()
hammingkernelMatrix = hammingkernel(df,lambda = .8)
proc.time() - ptm
#Perform spectral ranking 
ptm <- proc.time()
SpectralAnomaly = sra(hammingkernelMatrix, Xi = .4)
proc.time() - ptm
#plot
g = ggplot(SpectralAnomaly$EigenSpace,aes(x=np_Eigenvector_1, y = np_Eigenvector_2,color=ifelse(sign(SpectralAnomaly$Anomaly)==-1,1,SpectralAnomaly$Anomaly+1))) + geom_point() + scale_color_gradient("Anomaly",trans="log",low="black",high="red")
g = g + ggtitle(paste("mFLAG= ",SpectralAnomaly$mFLAG))
g = g + theme(legend.title = element_text(face="plain"), legend.text = element_text(color = "white"))
g
# Test set AUC
library(ROCR)
ROCRpred = prediction(SpectralAnomaly$Anomaly, breastcancer$V2)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf,colorize=T,print.cutoffs.at=seq(0,1,by=0.2),main=paste("AUC: ",as.numeric(performance(ROCRpred, "auc")@y.values)))
