# Spectral-Ranking-for-Abnormality

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
