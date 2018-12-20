
source("predictTest.R")

websites <- read.csv(file = "data1.csv")
websites$Name <- as.character(websites$Name)
websites$avgResponseTime <- as.numeric(websites$avgResponseTime)
websites$Ninetyline <- as.numeric(websites$Ninetyline)
websites$Ninetynineline <- as.numeric(websites$Ninetynineline)
websites$avgLatency <- as.numeric(websites$avgLatency)
websites$stDev <- as.numeric(websites$stDev)
websites$avgThroughput <- as.numeric(websites$avgThroughput)
websites$UserRating <- as.numeric(rep(0, 88))
websites$Score <- as.numeric(rep(0, 88))
websites$Class <- as.numeric(rep(0, 88))

# ask users to rate some of the websites => URW (User Rated Websites)
numUserInput = 7
for (i in 1:numUserInput){
  x <- sample(1:88, 1)
  print(websites$Name[x])
  print(websites[x,3:8])
  webRate <- readline(prompt = "Rate the website: ")
  websites$UserRating[x] <- as.numeric(webRate)
}

# perform clustering over the entire dataset
numClusters <- 5
kmClusters <- kmeans(websites[,3:8], centers = numClusters, iter.max = 50)

# to every cluster, alot a score of sum(URW) in each cluster & each element score = score of cluster
for (i in 1:numClusters){
  websites$Score[kmClusters$cluster==i] <- sum(websites$UserRating[kmClusters$cluster==i], na.rm = T)
}

# decide a score threshold (TH) for each cluster
TH <- ceiling(0.5 * max(websites$Score, na.rm = T))

# ONE <- the set containing clusters with score greater than TH
# ZERO <- the set containing clusters with score less than TH
# Label of all elements on all clusters of set ONE = 1
websites$Class[websites$Score>TH] = as.numeric(1)

# Label of all elements on all clusters of set ZERO = 0
websites$Class[websites$Score<=TH] = as.numeric(0)

# Training dataset for supervised learning
trainIndex <- numeric()
trainSet <- websites
if(sum(websites$Class==1)<sum(websites$Class==0)){
  trainIndex <- which(websites$Class==1)
  trainSet <- websites[trainIndex,]
  if(sum(websites$Class==0)!=0){  # for when all websites are rated by the user.
    nonRatedRows <- which(websites$Class==0)
    randomNonRatedRows <- sample(nonRatedRows, size = nrow(trainSet))
    trainSet <- rbind(trainSet, websites[randomNonRatedRows,])
  }
} else{
  trainIndex <- which(websites$Class==0)
  trainSet <- websites[trainIndex,]
  if(sum(websites$Class==1)!=0){  # for when all websites are rated by the user.
    nonRatedRows <- which(websites$Class==1)
    randomNonRatedRows <- sample(nonRatedRows, size = nrow(trainSet))
    trainSet <- rbind(trainSet, websites[randomNonRatedRows,])
  }
}
trainSet <- trainSet[sample(nrow(trainSet)),]

# Supervised Learning on the clustered dataset
# Using ANN
sigThresh = 0.5

library(neuralnet)

nnModel <- neuralnet(Class~avgResponseTime+Ninetyline+Ninetynineline+avgLatency+stDev+avgThroughput, data = trainSet[1:50,], hidden = 8, rep = 10)

# Testing set
F1 <- 2.14
F2 <- 2
F3 <- 4
F4 <- 2.02
F5 <- 11.72
F6 <- 2.899
testSet <- cbind(F1, F2, F3, F4, F5, F6)
testSet <- data.frame(testSet)

testSet$F1 <- as.numeric(testSet$F1)
testSet$F2 <- as.numeric(testSet$F2)
testSet$F3 <- as.numeric(testSet$F3)
testSet$F4 <- as.numeric(testSet$F4)
testSet$F5 <- as.numeric(testSet$F5)
testSet$F6 <- as.numeric(testSet$F6)

predictTest(nnModel, testSet[,1:6], sigThresh)
