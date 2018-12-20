


predictTest<-function(nnModel, testSet, sigThresh){
  computeProb <- compute(nnModel, testSet)
  result <- ifelse(computeProb$net.result>sigThresh, yes = 1, no = 0)
  return(result)
}