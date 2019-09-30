#calcular senators
imputeSenators <- function(x, k=100){
  require("cluster")
  if( 0.1*nrow(x) < k ){
    k = 0.1*nrow(x)
  }
  result <- clara(x, k)
  senatorsWeight <- as.integer(table(result$clustering))
  mySenators <- result$clustering
  senatorsWide <- result$medoids
  reOrder <- sort(table(result$clustering),decreasing=TRUE)
  mySenators <- match(mySenators,names(reOrder))
  senatorsWeight <- as.integer(reOrder)
  names(senatorsWeight) <- paste("sen",1:100,sep="")
  senatorsWide <- senatorsWide[as.integer(names(reOrder)),]
  rownames(senatorsWide) <- names(senatorsWeight)
  mySenator <- data.frame(id=x,senators=paste("sen",mySenators,sep=""))
  senatorObj<-list(mySenator=mySenator,senatorsWide=senatorsWide,senatorsWeight=senatorsWeight)
  return(senatorObj)
}
