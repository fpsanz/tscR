#calcular senators
imputeSenators <- function(x, k=100){
  if( 0.1*nrow(x) < k ){
    k = 0.1*nrow(x)
    cat(paste("Setting k to",k,". 10% of total data"))
  }
  if(k > 100){
    warning("K higher 100 has several computacional cost. Setting k to 100.")
    k=100
  }
  result <- clara(x, k)
  senatorsWeight <- as.integer(table(result$clustering))
  mySenators <- result$clustering
  senatorsWide <- result$medoids
  reOrder <- sort(table(result$clustering),decreasing=TRUE)
  mySenators <- match(mySenators,names(reOrder))
  senatorsWeight <- as.integer(reOrder)
  names(senatorsWeight) <- paste("sen",1:k,sep="")
  senatorsWide <- senatorsWide[as.integer(names(reOrder)),]
  rownames(senatorsWide) <- names(senatorsWeight)
  mySenator <- data.frame(id=x,senators=paste("sen",mySenators,sep=""))
  senatorObj<-list(mySenator=mySenator,senatorsWide=senatorsWide,senatorsWeight=senatorsWeight)
  return(senatorObj)
}
