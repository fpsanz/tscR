## asignar clusters de senators a resto de datos
imputeSenatorToData <- function(senators, combined){
  data <- senators$mySenator
  data <- data %>% mutate_at(vars(senators), as.character)
  senWide <- senators$senatorsWide
  combinedCluster <- data.frame(senId = rownames(senWide), combClust =  paste("end", combined$clustering, sep=""))
  combinedCluster <- combinedCluster %>% mutate_all(as.character)
  endData <- left_join(data, combinedCluster, by = c("senators" = "senId"))
  finalData <- new("imputeSenator", data = as.data.frame(endData[,1:(ncol(endData)-2)]),
                   senators = endData$senators,
                   endCluster = endData$combClust
  )
  return(finalData)
}
