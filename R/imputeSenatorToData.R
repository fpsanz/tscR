## asignar clusters de senators a resto de datos
imputeSenatorToData <- function(senators, combined){
  data <- senators$data
  data <- data %>% mutate_at(vars(senators), as.character)
  senData <- senators$senatorData
  combinedCluster <- data.frame(senId = rownames(senData), combClust =  paste("end", combined$clustering, sep=""))
  combinedCluster <- combinedCluster %>% mutate_all(as.character)
  endData <- left_join(data, combinedCluster, by = c("senators" = "senId"))
  finalData <- new("imputeSenator", data = as.data.frame(endData[,1:(ncol(endData)-2)]),
                   senators = gsub("sen","",endData$senators),
                   endCluster = gsub("end","",endData$combClust)
  )
  return(finalData)
}
