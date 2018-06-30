
summary.anomaly_series = function(object,...){
  
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  anomaly_object = object
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  
}
