
plot.anomaly_series = function(x,xlab="",ylab="",...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  plot(anomaly_object[["x"]],ylab=ylab,xlab=xlab)
  
  points(anomaly_object[["pointanomalies"]],anomaly_object[["x"]][anomaly_object[["pointanomalies"]]],col="red")
  
  abline(v = anomaly_object[["anomalywindows"]][,"start"],col="red")
  abline(v = anomaly_object[["anomalywindows"]][, "end" ],col="red")
  
}
