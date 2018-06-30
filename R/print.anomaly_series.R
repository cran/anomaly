
print.anomaly_series = function(x,...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",paste(unexpectedarguments,", "),"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("location\t")
  cat("strength\t")
  cat("\n")
  if (length(anomaly_object[["pointanomalies"]]) > 0){
    for (ii in 1:length(anomaly_object[["pointanomalies"]]) ){
      cat(anomaly_object[["pointanomalies"]][ii])
      cat("\t")
      cat("\t")
      cat(anomaly_object[["pointanomalies_strength"]][ii])
      cat("\n")
    }
  }
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  cat("\n")
  cat("start\t")
  cat("end\t")
  cat("mean change\t")
  cat("variance change\t")
  cat("\n")
  if (nrow(anomaly_object[["anomalywindows"]]) > 0){
    for (ii in 1:nrow(anomaly_object[["anomalywindows"]]) ){
      cat(anomaly_object[["anomalywindows"]][ii,"start"])
      cat("\t")
      cat(anomaly_object[["anomalywindows"]][ii,"end"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"mean_change"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"variance_change"])
      cat("\n")
    }
  }
  
}
