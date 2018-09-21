plot.anomaly_series = function(x,xlab="",ylab="",...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  tmpdaf = data.frame(x = 1:length(anomaly_object[["x"]]),y = anomaly_object[["x"]])
  
  output = ggplot(tmpdaf,aes_string(x="x",y="y"))
  
  if (length(anomaly_object[["pointanomalies"]])>0.5){
    pointanomalydaf = data.frame(x=anomaly_object[["pointanomalies"]],y=anomaly_object[["x"]][anomaly_object[["pointanomalies"]]])
    output = output + geom_point(data = pointanomalydaf,aes_string(x="x",y="y"), colour="red", size=2)
  }
  
  if (nrow(anomaly_object[["anomalywindows"]])>0){
    collectiveanomalydaf = anomaly_object[["anomalywindows"]]
    collectiveanomalydaf$ymax =  Inf
    collectiveanomalydaf$ymin = -Inf
    output = output + geom_rect(data = collectiveanomalydaf,inherit.aes = F,mapping = aes_string(xmin="start",xmax="end",ymin="ymin",ymax="ymax"),fill="red",alpha=0.5)
  }
  
  output = output+geom_point()+ labs(x=xlab,y=ylab)
  
  return(output)
  
}
