anomaly_series = function(x, penaltywindow = NULL, penaltyanomaly = NULL, minimumsegmentlength = 10, warnings = TRUE, method = "meanvar"){
   
  ##### We do our error handling here
  
  if(!is.logical(warnings)){
    stop("warnings must be a logical.")
  }
  
  if(length(warnings)>1){
    warnings = warnings[1]
    if(warnings){warning("warnings should be a logical vector of length 1. Considered only the first entry.")}
  }

  tryCatch({x = as.vector(x)}, error = function(e){stop("The data x is not a vector and can not be converted to one.")}, 
             warning = function(w){if(warnings){warning("The data x is not a vector. We managed to convert it but it is probably not a good idea.")}} )
  
  if (is.null(method)){
    stop("Null argument for method.")
  }
  
  if (!is.character(method)){
    stop("Non character argument for method.")
  }
  
  if (length(method) == 0){
    stop("Argument for method has length 0.")
  }
  
  if (length(method) > 1){
    if(warnings){warning("Argument for method has length > 1 and only the first element will be used")}
    method = method[1]
  }
  
  if (!method %in% c("mean","meanvar")){
    stop("Argument for method should be either 'mean' or 'meanvar'")
  }
  
  
  if (sum(is.na(x)) > 0){
    if(warnings){warning("x contains NAs. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.na(x))]
  }
  
  if (sum(is.nan(x)) > 0){
    if(warnings){warning("x contains NaNs. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.nan(x))]
  }
  
  if (sum(is.infinite(x)) > 0){
    if(warnings){warning("x contains infinite values. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.infinite(x))]
  }
  
  if(mad(x) == 0){
    stop("x has robust variance 0. We can not run our algorithm on such data")
  }
  
  defaultreturn_minimumsegmentlength = function(){
    if(warnings){warning("The input for minimumsegmentlength is not an integer and cannot be converted to an integer. We revert to default.")}
    return(10)
  }
  
  if (!(is.numeric(x) + is.integer(x))){
    stop("x must contain numbers.")
  }
  
  minimumsegmentlength = tryCatch({as.integer(minimumsegmentlength)},
           error   = function(e){defaultreturn_minimumsegmentlength()},
           warning = function(w){defaultreturn_minimumsegmentlength()})
  
  if (length(minimumsegmentlength)>1){
    if(warnings){warning("The input for minimumsegmentlength has multiple entries. Only the first one is kept")}
    minimumsegmentlength = minimumsegmentlength[1]
  }
  
  if(is.na(minimumsegmentlength)){
    minimumsegmentlength = defaultreturn_minimumsegmentlength()
  }
  
  if(length(x) <= minimumsegmentlength){
    stop("The length of x must be longer than the minimum segment length")
  }
  
  if (method == "meanvar"){
  if(2 > minimumsegmentlength){
    if(warnings){warning("minimumsegmentlength must be at least 2. We reverted to default")}
    minimumsegmentlength = 10
  }
  }
  
  if (method == "mean"){
    if(1 > minimumsegmentlength){
      if(warnings){warning("minimumsegmentlength must be at least 1. We reverted to default")}
      minimumsegmentlength = 10
    }
  }
  
  if(length(x) <= 100){
    if(warnings){warning("The length of x is less than 100...")}
  }
  
  if(is.null(penaltywindow)){
    if (method == "meanvar"){penaltywindow = 4*log(length(x))}
    if (method == "mean"){penaltywindow = 3*log(length(x))}
  }
  
  if(is.null(penaltyanomaly)){
    penaltyanomaly = 3*log(length(x))
  }
  
  Defaultwindowpenalty = function(){
    if(warnings){warning("Non-numeric argument for penaltywindow. Default penalty used.")}
    if (method == "meanvar"){penaltywindow = 4*log(length(x))}
    if (method == "mean"){penaltywindow = 3*log(length(x))}
  }
  
  Defaultanomalypenalty = function(){
    if(warnings){warning("Non-numeric argument for penaltyanomaly. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  penaltywindow  = tryCatch({as.numeric(penaltywindow)} ,error = function(e){Defaultwindowpenalty()} , warning = function(w){Defaultwindowpenalty()} )
  penaltyanomaly = tryCatch({as.numeric(penaltyanomaly)},error = function(e){Defaultanomalypenalty()}, warning = function(w){Defaultanomalypenalty()})
  
  if(is.na(penaltywindow)){
    if(warnings){warning("penaltywindow is NA. Default penalty used.")}
    if (method == "meanvar"){penaltywindow = 4*log(length(x))}
    if (method == "mean"){penaltywindow = 3*log(length(x))}
  }
  
  if(is.nan(penaltywindow)){
    if(warnings){warning("penaltywindow is NaN. Default penalty used.")}
    if (method == "meanvar"){penaltywindow = 4*log(length(x))}
    if (method == "mean"){penaltywindow = 3*log(length(x))}
  }
  
  if(is.infinite(penaltywindow)){
    if(warnings){warning("penaltywindow is infinite. Default penalty used.")}
    if (method == "meanvar"){penaltywindow = 4*log(length(x))}
    if (method == "mean"){penaltywindow = 3*log(length(x))}
  }
  
  if(is.na(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is NA. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(is.nan(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is NaN. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(is.infinite(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is infinite. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(length(penaltyanomaly)>1){
    if(warnings){warning("penaltyanomaly has more than one entry. Only the first one is kept.")}
    penaltyanomaly = penaltyanomaly[1]
  }
  
  if(length(penaltywindow)>1){
    if(warnings){warning("penaltywindow has more than one entry. Only the first one is kept.")}
    penaltywindow = penaltywindow[1]
  }
  
  if((penaltyanomaly)<=0){
    if(warnings){warning("penaltyanomaly is less than 0!")}
  }
  
  if((penaltywindow)<=0){
    if(warnings){warning("penaltywindow is less than 0!")}
  }
  
  ##### Actual code happens below
  
  output        = list()
  output[["x"]] = x
  
  output[["anomalies_strength"]]      = data.frame(variance_change = numeric(0), mean_change = numeric(0))
  output[["pointanomalies_strength"]] = numeric(0)

  n = length(x)
  x = x - median(x)
  x = x/mad(x)
  
  if (method == "meanvar"){
  Canomalyoutput = .Call("MeanVarAnomaly", PACKAGE = "anomaly", x, as.integer(n), as.integer(minimumsegmentlength), penaltywindow, penaltyanomaly)
  }
  if (method == "mean"){
    Canomalyoutput = .Call("MeanAnomaly", PACKAGE = "anomaly", x, as.integer(n), as.integer(minimumsegmentlength), penaltywindow, penaltyanomaly)
  }  
  
  if(is.null(Canomalyoutput)){
    warning("User interrupt. NULL is returned.")
    return(NULL)
  }
  
  emptyoutput = matrix(nrow = 0,ncol = 2)
  colnames(emptyoutput) = c("start","end")
  
  
  if(length(Canomalyoutput) == 2){
      
      tmp = matrix(nrow=0,ncol=2)
      colnames(tmp) = c("start","end")
    
      output[["pointanomalies"]]          = integer(0)
      output[["anomalywindows"]]          = tmp
      return(structure(output,class="anomaly_series"))
    
  }
  
  Canomalyoutput = rev(Canomalyoutput[3:length(Canomalyoutput)])
  Canomalyoutput = as.data.frame(matrix(Canomalyoutput, ncol = 2, byrow = T))
  colnames(Canomalyoutput) = c("start","end")
  
  output[["pointanomalies"]] = Canomalyoutput[which(Canomalyoutput$start == Canomalyoutput$end),"start"]
  output[["anomalywindows"]] = Canomalyoutput[which(Canomalyoutput$start < Canomalyoutput$end),]
  
  if (length(which(Canomalyoutput$start == Canomalyoutput$end)) > 0){
    
      output[["pointanomalies_strength"]] = abs(x[output[["pointanomalies"]]])
    
  } 
  
  if (length(which(Canomalyoutput$start < Canomalyoutput$end)) > 0){
      
      meanchanges     = rep(NA,length(which(Canomalyoutput$start < Canomalyoutput$end)))
      variancechanges = rep(NA,length(which(Canomalyoutput$start < Canomalyoutput$end)))  
      
      for (ii in 1:length(which(Canomalyoutput$start < Canomalyoutput$end)) ){
        
          observation = which(Canomalyoutput$start < Canomalyoutput$end)[ii]
          
          variance = var(x[Canomalyoutput$start[observation]:Canomalyoutput$end[observation]])
          
          variancechanges[ii] = sqrt(variance) + 1/sqrt(variance) - 2
          
          meanchanges[ii]     = mean(x[Canomalyoutput$start[observation]:Canomalyoutput$end[observation]])^2/sqrt(variance)
          
      }
        
      output[["anomalies_strength"]] = data.frame(variance_change = variancechanges, mean_change = meanchanges)
      
  }
  
  return(structure(output,class="anomaly_series"))
  
}
