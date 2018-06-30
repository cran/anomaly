period_average = function(data,period){
    

period = tryCatch({as.numeric(period)},
                                  error   = function(e){stop("period has to be a positive numeric")},
                                  warning = function(w){stop("period has to be a positive numeric")})


if (length(period)>1){
    warning("The input for period has multiple entries. Only the first one is kept")
    period = period[1]
  }
  
  if(is.na(period)){
    stop("period has to be a double. It is NA.")
  }
  
  if((period)<0){
    stop("period has to be positive.")
  }
  
  if(is.infinite(period)){
    stop("period is infinite.")
  }  
  
  if(is.nan(period)){
    stop("period has to be a double. It is NaN.")
  }
  
  data = tryCatch({as.data.frame(data)},
                    error   = function(e){stop("data has to be a data frame or something which can be converted into one")},
                    warning = function(e){stop("data has to be a data frame or something which can be converted into one")})


given_names = colnames(data)
  
  if (!("Day" %in% given_names)){
    stop("Column Day is missing in data")
  }
  
  if (!("Day" %in% given_names)){
    stop("Column Brightness is missing in data")
  }
  
  if(length(given_names) > 2){
    warning("data was expected to have 2 columns named Day and Brightness. Addidtional columns are ignored")
  }
  
  if (sum(is.infinite(data$Day)) > 0){
    warning("data$Day contains infinite values. We removed them and continued our analysis on the rest.")
    data = data[which(!is.infinite(data$Day)),]
  }
  
  if (sum(is.na(data$Day)) > 0){
    warning("data$Day contains NAs. We removed them and continued our analysis on the rest.")
    data = data[which(!is.na(data$Day)),]
  }
  
  if (sum(is.nan(data$Day)) > 0){
    warning("data$Day contains NaNs. We removed them and continued our analysis on the rest.")
    data = data[which(!is.nan(data$Day)),]
  }

  if (!(is.numeric(data$Day) + is.integer(data$Day))){
    stop("Column Day of data must contain numbers.")
  }  
  
  if (sum(is.infinite(data$Brightness)) > 0){
    warning("data$Brightness contains infinite values. We removed them and continued our analysis on the rest.")
    data = data[which(!is.infinite(data$Brightness)),]
  }
  
  if (sum(is.na(data$Brightness)) > 0){
    warning("data$Brightness contains NAs. We removed them and continued our analysis on the rest.")
    data = data[which(!is.na(data$Brightness)),]
  }
  
  if (sum(is.nan(data$Brightness)) > 0){
    warning("data$Brightness contains NaNs. We removed them and continued our analysis on the rest.")
    data = data[which(!is.nan(data$Brightness)),]
  }
  
  if (!(is.numeric(data$Brightness) + is.integer(data$Brightness))){
    stop("Column Brightness of data must contain numbers.")
  }  
  
  if(nrow(data)<3){
    stop("data has fewer than 3 rows.")
  }

    
  times = data$Day
  obs   = data$Brightness
  
  modtimes = times %% period
  
  numdatapoints = round(period/median(times[2:length(times)]-times[1:(length(times)-1)]))
  step = period/numdatapoints
  
  indices = ceiling(modtimes/step)
  
  tmp = data.frame(index = indices, observation = obs)
  
  planets <- group_by(tmp, .data$index)
  delay2 <- summarize(planets, avg = mean(.data$observation, na.rm = T))
  
  observationsavg = delay2$avg
  
  return(observationsavg)
}
