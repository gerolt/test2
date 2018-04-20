Quantile <- function(x, p, origin = "1970-01-01"){
  if(!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
  if(lubridate::is.Date(x)){
    result <- as.Date(quantile(as.numeric(x), p, na.rm = T),
                      origin = origin)
  } else {
    result <- quantile(x,p,na.rm = T)
  }
  return(result)
}
