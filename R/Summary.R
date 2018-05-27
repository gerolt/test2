Summary <- function (data){
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require(lubridate)) {
    install.packages("lubridate")
  }
  reserved_words <- c("if", "else", "while", "function", "for", 
                      "in", "next", "break", " TRUE", "FALSE", "NULL", "Inf", 
                      "NaN", "NA")
  if (sum(names(data) %in% reserved_words) != 0) {
    stop("변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while,\n         function, for, in, next, break, TRUE, FALSE, NULL, Inf, NaN, NA")
  }
  Card <- function(x) length(table(x))
  NofNA <- function(x) sum(is.na(x))
  con <- data[sapply(data, is.numeric)]
  cat <- data[sapply(data, is.character)]
  dat <- data[sapply(data, lubridate::is.Date)]
  message("Calculating quantitative data..")
  CalcCon <- function(x) {
    if (length(x) == 0) {
      message("No Quantitative data")
      result <- NA
    }
    else {
      con_n <- sapply(x, length)
      con_na <- sapply(x, NofNA)
      con_card <- sapply(x, Card)
      result <- data.frame(stringsAsFactors = F, names(x), 
                           con_n, con_na, con_na/con_n, con_card, con_card/con_n, 
                           sapply(x, min, na.rm = T), sapply(x, Quantile, 
                                                             0.25), sapply(x, median, na.rm = T), sapply(x, 
                                                                                                         mean, na.rm = T), sapply(x, Quantile, 0.75), 
                           sapply(x, max, na.rm = T), sapply(x, sd, na.rm = T))
      names(result) <- c("Variable", "n", "Missing", "Missing.R", 
                         "Cardinality", "Cardinality.R", "Minimum", "Q1", 
                         "Mean", "Median", "Q3", "Maximum", "Std")
    }
    return(result)
  }
  con_dat <- CalcCon(con)
  message("Calculating date data..")
  dat_dat <- CalcCon(dat)
  for (i in c("Minimum", "Q1", "Mean", "Median", "Q3", "Maximum")) {
    dat_dat[[i]] <- as.Date(dat_dat[[i]], origin = "1970-1-1") %>% as.Date
  }
  message("Calculating qualitative data..")
  CalcCat <- function(x) {
    if (length(x) == 0) {
      message("No Qualitative data")
      result <- NA
    }
    else {
      cat_n <- sapply(x, length)
      cat_na <- sapply(x, NofNA)
      cat_card <- sapply(x, Card)
      catm1c <- sapply(x, GetMode, order = 1, type = "count")
      catm2c <- sapply(x, GetMode, order = 2, type = "count")
      result <- data.frame(stringsAsFactors = F, names(x), 
                           cat_n, cat_na, cat_na/cat_n, cat_card, cat_card/cat_n, 
                           sapply(x, GetMode, order = 1, type = "value"), 
                           catm1c, catm1c/cat_n, sapply(x, GetMode, order = 2, 
                                                        type = "value"), catm2c, catm2c/cat_n)
      names(result) <- c("Variable", "n", "Missing", "Missing.R", 
                         "Cardinality", "Cardinality.R", "FirstMode", 
                         "FirstMode.C", "FirstMode,R", "SecondMode", "SecondMode.C", 
                         "SecondMode.R")
      result$FS.R <- (result$FirstMode.C + result$SecondMode.C)/result$n
    }
    return(result)
  }
  cat_dat <- CalcCat(cat)
  output <- list(Qualitative = cat_dat, Quantitative = con_dat, 
                 Date = dat_dat)
  message("Done")
  invisible(output)
}
