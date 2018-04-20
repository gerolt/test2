UnifyBracket <- function(x, type = 1){
  if(!require(stringr)) {install.packages('stringr'); library(stringr)}
  replacement <- c('\\(', '\\)')
  if(type == 2){
    replacement <- c('\\{', '\\}')
  } else if(type ==3){
    replacement <- c('\\[', '\\]')
  }
  x <- str_replace_all(x, '\\(|\\[|\\{', replacement[1])
  x <- str_replace_all(x, '\\)|\\]|\\}', replacement[2])
  return(x)
}
