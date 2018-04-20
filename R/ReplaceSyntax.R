ReplaceSyntax <- function(x, pattern, replacement){
  if(!require(stringr)) {install.packages('stringr'); library(stringr)}

  s.char <- paste('\\', c('+', '.', '*', '[', ']', '(', ')', '?', '|'), '{1,}', sep = '')
  s.char <- paste(s.char, collapse = '|')
  if(any(str_detect(pattern, s.char)==T) & str_detect('\\', '\\\\') == F){
    message(
      'You must put "\\\\" before the special character.\nExample : c("\\\\.", "\\\\*")\nIf not using backslash, it may work incorrectly'
    )
  }

  pattern <- paste(pattern, collapse = '|')
  x <- str_replace_all(x, pattern, replacement)
  return(x)
}
