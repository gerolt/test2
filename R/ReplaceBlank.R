ReplaceBlank <- function(x, replacement = '_'){
  if(!require(stringr)) {install.packages('stringr'); library(stringr)}
  x <- str_replace_all(x, '^[[:blank:]]|[[:blank:]]$', '') # Removing leading and trailing space
  x <- str_replace_all(x, '[[:blank:]]{1,}', replacement)
  return(x)
}
