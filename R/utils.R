lag <- function(x){
  if(length(x) > 1){
    c(x[1], x[seq_len(length(x)-1)])
  }else{
    x[1]
  }
}

lead <- function(x){
  if(length(x) > 1){
    c(x[1+seq_len(length(x)-1)], x[length(x)])
  }else{
    x[length(x)]
  }
}

`%|%` <- function(x, y){
  if(is.null(x)){
    y
  }else{
    ifelse(is.na(x), y, x)
  }
}
