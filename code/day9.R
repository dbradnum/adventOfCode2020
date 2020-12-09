library(tidyverse)
library(bit64)

raw = as.integer64(read_lines("inputs/9.txt"))

windowSize = 25

isValid = imap_lgl(raw, function(x,i) {
  if (i <= windowSize) return(T)
  
  window = raw[(i-windowSize) : (i-1)]

  previousSums = combn(raw[(i-windowSize) : (i-1)], 
                       2,
                       FUN = sum.integer64,
                       simplify = F) #  because we're dealing with int64
  
  u = unlist(previousSums)
  
    # because of int64 issue - need to investigate this further
  class(u) = "integer64"
  
  return (x %in% u)
})

raw[which(!isValid)]     
  

# s = seq(10)
# test = as.integer64(s)
# combn(s,2)
# combn(test,2)
# 
# matrix(s)
# matrix(test)
