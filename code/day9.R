library(tidyverse)
library(bit64)
library(slider)


raw = as.integer64(read_lines("inputs/9.txt"))

windowSize = 25

isValid = imap_lgl(raw, function(x,i) {
  if (i <= windowSize) return(T)
  
  window = raw[(i-windowSize) : (i-1)]

  previousSums = combn(raw[(i-windowSize) : (i-1)], 
                       2,
                       FUN = sum.integer64) 
  
  # because of int64 issue - need to investigate this further
  class(previousSums) = "integer64"
  
  return (x %in% previousSums)
})

part1Solution = raw[which(!isValid)]     
part1Solution  


# part 2 ------------------------------------------------------------------

iSeqLength = 2

success = F
while (!success){
  slidingSum = slide_dbl(raw,sum,.before = iSeqLength - 1,.complete = T)
  hits =  slidingSum == part1Solution 
  
  if (sum(hits,na.rm = T) > 0 ){
    endIndex = which(hits)
    print(str_glue("Success: sequence of {iSeqLength} values ending at position {endIndex}"))
    
    range = raw[(endIndex - iSeqLength):endIndex]
    print(str_glue("Min + max = {min(range) + max(range)}"))
    
    success = T
  } else {
    iSeqLength = iSeqLength + 1
  }
}
