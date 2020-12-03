raw = readLines("inputs/3example.txt")

split = strsplit(raw,"")

convertCharToInt = function(x){
  ifelse(x == "#",1,0)
}

# convert each list of characters to integer values, then rbind the list of results
m = do.call(rbind,
            lapply(split, convertCharToInt))

m

getTreeHits = function(xStep,yStep = 1){
  
  nSteps = ceiling(dim(m)[1] / yStep) 
  gridWidth = dim(m)[2]
  
  # plan: find series of indices to look up in the matrix, one for each row 
  # Rather than make the matrix wider, use modulo arithmetic to 'wrap around'
  xPositions = (seq(0,nSteps - 1) * xStep) %% gridWidth + 1
  
  lookups = cbind(seq(1,nSteps,by = yStep),xPositions)
  
  sum(m[lookups])
}


# part1 -------------------------------------------------------------------

getTreeHits(3)


# part 2 ------------------------------------------------------------------

library(tidyverse)
patterns = tribble(~xStep,~yStep,
                   1,1,
                   3,1,
                   5,1,
                   7,1,
                   1,2) %>% 
  mutate(across(everything(),as.integer))

pmap_dbl(patterns,getTreeHits)
