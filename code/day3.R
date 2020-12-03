raw = readLines("inputs/3.txt")

split = strsplit(raw,"")

convertCharToInt = function(x){
  ifelse(x == "#",1,0)
}

# convert each list of characters to integer values, then rbind the list of results
m = do.call(rbind,
            lapply(split, convertCharToInt))


getTreeHits = function(xStep,yStep = 1){
  
  gridHeight = dim(m)[1]
  gridWidth = dim(m)[2]
  nSteps = ceiling(gridHeight / yStep) 

  # plan: find series of indices to look up in the matrix, one for each row 
  # Rather than make the matrix wider, use modulo arithmetic to 'wrap around'
  xPositions = (seq(0,nSteps - 1) * xStep) %% gridWidth + 1
  

  lookups = cbind(seq(1,gridHeight,by = yStep),xPositions)

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
                   1,2) 

prod(pmap_dbl(patterns,getTreeHits))
