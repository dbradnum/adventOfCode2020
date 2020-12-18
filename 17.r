library(tidyverse)

m = read_lines("inputs/17.txt") %>%
  str_split("",simplify = T)

nRows = dim(m)[1]
nCols = dim(m)[2]

nCyclesToRun = 6
maxSize = max(dim(m)) + 2*(nCyclesToRun)
centre = ceiling(maxSize/2)

getSurroundingArray = function(maxSize,dims = 3){
  array(NA,rep(maxSize,dims) )
}

getNext = function(m) {
  mNext = getSurroundingArray(max(dim(m)))
  
  for (row in 1:maxSize){
    for (col in 1:maxSize){
      for (z in 1:maxSize) {
        
        region = m[max(0,row-1) : min(maxSize, row+1),
                   max(0,col-1) : min(maxSize, col+1),
                   max(0,z-1) : min(maxSize, z+1)]
        
        cellVal = m[row,col,z]
        
        nOccupied = sum(region == "#",na.rm = T) - (!is.na(cellVal) & cellVal == "#")
        
        if (!is.na(cellVal) & cellVal == "#"){
          newVal = ifelse(nOccupied %in% c(2,3),"#",".")
        } else {
          newVal = ifelse(nOccupied == 3, "#",".")
        }
        mNext[row,col,z] = newVal
      }
    }
  }
  mNext
}

m3 = getSurroundingArray(maxSize)

centreRange = (nCyclesToRun + 1) : (maxSize - (nCyclesToRun))

m3[centreRange,centreRange,(maxSize + 1) / 2] = m 

i = 1
while(i <= 6){
  mNext = getNext(m3)
  # print(mNext)
  
  m3 = mNext
  
  i = i + 1
  print(sum(m3 == "#",na.rm = T))
}


# part 2 ------------------------------------------------------------------


getNext4D = function(m) {
  mNext = getSurroundingArray(max(dim(m)),dims = 4)
  
  for (row in 1:maxSize){
    for (col in 1:maxSize){
      for (z in 1:maxSize) {
        for (w in 1:maxSize) {
          
          region = m[max(0,row-1) : min(maxSize, row+1),
                     max(0,col-1) : min(maxSize, col+1),
                     max(0,z-1) : min(maxSize, z+1),
                     max(0,w-1) : min(maxSize, w+1)]
          
          cellVal = m[row,col,z,w]
          
          nOccupied = sum(region == "#",na.rm = T) - (!is.na(cellVal) & cellVal == "#")
          
          if (!is.na(cellVal) & cellVal == "#"){
            newVal = ifelse(nOccupied %in% c(2,3),"#",".")
          } else {
            newVal = ifelse(nOccupied == 3, "#",".")
          }
          mNext[row,col,z,w] = newVal
        }
      }
    }
  }
  mNext
}

m4 = getSurroundingArray(maxSize,4)

centreRange = (nCyclesToRun + 1) : (maxSize - (nCyclesToRun))
centre = (maxSize + 1) / 2

m4[centreRange,centreRange,centre,centre] = m 

i = 1
while(i <= 6){
  mNext = getNext4D(m4)
  # print(mNext)
  
  m4 = mNext
  
  i = i + 1
  print(sum(m4 == "#",na.rm = T))
}

