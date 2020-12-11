library(tidyverse)

m = read_lines("inputs/11.txt") %>%
  str_split("",simplify = T)

nRows = dim(m)[1]
nCols = dim(m)[2]

getNext = function(m) {
  mNext = matrix(NA,nRows,nCols)
  
  for (row in 1:nRows){
    for (col in 1:nCols){
      region = m[max(0,row-1) : min(nRows, row+1),
                 max(0,col-1) : min(nCols, col+1)]
      
      cellVal = m[row,col]
      nOccupied = sum(region == "#") - (cellVal == "#")
      
      
      mNext[row,col] = ifelse(cellVal == "L" & nOccupied == 0, "#",
                              ifelse(cellVal == "#" & nOccupied > 3,"L",
                                     cellVal))

    }
  }
  mNext
}

i = 1
while(i < 5){
  print(i)
  mNext = getNext(m)
  if (identical(m, mNext)) {
    totalOccupied = sum(m == "#")
    print(str_glue("No change! There are currently {totalOccupied} seats occupied."))
    break
  }
  m = mNext
  i = i+1
}


# part 2 ------------------------------------------------------------------


