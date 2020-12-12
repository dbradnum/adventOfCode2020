library(tidyverse)
library(arrangements)

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

while(T){
  mNext = getNext(m)
  if (identical(m, mNext)) {
    totalOccupied = sum(m == "#")
    print(str_glue("No change! There are currently {totalOccupied} seats occupied."))
    break
  }
  m = mNext
}


# part 2 ------------------------------------------------------------------

mRaw = read_lines("inputs/11.txt") %>%
  str_split("",simplify = T) 

nRows = dim(mRaw)[1]
nCols = dim(mRaw)[2]

m = matrix(NA,nRows,nCols)

m[mRaw == "L"] = 0
m[mRaw == "#"] = 1
m[mRaw == "."] = NA

# get all possible directions by permuting -1, 0, and 1 - exclude 0,0
directions = permutations(c(-1,0,1),2,replace = T)
directions = directions[directions[,1] != 0 | directions[,2] != 0,]

isValidCell = function(row,col){
  1 <= row & row <= nRows & 1 <= col & col <= nCols
}

getOccupied = function(row,col)
{
  occupied = 0
  for (iDirection in 1:8)
  {
    for(jStep in 1:max(nRows,nCols)){
      nextCell = c(row,col) + directions[iDirection,] * jStep
      # print(nextCell)
      
      if (!isValidCell(nextCell[1],nextCell[2])) break
      
      nextCellVal = m[nextCell[1],nextCell[2]]
      if (is.na(nextCellVal)) next 
          
      if (!nextCellVal) {
        break
      } else {
        occupied = occupied + 1
        break
      } 
    }
  }
  occupied
}

getNext = function(m) {
  mNext = matrix(NA,nRows,nCols)
  
  for (row in 1:nRows){
    for (col in 1:nCols){
      cellVal = m[row,col]
      
      if (is.na(cellVal)) next
      
      nOccupied = getOccupied(row,col)
      
      mNext[row,col] = ifelse(cellVal == F & nOccupied == 0, T,
                              ifelse(cellVal == T & nOccupied > 4,F,
                                     cellVal))
    }
  }
  mNext
}

while(T){
  mNext = getNext(m)
  # print(mNext)

    if (identical(m, mNext)) {
    totalOccupied = sum(m,na.rm = T)
    print(str_glue("No change! There are currently {totalOccupied} seats occupied."))
    break
  }
  m = mNext
}

