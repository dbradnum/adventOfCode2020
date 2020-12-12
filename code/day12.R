library(tidyverse)
library(expm)

instructions = read_csv("inputs/12.txt",col_names = "instruction") %>%
  separate(instruction,into = c("direction","qty"),sep = 1) %>% 
  mutate(qty = as.integer(qty))

compass = list(matrix(c(1,0)), # E
               matrix(c(0,1)), # S
               matrix(c(-1,0)), # W
               matrix(c(0,-1))) # N

directionMap = 1:4
names(directionMap) = c("E","S","W","N")

position = matrix(c(0,0))
currentDirection = 1

for (i in 1:nrow(instructions)) {
  # print(instructions[i,])
  iDirection = instructions$direction[i]
  iQty = instructions$qty[i]
  
  if (iDirection %in% names(directionMap)) {
    position = position + compass[[directionMap[iDirection]]] * iQty
    
  } else if (iDirection == "F") {
    position = position + compass[[directionMap[currentDirection]]] * iQty
    
  } else {
    currentDirection = (currentDirection + 
                          ifelse(iDirection == "L",-1,1) * (iQty/90) - 1) %% 4 + 1
  }
  # print(position)
}

print(str_glue("Sum of final coords = {abs(position[1]) + abs(position[2])}"))


# part 2 ------------------------------------------------------------------

position = matrix(c(0,0))
wayPosition = matrix(c(10,-1))
currentDirection = 1

rotation = rbind(c(0,-1),
                 c(1,0))

for (i in 1:nrow(instructions)) {
  # print(instructions[i,])
  iDirection = instructions$direction[i]
  iQty = instructions$qty[i]
  
  if (iDirection %in% names(directionMap)) {
    wayPosition = wayPosition + compass[[directionMap[iDirection]]] * iQty
    
  } else if (iDirection == "F") {
    position = position + wayPosition * iQty
    
  } else {
    nRotations = (ifelse(iDirection == "L",-1,1) * (iQty/90) - 1) %% 4 + 1
    
    wayPosition = (rotation %^% nRotations) %*% matrix(c(wayPosition[[1]],
                                                          wayPosition[[2]]),
                                                          2,1)
  }
  # print("ship:")
  # print(position)
  # print("way:")
  # print(wayPosition)
}

print(str_glue("Sum of final coords = {abs(position[1]) + abs(position[2])}"))
