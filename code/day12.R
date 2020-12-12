library(sets)
library(tidyverse)

instructions = read_csv("inputs/12.txt",col_names = "instruction") %>%
  separate(instruction,into = c("direction","qty"),sep = 1) %>% 
  mutate(qty = as.integer(qty))

compass = list(tuple(1,0), # E
               tuple(0,1), # S
               tuple(-1,0), # W
               tuple(0,-1)) # N

directionMap = 1:4
names(directionMap) = c("E","S","W","N")

position = tuple(0,0)
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
