library(tidyverse)

raw = read_lines("inputs/13.txt")

startTime = as.integer(raw[1])

buses = str_split(raw[2],",",simplify = T) 
busNumbers = as.integer(buses[buses != "x"])

timeToWait = function(busNo,startTime){
  (startTime %/% busNo + 1) * busNo - startTime
}

tibble(busNo = busNumbers) %>% 
  mutate(waitTime = map_dbl(busNo,~timeToWait(.,startTime)),
         prod = busNo * waitTime) %>% 
  top_n(1,-waitTime) %>% 
  pull(prod)

