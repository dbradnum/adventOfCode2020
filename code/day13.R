library(tidyverse)

raw = read_lines("inputs/13.txt")

startTime = as.integer(raw[1])

buses = str_split(raw[2],",") 

timeToWait = function(busNo,startTime){
  (startTime %/% busNo + 1) * busNo - startTime
}

busTbl = enframe(unlist(buses),name = "index",value = "busNo") %>% 
  filter(busNo != "x") %>% 
  mutate(busNo = as.integer(busNo))

busTbl %>%
  mutate(waitTime = map_dbl(busNo,~timeToWait(.,startTime)),
         prod = busNo * waitTime) %>% 
  top_n(1,-waitTime) %>% 
  pull(prod)


