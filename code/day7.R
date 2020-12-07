library(tidyverse)
library(igraph)         

raw = as_tibble(read_lines("inputs/7.txt"))

relationships = raw %>% 
  separate(value," contain ",into = c("outer","allInner")) %>% 
  mutate(inner = str_split(allInner,", ")) %>% 
  unnest_longer(inner) %>% 
  mutate(across(c(outer,inner),
         ~str_remove(.," bag[s]?\\.?")))

links = relationships %>% 
  filter(inner != "no other") %>% 
  separate(inner," ",into = c("weight","inner"),extra = "merge") %>% 
  mutate(weight = as.integer(weight))


g = graph_from_data_frame(links %>% select(outer,inner,weight))

# plot.igraph(g,
#             edge.label = E(g)$weight)

# find all the vertices that have a path "shiny gold"
upstream = unlist(ego(g,length(V(g)), "shiny gold",mode = "in"))

length(upstream) - 1

# part 2 ------------------------------------------------------------------

# find all the vertices that can be reached from "shiny gold" - ie live inside that bag
downstream = unlist(ego(g,length(V(g)), "shiny gold",mode = "out"))

# Find the number of bags implied by a specific path (seq of vertices), by multiplying edge weights.
# This doesn't feel optimal - surely there must be a better way to get edge weights along a path?!
getWeightForPath = function(path){
  weight = 1
  for(i in 1:(length(path) - 1)){
    eid = get.edge.ids(g,c(path[i],path[i+1]))
    weight = weight * E(g)[eid]$weight 
  }
  weight
}

# Find the number of bags implied by all paths to a particular node (ie colour)
getAllWeightsForNode = function(nodeId){
  paths = all_simple_paths(g,"shiny gold",nodeId,mode = "out")
  
  sum(map_dbl(paths,getWeightForPath))
}

# and finally sum up the totals for each downstream node
sum(map_dbl(downstream,getAllWeightsForNode))
