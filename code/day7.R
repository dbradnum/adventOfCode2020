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
  separate(inner," ",into = c("noOfBags","inner"),extra = "merge")


g = graph_from_data_frame(links %>% select(inner,outer))

# plot(g)

connected = ego(g,length(V(g)), "shiny gold",mode = "out")

length(connected[[1]]) - 1


# part 2 ------------------------------------------------------------------


