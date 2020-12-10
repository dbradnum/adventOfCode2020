library(tidyverse)
library(arrangements)

raw = read_csv("inputs/10.txt",col_names = "joltage")

myadapter = max(raw$joltage) + 3

toAdd = tibble(joltage = c(0,myadapter))

diffs = raw %>% 
  bind_rows(toAdd) %>% 
  arrange(joltage) %>% 
  mutate(diff = joltage - lag(joltage)) 

diffs %>% 
  count(diff)  %>% 
  pivot_wider(names_from = diff,
              names_prefix = "diff",
              values_from = n) %>% 
  transmute(answer = diff3 * diff1)


# part 2 ------------------------------------------------------------------

# we need to find contiguous sequences of 1-differences - 
# we can convert some of the 1s to 2s or 3s, implying adapter subsequences. e.g.

# 3113 could optionally be switched to 323
# 31113 could become 3123 , 3213, 333

# if we find the number of permutations for each distinct sequence of 1-differences, 
# then the total number of adapter combos is the product of the independent sequence permutations

# Qu: how can we find the number of permutations for a sequence of n 1s ?
# A:  build up all permutations of 1s, 2s, and 3s

integersToPermute = 1:3

# Get all n-digit permutations of our integers, with replacement, and the sum of digits for each
getIntegerPerms = function(permLength,ints){
  tibble(perm = permutations(ints, permLength, replace = TRUE,layout = "list")) %>% 
    rowwise %>% 
    mutate(digitSum = sum(perm))
}

# Get our initial string of differences
diffString = diffs %>% 
  filter(!is.na(diff)) %>%  
  pull(diff) %>% 
  paste0(collapse = "")

# How many sequences of 1s do we have, for each distinct length?
sequencesOf1s = str_extract_all(diffString,pattern = "1+") %>% 
  unlist() %>% 
  enframe() %>% 
  mutate(seqLength = str_length(value)) %>% 
  count(seqLength,name = "nSequences") %>% 
  filter(seqLength > 1)

# Find the longest sequence we need to deal with
maxSeqLength = max(sequencesOf1s$seqLength)

# and generate all the possible permutations up to that length
allPerms = map_dfr(1:maxSeqLength,
                   ~getIntegerPerms(.,integersToPermute))

# create a mapping table that converts each sequence length to the number of perms 
seqLengthMapping = allPerms %>% 
  filter(digitSum <= maxSeqLength) %>% 
  count(digitSum,name = "perms")

# finally - multiply all the permutations of each sequence of 1s
sequencesOf1s %>% 
  inner_join(seqLengthMapping, by = c("seqLength" = "digitSum")) %>% 
  summarise(answer = prod(perms^nSequences))

