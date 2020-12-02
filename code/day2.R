library(tidyverse)

raw = read_csv("inputs/2.txt",
               col_names = "instruction")

cleaned = raw %>% 
  separate(instruction, 
           into = c("min","max", "char","space","string"),
           sep = "[ \\-\\:]",
           remove = F) %>% 
  select(-space) %>% 
  mutate(across(c(min,max),as.integer))

cleaned %>% 
  mutate(count = str_count(string,char),
         valid = (min <= count) & (max >= count)) %>% 
  summarise(result = sum(valid))

# part 2 ------------------------------------------------------------------

cleaned %>% 
  mutate(first = str_sub(string,min,min),
         second = str_sub(string,max,max),
         firstvalid = first == char,
         secondvalid = second == char,
         valid = (firstvalid + secondvalid == 1)) %>% 
  summarise(result = sum(valid))
