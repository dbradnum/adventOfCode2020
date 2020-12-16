library(tidyverse)

raw = read_lines("inputs/16example.txt")

ticketHeaderLines = which(str_detect(raw,"ticket"))

rules = raw[1:(ticketHeaderLines[1] - 2)] %>% 
  as_tibble() %>% 
  separate(value,into = c("fieldName","fieldRange"),sep = ": ") %>% 
  mutate(range = str_split(fieldRange," or "),
         fieldId = row_number()) %>% 
  unnest_longer(range) %>% 
  separate(range,into = c("min","max"),sep = "-") %>% 
  mutate(across(c(min,max), as.integer)) %>% 
  select(fieldId,fieldName,min,max)

myTicket = raw[ticketHeaderLines[1] + 1]

nearbyTickets = raw[(ticketHeaderLines[2] + 1):length(raw)] %>% 
  as_tibble() %>% 
  mutate(fieldVal = str_split(value,pattern = ","),
         ticketId = row_number()) %>% 
  unnest_longer(fieldVal) %>% 
  mutate(fieldVal = as.integer(fieldVal)) %>% 
  select(ticketId,fieldVal)

rules %>% 
  crossing(nearbyTickets) %>% 
  mutate(inRange = (fieldVal >= min) & (fieldVal <= max)) %>% 
  group_by(ticketId,fieldVal) %>% 
  summarise(nValid = sum(inRange)) %>%
  ungroup() %>% 
  filter(nValid == 0) %>% 
  summarise(answer = sum(fieldVal))
