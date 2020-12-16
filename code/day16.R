library(tidyverse)

raw = read_lines("inputs/16.txt")

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

myTicket = raw[ticketHeaderLines[1] + 1] %>% 
  as_tibble() %>% 
  mutate(fieldVal = str_split(value,pattern = ",")) %>% 
  unnest_longer(fieldVal,indices_to = "position") %>% 
  mutate(fieldVal = as.integer(fieldVal)) %>% 
  select(position,fieldVal)

nearbyTickets = raw[(ticketHeaderLines[2] + 1):length(raw)] %>% 
  as_tibble() %>% 
  mutate(fieldVal = str_split(value,pattern = ","),
         ticketId = row_number()) %>% 
  unnest_longer(fieldVal,indices_to = "position") %>% 
  mutate(fieldVal = as.integer(fieldVal)) %>% 
  select(ticketId,position,fieldVal)

validation = rules %>% 
  crossing(nearbyTickets) %>% 
  mutate(inRange = (fieldVal >= min) & (fieldVal <= max)) %>% 
  group_by(ticketId,fieldVal) %>% 
  summarise(nValid = sum(inRange)) %>%
  ungroup() 

validation %>% 
  filter(nValid == 0) %>% 
  summarise(answer = sum(fieldVal))

# part 2 ------------------------------------------------------------------

validTickets = validation %>% 
  group_by(ticketId) %>% 
  summarise(allValid = (min(nValid) > 0)) %>% 
  filter(allValid) %>%
  select(ticketId) %>% 
  inner_join(nearbyTickets)

nTickets = validTickets %>% summarise(n_distinct(ticketId)) %>% pull()
nFields = validTickets %>% summarise(n_distinct(position)) %>% pull()

# Idea: find how many positions each ticket can slot into; we'll fill from least to most, and then recheck

filledPositions = tibble(fieldId = numeric(),position = numeric() )
while (nrow(filledPositions) < nFields){
  
  validPositions = rules %>% 
    crossing(validTickets) %>% 
    anti_join(filledPositions,by = "fieldId") %>% 
    mutate(inRange = (fieldVal >= min) & (fieldVal <= max)) %>% 
    group_by(fieldId,fieldName,position) %>% 
    summarise(nValid = sum(inRange)) %>%
    ungroup() %>% 
    filter(nValid == nTickets)
  
  validOptionsPerPosition = validPositions %>% 
    count(position) %>% 
    arrange(n)
  
  nextSlot = validPositions %>% 
    filter(position == validOptionsPerPosition %>% 
             slice(1) %>% 
             pull(position) )
  
  # print(nextSlot)
  
  # growing a dataframe like this is inefficient, but we only have 20 rows to deal with...
  filledPositions = bind_rows(filledPositions,
                              nextSlot %>% select(-nValid))  
}

myTicket %>% 
  inner_join(filledPositions,by = "position") %>% 
  filter(str_starts(fieldName,"departure")) %>% 
  summarise(answer = prod(fieldVal))


