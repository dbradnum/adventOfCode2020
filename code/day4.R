library(tidyverse)

CRLF = "\\r\\n"
recordSeparator = strrep(CRLF,2)

raw = str_split(
  read_file("inputs/4.txt"),
  recordSeparator)

longForm = tibble(fullText = unlist(raw)) %>% 
  mutate(rowID = row_number(),
         fullText = str_replace_all(fullText,CRLF," "),
         pieces = str_split(fullText," ")) %>% 
  select(-fullText) %>% 
  unnest_longer(pieces) %>% 
  separate(pieces,":", into = c("key","value")) 

longForm %>% 
  mutate(present = !is.na(value))%>% 
  pivot_wider(id_cols = rowID, 
              names_from = key,
              values_from = present,
              values_fill = F) %>% 
  mutate(totalPresent = byr + iyr + eyr + hgt + hcl + ecl + pid + cid,
         valid = (totalPresent == 8 | 
                    (totalPresent == 7 & !cid))) %>% 
  summarise(answer = sum(valid))


# part 2 ------------------------------------------------------------------

digitsInRange = function(input,min,max){
  intVal = as.integer(input)
  !is.na(intVal) & intVal >= min & intVal <= max
}

# sure this could be done more elegantly, but time is short...
validateHeight = function(input){
  toCheck = tibble(units = str_sub(input,-2),
                   value = str_sub(input,1,-3))
  
  toCheck %>% 
    mutate(valid = case_when(units == "cm" ~ digitsInRange(value,150,193),
                             units == "in" ~ digitsInRange(value,59,76),
                             TRUE ~ FALSE)) %>% 
    pull(valid)
                     
}

longForm %>% 
  pivot_wider(id_cols = rowID, 
              names_from = key,
              values_from = value) %>% 
  mutate(byrValid = digitsInRange(byr,1920,2002),
         iyrValid = digitsInRange(iyr,2010,2020),
         eyrValid = digitsInRange(eyr,2020,2030),
         hgtValid = validateHeight(hgt),
         hclValid = str_length(hcl) == 7 & str_detect(hcl,"#[0-9a-f]{6}"),
         eclValid = ecl %in% c("amb","blu","brn","gry","grn","hzl","oth"),
         pidValid = str_length(pid) == 9 & str_detect(pid,"[0-9]{9}")
  ) %>%
  # this is ugly...
  mutate(valid = byrValid & iyrValid & eyrValid & hgtValid & hclValid & eclValid & pidValid) %>% 
  summarise(answer = sum(valid,na.rm = T))
