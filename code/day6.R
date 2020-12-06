library(tidyverse)

CRLF = "\\r\\n"
recordSeparator = strrep(CRLF,2)

raw = str_split(
  read_file("inputs/6.txt"),
  recordSeparator)

tibble(fullText = unlist(raw)) %>% 
  mutate(rowID = row_number(),
         fullText = str_replace_all(fullText,CRLF,""),
         allChars = str_split(fullText,""),
         distinctChars = map(allChars,unique),
         nDistinct = lengths(distinctChars)) %>% 
  summarise(answer = sum(nDistinct))


# part 2 ------------------------------------------------------------------

tibble(fullText = unlist(raw)) %>% 
  mutate(rowID = row_number(),
         answers = str_split(fullText,CRLF)) %>% 
  unnest_longer(answers) %>% 
  mutate(allChars = str_split(answers,"")) %>% 
  group_by(rowID) %>% 
  summarise(intersection = list(reduce(allChars,intersect))) %>% 
  summarise(answer = sum(lengths(intersection)))
