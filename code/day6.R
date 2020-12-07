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
         nDistinct = map_int(allChars,
                             ~length(unique(.)))
         ) %>% 
  summarise(answer = sum(nDistinct))


# part 2 ------------------------------------------------------------------

tibble(fullText = unlist(raw)) %>% 
  mutate(rowID = row_number(),
         answers = str_split(fullText,CRLF),
         nChars = map_int(answers,
                          ~str_split(.,"") %>% 
                            reduce(intersect) %>% 
                            length)) %>% 
  summarise(answer = sum(nChars))



