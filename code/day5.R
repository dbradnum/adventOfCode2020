library(tidyverse)

raw = read_csv("inputs/5.txt",col_names = "fullText")

split = raw %>% separate(fullText,-3,
                         into = c("rowCode","seatCode"))

convertWithBinary = function(str,conversion){
  str_replace_all(str,conversion) %>% 
    strtoi(base = 2)
}

seatDetails = split %>% 
  mutate(rowNum = convertWithBinary(rowCode,
                                    c("F" = "0","B" = "1")),
         seatNum = convertWithBinary(seatCode,
                                     c("L" = "0","R" = "1")),
         seatID = rowNum * 8 + seatNum)

# part 1 ------------------------------------------------------------------

seatDetails %>% 
  slice_max(seatID,n = 1) %>% 
  pull(seatID)

# part 2 ------------------------------------------------------------------

seatDetails %>% 
  arrange(seatID) %>% 
  filter(seatID != lead(seatID) - 1) %>% 
  pull(seatID) + 1
