library(tidyverse)

raw = read_csv("inputs/14.txt",col_names = "instruction")

instructions = raw %>% 
  separate(instruction,into = c("opType","value"),sep = " = ") %>% 
  separate(opType,into = c("opType","index"),sep = "[\\[\\]]",fill = "right",extra = "drop") %>% 
  mutate(index = as.integer(index))


dec2bin <- function(x) {
  bit32 = paste(as.integer(rev(intToBits(x))), collapse = "")
  paste0("0000",bit32)
}

maskAndConvert = function(mask,baseValue){
  baseInBinary = dec2bin(baseValue)
  
  maskBits = getMaskCharacters(mask) %>% rename(maskVal = value)
  baseBits = getMaskCharacters(baseInBinary) %>% rename(baseVal = value)
  
  baseBits %>% 
    left_join(maskBits,by = "bit") %>% 
    mutate(final = coalesce(maskVal,baseVal)) %>%
    filter(final == 1) %>% 
    summarise(decimal = sum(2^(36-bit))) %>% 
    pull(decimal)
  
  }

getMaskCharacters = function(mask){
  enframe(str_split(mask,pattern = "")[[1]],
          name = "bit")  %>% 
  filter(value != "X")
}

mem = rep(NA,max(instructions$index,na.rm = T))
for (i in 1:nrow(instructions)){
  iOpType = instructions$opType[i]
  iIndex = instructions$index[i]
  iValue = instructions$value[i]
  
  if (iOpType == "mask") {
    currentMask = iValue
  } else {
    mem[iIndex] = maskAndConvert(currentMask,as.integer(iValue))
  }
}

format(sum(mem,na.rm = T),scientific = F)


