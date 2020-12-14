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

getMaskCharacters = function(mask,excludeX = T){
  bits = enframe(str_split(mask,pattern = "")[[1]],
          name = "bit")  
  
  if (excludeX) {
    bits = bits %>% 
      filter(value != "X")
  }
  bits
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

# part 2 ------------------------------------------------------------------

library(arrangements)

getFloatingMaskPositions = function(mask,baseValue){
  baseInBinary = dec2bin(baseValue)
  
  baseChars = str_split(baseInBinary,pattern = "")[[1]]
  maskChars = str_split(mask,pattern = "")[[1]]
  
  overlaidChars = ifelse(maskChars == "0",baseChars,maskChars)
  
  nx = sum(overlaidChars == "X")
  # print(nx)
  
  expandedXs = permutations(c(0,1),nx,replace = T)
  
  binarySequences = matrix(rep.int(overlaidChars,nrow(expandedXs)),
                           nrow = nrow(expandedXs),
                           byrow = T)
  
  colsToExpand = which(overlaidChars == "X")

  binarySequences[,colsToExpand] = expandedXs

  binarySequences = matrix(sapply(binarySequences,as.numeric),
                           nrow = nrow(expandedXs))
    
  decimals = binarySequences %*% 2^(seq(35,0,by = -1))
  decimals
}


results = tibble()
for (i in 1:nrow(instructions)){
  iOpType = instructions$opType[i]
  iIndex = instructions$index[i]
  iValue = instructions$value[i]
  
  if (iOpType == "mask") {
    currentMask = iValue
  } else {
    positions = getFloatingMaskPositions(currentMask,as.integer(iIndex))
    
    iResult = tibble(index = i, position = as.vector(positions),value = as.integer(iValue))
    results = results %>% bind_rows(iResult)
  }
}

solution = results %>% #
  arrange(index) %>% 
  group_by(position) %>% 
  top_n(1,wt = index) %>% 
  ungroup() %>% 
  summarise(solution = sum(value)) %>% 
  pull(solution)
  

format(solution,scientific = F)

