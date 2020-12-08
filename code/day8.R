library(tidyverse)

instructions = read_csv("inputs/8.txt",col_names = "instruction")%>% 
  separate(instruction,into = c("operation","amount"),sep = " ") %>% 
  mutate(amount = as.integer(amount))

runInstructions = function(instructions){
  
  iLine = 1
  executedLines = c()
  accumulator = 0
  nLines = nrow(instructions)
  
  while(T){
    if (anyDuplicated(executedLines)){
      print(str_glue("Line {iLine} about to be re-run. Accumulator = {accumulator}"))
      return(FALSE)
    }
    
    if (iLine > nLines){
      print(str_glue("About to run line {iLine}, which is beyond EOF. Accumulator = {accumulator}"))
      return(TRUE)
    }
    
    iOperation = instructions$operation[iLine]
    iAmount = instructions$amount[iLine]
    
    if (iOperation == "nop"){
      iLine = iLine + 1
    } 
    else if (iOperation == "acc") {
      accumulator = accumulator + iAmount
      iLine = iLine + 1
    } 
    else if (iOperation == "jmp") {
      iLine = iLine + iAmount
    } 
    else stop(str_glue("Unknown operation: {iOperation}, on line {iLine}"))
    
    executedLines = c(executedLines,iLine)
  }
}

runInstructions(instructions)

# part 2 ------------------------------------------------------------------

possibleSwitches = which(instructions$operation == "nop" | 
                           instructions$operation == "jmp") 

amendInstructions = function(instructions,iLineToSwitch){
  amendedInstructions = instructions
  
  currentOp = instructions$operation[iLineToSwitch]
  amendedInstructions$operation[iLineToSwitch] = ifelse(currentOp == "nop","jmp","nop")

  amendedInstructions
}

for (iSwitch in 1:length(possibleSwitches)){
  amendedInstructions = amendInstructions(instructions,possibleSwitches[iSwitch])
  success = runInstructions(amendedInstructions)
  if(success) break
}
