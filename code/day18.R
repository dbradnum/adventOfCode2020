library(tidyverse)

raw = read_lines("inputs/18.txt")

hasBrackets = function(eqn){
  str_detect(eqn,"[()]")
}

hasOperators = function(eqn) {
  str_detect(eqn,"[\\+\\*]")
}

removeInnerBrackets = function(eqn,additionFirst = F)
{
  expression = "\\([0-9 \\+\\*]*\\)"
  innermost = str_extract(eqn,expression)
  
  # print(str_glue("innermost brackets: {innermost}"))
  innerEval = evaluateFromLeft(str_remove_all(innermost,"[\\(\\)]"),
                               additionFirst)
  
  # print(str_glue("Replacing {innermost} with {innerEval}..."))
  simplified = str_replace(eqn,
                           fixed(innermost),
                           innerEval)
  
  simplified
}

evaluateFromLeft = function(eqn,additionFirst = F){
  if (hasOperators(eqn)){
    if (additionFirst & str_detect(eqn,"\\+")){
      expression = "[0-9 ]+[\\+][0-9 ]+"
    } else {
      expression = "^[0-9 ]+[\\+\\*][0-9 ]+"
    }
    
    toReplace = str_extract(eqn,expression)
    
    replaceEval = as.character(eval(parse(text = toReplace)))
    
    # print(str_glue("Replacing {toReplace} with {replaceEval}..."))
    simplified = str_replace(eqn,fixed(toReplace),replaceEval)
    
    return(evaluateFromLeft(simplified,additionFirst))
  } else {
    return(as.numeric(eqn))
  }
}

processEquation = function(eqn,additionFirst = F){
  if (hasBrackets(eqn)){
    eqn = removeInnerBrackets(eqn,additionFirst)
    processEquation(eqn,additionFirst)
  } else {
    evaluateFromLeft(eqn,additionFirst)
  }
}

map_dbl(raw,processEquation) %>% sum %>% format(scientific = F)


# part 2 ------------------------------------------------------------------

raw
map_dbl(raw,processEquation,T) %>% sum %>% format(scientific = F)

