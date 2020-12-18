library(tidyverse)

raw = read_lines("inputs/18example.txt")

hasBrackets = function(eqn){
  str_detect(eqn,"[()]")
}

hasOperators = function(eqn) {
  str_detect(eqn,"[\\+\\*]")
}

removeInnerBrackets = function(eqn)
{
  expression = "\\([0-9\\+\\*]*\\)"
  innermost = str_extract(eqn,"\\([0-9\\+\\*]*\\)")
  simplified = str_replace(eqn,
                           innermost,
                           evaluateFromLeft(innermost))
  simplified
}

evaluateFromLeft = function(eqn){
  if (hasOperators(eqn)){
    expression = "^[0-9 ]+[\\+\\*][0-9 ]+"
    leftMost = str_extract(eqn,expression)
    print(leftMost)
    
    leftEval = as.character(eval(parse(text = leftMost)))
    print(leftEval)
    
    simplified = str_replace(eqn,leftMost,leftEval)
    
    evaluateFromLeft(simplified)
  } else {
    return(as.numeric(eqn))
  }
}

processEquation = function(eqn){
  if (hasBrackets(eqn)){
    eqn = removeInnerBrackets(eqn)
    processEquation(eqn)
  } else {
    evaluateFromLeft(eqn)
  }
}

processEquation("2+3")
