library(tidyverse)

input = read_csv("inputs/1.txt",col_names = F)

crossing(X1 = input$X1,
         X2 = input$X1) %>% 
  filter(X1 < X2,
         X1 + X2 == 2020) %>% 
  mutate(sum = X1 + X2, 
         product = X1 * X2)

crossing(X1 = input$X1,
         X2 = input$X1,
         X3 = input$X1) %>% 
  filter(X1 < X2,
         X2 < X3,
         X1 + X2 + X3 == 2020) %>% 
  mutate(sum = X1 + X2 + X3, 
         product = X1 * X2 * X3)
