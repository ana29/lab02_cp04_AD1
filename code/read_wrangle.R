read_wrangle_data <- function(){
  require(tidyverse)
  require(here)
  require(stringr)
  
  read_csv(here("data/respostas-merepresenta-2016.csv"))
 }