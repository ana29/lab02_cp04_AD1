read_wrangle_data <- function(){
  require(tidyverse)
  require(here)
  require(stringr)
  
  dados = read_csv(here("data/respostas-merepresenta-2016.csv"))
  respostas= dados%>% gather(key = "tema", value = "resposta", 10:23)
  
}


