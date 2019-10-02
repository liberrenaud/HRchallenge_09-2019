library(h2o)
library(glue)
library(cowplot)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id) 
  
  print(model_name)
  
  return(model_name)
  
}