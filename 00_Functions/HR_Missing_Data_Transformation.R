library(tidyverse)
library(janitor)


HR_Data_Missing_Value <- function(data) {
  data %>%
    clean_names() %>% 
    mutate(previous_year_rating=replace_na(previous_year_rating,999),
           education           =replace_na(education,"not_provided")) 
}


