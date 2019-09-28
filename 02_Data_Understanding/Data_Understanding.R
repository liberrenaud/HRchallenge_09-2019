#Import Libraries----

library(tidyverse)
library(tidyr)
library(data)
library(DataExplorer)
library(correlationfunnel)
library(janitor)

install.packages("correlationfunnel")

#Import Data----
employee_data_raw <- read_csv("00_Data/train_LZdllcl.csv")

#Clean Name & Deal with NA----

employee_data <- employee_data_raw %>% 
  clean_names() %>% 
  mutate(previous_year_rating=replace_na(previous_year_rating,999),
         education           =replace_na(education,"not_provided"))  
  
  
employee_data %>% 
  glimpse()

  
#Run Data Explorer to have understanding of data at high level----
  
DataExplorer::create_report(employee_data)

#Run Correlation Funnel to understand which feature have the highest predictive power----


employee_binarize <- employee_data %>% 
  select(-employee_id) %>% 
  binarize(n_bins = 4, thresh_infreq = 0.01)



Employee_correlation_funnel <- employee_binarize %>%
  correlate(target = is_promoted__1) %>% 
  plot_correlation_funnel(interactive = FALSE)

#From the correlation funnel two elements indicate if a employee is likely to be promoted 
      #The fact that KPI were met
      #The fact that an employee won an award
# Other element like the previous year rating and the average trinaing score have as well some predicting 
# power but this is quite limited.
