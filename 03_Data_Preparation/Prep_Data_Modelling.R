
library(tidyverse)
library(tidyr)
library(data)
library(recipes)
library(magrittr)


source("00_Functions/Facet_Histogram_Data_Prep.R")


#A.Visualize the distribution of my features----

employee_data_raw %>% 
  select(is_promoted,everything()) %>% 
  plot_hist_facet()

#Data Preprocessing with Recipes----
  
  employee_data %>% 
    glimpse()
  
#1.Impute
#Was done within the data understanding part

#2.Individual transformations for skewness and other issues


          #Checking the features skewness
          skewed_features_name <- employee_data %>% 
            select_if(is.numeric) %>% 
            map_df(skewness) %>%    #Mapdf aplly skewness function to all column keeping is as a data frame
            gather(factor_key = T) %>% 
            arrange(desc(value)) %>% 
            filter(value>0.8) %>% 
            pull(key) %>% 
            as.character()
          
          #Check my features to define if any factors
          
          employee_data %>% 
            select(skewed_features_name) %>% 
            plot_hist_facet()
          
          #Factors will need to be excluded - Award won, Previous Year Rating, Is promoted
          
          
          #Checking feature skewness excluding factors
          
          skewed_features_name <- employee_data %>% 
            select_if(is.numeric) %>% 
            map_df(skewness) %>%    #Mapdf aplly skewness function to all column keeping is as a data frame
            gather(factor_key = T) %>% 
            arrange(desc(value)) %>% 
            filter(value>0.8) %>% 
            filter(!key %in% c("awards_won","previous_year_rating","is_promoted")) %>% 
            pull(key) %>% 
            as.character()
            
            
          factor_names <- c("awards_won","previous_year_rating","is_promoted")  

  #Creation of the recipe object
  recipe_obj <- recipe(is_promoted~.,data=employee_data) %>% 
    step_YeoJohnson(skewed_features_name) %>% 
    step_num2factor(factor_names)

          recipe_obj %>%   
            prep() %>% 
            bake(employee_data) %>% 
            select(skewed_features_name) %>% 
            plot_hist_facet()

#3.Normalization steps (center, scale, range, etc)

  #Adding steps to the recipe object
  recipe_obj %<>% 
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())

        recipe_obj %>%   
          prep() %>% 
          bake(employee_data) %>% 
          select(skewed_features_name) %>% 
          plot_hist_facet()


#4.Create dummy variables

recipe_obj %<>% 
  step_dummy(all_nominal())

        recipe_obj %>%   
          prep() %>% 
          bake(employee_data) %>% 
          select(factor_names) %>% 
          plot_hist_facet()

#5.Create interactions /Engineered Features----
#Might need to look into it if my algorithm is not good enough

#6.Multivariate transformation (e.g. PCA, spatial sign, etc)----
#Would be used if I have too many features but here should be fine


#Note here that we will deal with Class unbalance with my target

rec_obj <- recipe(is_promoted~.,data = employee_data) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(age,)  %>%
  step_scale(all_predictors()) 
