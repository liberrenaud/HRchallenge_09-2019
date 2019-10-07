
library(tidyverse)
library(tidyr)
library(data)
library(recipes)
library(magrittr)


source("00_Functions/Facet_Histogram_Data_Prep.R")

source("00_Functions/HR_Missing_Data_Transformation.R")


path_train <- "00_Data/train_LZdllcl.csv"
path_test  <- "00_Data/test_2umaH9m.csv"


train_raw <- read_csv(path_train) %>% HR_Data_Missing_Value()
test_raw  <- read_csv(path_test)  %>% HR_Data_Missing_Value()

#A.Visualize the distribution of my features----

train_raw %>% 
  select(is_promoted,everything()) %>% 
  plot_hist_facet()

#Data Preprocessing with Recipes----
  
train_raw %>% 
    glimpse()
  
#1.Impute
#Was done within the data understanding part

#2.Individual transformations for skewness and other issues


          #Checking the features skewness
          skewed_features_name <- train_raw %>% 
            select_if(is.numeric) %>% 
            map_df(skewness) %>%    #Mapdf aplly skewness function to all column keeping is as a data frame
            gather(factor_key = T) %>% 
            arrange(desc(value)) %>% 
            filter(value>0.8) %>% 
            pull(key) %>% 
            as.character()
          
          #Check my features to define if any factors
          
          train_raw %>% 
            select(skewed_features_name) %>% 
            plot_hist_facet()
          
          #Factors will need to be excluded - Award won, Previous Year Rating, Is promoted
          
          
          #Checking feature skewness excluding factors
          
          skewed_features_name <- train_raw %>% 
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
  recipe_obj <- recipe(is_promoted~.,data=train_raw) %>% 
    step_YeoJohnson(skewed_features_name) %>% 
    step_num2factor(factor_names)

          recipe_obj %>%   
            prep() %>% 
            bake(train_raw) %>% 
            select(skewed_features_name) %>% 
            plot_hist_facet()

#3.Normalization steps (center, scale, range, etc)

  #Adding steps to the recipe object
  recipe_obj %<>% 
    step_center(all_numeric()) %>% 
    step_scale(all_numeric())

        recipe_obj %>%   
          prep() %>% 
          bake(train_raw) %>% 
          select(skewed_features_name) %>% 
          plot_hist_facet()


#4.Create dummy variables

recipe_obj %<>% 
  step_dummy(all_nominal())

        recipe_obj %>%   
          prep() %>% 
          bake(train_raw) %>% 
          #select_if(is.factor) %>%   Need to fix here issue to only select the factors
          plot_hist_facet()

#5.Create interactions /Engineered Features----
#Might need to look into it if my algorithm is not good enough

#6.Multivariate transformation (e.g. PCA, spatial sign, etc)----
#Would be used if I have too many features but here should be fine


#Note here that we will deal with Class unbalance with my target

rec_obj <- recipe(is_promoted~.,data = train_raw) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(age,)  %>%
  step_scale(all_predictors()) 

        