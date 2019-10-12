#H20 Modelling----

library(h2o)
library(recipes)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)
library(correlationfunnel)
library(rsample)



source("00_Functions/HR_Missing_Data_Transformation.R")

#1. Set up----

path_train <- "00_Data/train_LZdllcl.csv"
path_valid  <- "00_Data/test_2umaH9m.csv"


train_raw <- read_csv(path_train) %>% HR_Data_Missing_Value()
test_valid  <- read_csv(path_valid)  %>% HR_Data_Missing_Value()

split <- train_raw %>%
  initial_split(prop = 0.80,strata = "is_promoted")

df_train <- training(split)
df_test <- testing(split)

train_raw %>% 
  binarize() %>% 
  correlate(is_promoted__1) %>% 
  plot_correlation_funnel()




#Data transformation with Recipte


recipe_h2o <- recipe(is_promoted~.,data=train_raw) %>%  
  step_num2factor(awards_won,no_of_trainings,length_of_service,age,is_promoted) %>% 
  step_rm(employee_id) %>% 
  step_string2factor(all_nominal()) %>% 
  prep()
  
train_tbl <- bake(recipe_h2o,new_data=df_train)
test_tbl <- bake(recipe_h2o,new_data=df_train)


train_tbl %>% 
  rsample::

train_tbl

#2. Modelling----

# Set up h2o - train, valid, test & column set 


#Initiate h2o
h2o.init()


#Prepare my Frames

train_h2o <- as.h2o(train_tbl)
test_h2o <- as.h2o(test_tbl)

test_h2o <- as.h2o(test_tbl)


#Actual Modelling

y <- "is_promoted"
x <- setdiff(names(train_h2o),y)



automl_models_h2o <- h2o.automl(
  x=x,
  y=y,
  training_frame = train_h2o,
  validation_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)


automl_models_h2o@leaderboard
automl_models_h2o@leader

#View a model

automl_models_h2o@leaderboard %>% 
  as.tibble() %>% 
  slice(3) %>% 
  pull(model_id) %>% 
  h2o.getModel()
  
  
#Save my leader model
  automl_models_h2o@leader %>% 
    h2o.saveModel("04_Modeling/H2O_Models/")
