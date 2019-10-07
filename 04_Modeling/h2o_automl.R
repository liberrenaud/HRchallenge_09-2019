#H20 Modelling----

library(h2o)
library(recipes)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)


source("00_Functions/HR_Missing_Data_Transformation.R")

#1. Set up----

path_train <- "00_Data/train_LZdllcl.csv"
path_test  <- "00_Data/test_2umaH9m.csv"


train_raw <- read_csv(path_train) %>% HR_Data_Missing_Value()
test_raw  <- read_csv(path_test)  %>% HR_Data_Missing_Value()



#Data transformation with Recipte


recipe_h2o <- recipe(is_promoted~.,data=train_raw) %>%  
  step_num2factor(awards_won,no_of_trainings,length_of_service,age) %>% 
  prep()
  
train_tbl <- bake(recipe_h2o,new_data=train_raw)
test_tbl <- bake(recipe_h2o,new_data=test_raw)

train_tbl

#2. Modelling----

# Set up h2o - train, valid, test & column set 

h2o.init()


split_h2o <- h2o.splitFrame(as.h2o(train_tbl),ratios = c(0.85),seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

y <- "is_promoted"
x <- setdiff(names(train_h2o),y)

#Initiate h2o

automl_models_h2o <- h2o.automl(
  x=x,
  y=y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)
