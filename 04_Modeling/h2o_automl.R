library(h2o)
library(recipes)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(cowplot)
library(fs)
library(glue)


source("00_Functions/HR_Missing_Data_Transformation.R")


path_train <- "00_Data/train_LZdllcl.csv"
path_test  <- "00_Data/test_2umaH9m.csv"


train_raw <- read_csv(path_train) %>% HR_Data_Missing_Value()
test_raw  <- read_csv(path_test)  %>% HR_Data_Missing_Value()



#Data transformation with Recipte


train_raw