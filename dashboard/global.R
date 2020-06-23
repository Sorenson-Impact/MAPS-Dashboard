# Created by Sreeja Nair
# Date: Tue Jun  9 14:56:37 2020

# load libraries ----
library(tidyverse)
library(siverse)
library(googlesheets4)
library(sf)

# load data ----

spending <-  read_rds("./www/data/spending.rds")

spending_shp <- read_rds("./www/data/spending_shp.rds")

surveys_comp <- read_rds("./www/data/surveys_comp.rds")

surveys_natnl <-read_rds("./www/data/surveys_natnl.rds")

mg_cl <- read_rds("./www/data/mergers_closures.rds")


surveys_state <-  read_rds("./www/data/surveys_state.rds")








