# Created by Sreeja Nair
# Date: Tue Jun  9 14:56:37 2020

# load libraries ----
library(tidyverse)
library(googlesheets4)
library(sf)

# load data ----

spending <-  read_rds("./www/data/spending.rds")

spending_shp <- read_rds("./www/data/spending_shp.rds")

surveys_comp <- read_rds("./www/data/surveys_comp.rds")

surveys_natnl <-read_rds("./www/data/surveys_natnl.rds")

mg_cl <- read_rds("./www/data/mergers_closures.rds")


surveys_state <-  read_rds("./www/data/surveys_state.rds")

# functions

si_scale_big_dollar <- function(x, sep = " ", suffix_n = F) {
  x <- as.numeric(x)
  limits <- c(1, 1000, 1e+06, 1e+09, 1e+12)
  suffix <- c(" ", "k", "M", "B", "T")
  if(suffix_n) suffix <- c(" ", "k", "M", "Bn", "Tn")
  i <- findInterval(abs(x), limits)
  i <- ifelse(i == 0, which(limits == 1), i)
  paste0("$", format(round(x/limits[i], 1), trim = TRUE, scientific = FALSE), sep, suffix[i])
}






