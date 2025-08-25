# ==============================================================================
# Test Plotting Vectors
# ==============================================================================

# Load Required Packages
library(tidyverse) # For loading and manipulating data tables
library(ggplot2) # For plotting and visualization
library(here) # Shorthand for calling absolute pathways

# STEP 1: Load the WVO vector tables and functions 
# -------------------------------------------------------------
# 1. Load the wildlife value orientation vector coordinates from 00_load_data.R

df_month <- df_month
df_year <- df_year

# 2. Load in the functions from /scripts/utilities/my_functions.R

source(here::here("scripts/utilities/my_functions.R"))

# STEP 2: Use the functions to create data frames of WVOs and publication of interest
# -------------------------------------------------------------
## Great Falls Tribune, Publication ID = 43825
## The Billings Gazette, Publication ID = 46179
## The New York Times, Publication ID = 11561

# 2.1 Create a list of the publications
publications <- c("Great Falls Tribune", "The Billings Gazette", "The New York Times")

# 2.2 Create data frames for publication of interest at monthly and annual time steps

df_month_plot <- vector_tails_to_head(data = df_month, time_step = month_yr, publication = publications[1])
#df_month_plot <- vector_tails_to_zero(data = df_month, time_step = month_yr, publication = publications[1])

#df_year_plot <- vector_tails_to_head(df_year, year, publications[1])
df_year_plot <- vector_tails_to_zero(df_year, year, publications[1])


ggplot(df_year_plot, aes(head_x, head_y)) + 
  geom_line()

ggplot(df_month_plot, aes(head_x, head_y)) + 
  geom_line()







