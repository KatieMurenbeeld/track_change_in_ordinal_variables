# ==============================================================================
# Load the WVO Prediction Data and Create a New Table with Vector Coordinates
# ==============================================================================

# Load Required Packages
library(tidyverse)  # For loading and manipulating data tables

# STEP 1: Load, join, and filter the prediction data and the meta data
# -------------------------------------------------------------
# 1. Load the wildlife value orientation predictions

df <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/output/predictions/grizzly_bear_05_preds_gamma_2025-01-31.csv")
meta_df <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/data/original/metadata_w_coverage_type_gamma_0.5.csv")

## 1.1 Join the two datasets by GOID = Article_ID

df_join <- df %>% 
  left_join(meta_df, by = c("Article_ID" = "GOID"))

## 1.2 Filter for articles published by 
## Great Falls Tribune, Publication ID = 43825
## The Billings Gazette, Publication ID = 46179
## The New York Times, Publication ID = 11561

df_filt <- df_join %>%
  filter(`Publication ID` %in% c(43825, 46179, 11561))

# STEP 2. From predictions create vector points based on the magnitudes and directions
# -------------------------------------------------------------
# Where magnitude = monthly (or annual) count of articles with the same score 
# And the angle is based on the predicted values (angle in degrees)
# WVO = 1 = 270deg
# WVO = 2 = 300deg
# WVO = 3 = 330deg
# WVO = 4 = 0deg
# WVO = 5 = 30deg
# WVO = 6 = 60deg
# WVO = 7 = 90deg

## 2.1 Adjust the dates to get months and years
df_filt$Date.x <- as.Date(df_filt$Date.x) # make the date datetime

df_time <- df_filt %>% # add columns for the month, year, and month-year
  mutate(year = year(Date.x), 
         month = month(Date.x),
         month_yr = format(as.Date(Date.x), "%Y-%m"))

df_time$month_yr <- as.Date(paste(df_time$month_yr, "-01", sep="")) # this needs to be in yr-mn-day for some reason so I just paste on a -01

## 2.2 Aggregate the scores for each newspaper by month (or year)

df_month <- df_time %>%
  group_by(`Publication Title`, month_yr, reg_05_pred_class) %>%
  summarise(mag = n())

df_year <- df_time %>%
  group_by(`Publication Title`, year, reg_05_pred_class) %>%
  summarise(mag = n())

## 2.3 Add the degree direction 

df_month <- df_month %>%
  mutate(deg = case_when(reg_05_pred_class == 1 ~ 270,
                         reg_05_pred_class == 2 ~ 300, 
                         reg_05_pred_class == 3 ~ 330, 
                         reg_05_pred_class == 4 ~ 360, 
                         reg_05_pred_class == 5 ~ 30, 
                         reg_05_pred_class == 6 ~ 60, 
                         reg_05_pred_class == 7 ~ 90))

df_year <- df_year %>%
  mutate(deg = case_when(reg_05_pred_class == 1 ~ 270,
                         reg_05_pred_class == 2 ~ 300, 
                         reg_05_pred_class == 3 ~ 330, 
                         reg_05_pred_class == 4 ~ 360, 
                         reg_05_pred_class == 5 ~ 30, 
                         reg_05_pred_class == 6 ~ 60, 
                         reg_05_pred_class == 7 ~ 90))

## 2.4 Determine the x, y for each WVO vector based on the magnitude (mag) and degree (deg)

df_month <- df_month %>%
  mutate(y = mag * sin(deg*(pi/180)), 
         x = mag * cos(deg*(pi/180)))

df_year <- df_year %>%
  mutate(y = mag * sin(deg*(pi/180)), 
         x = mag * cos(deg*(pi/180)))

# 3.0 Save to a csv

write_csv(df_month, here::here(paste0("data/processed/test_wvo_vectors_months_",
                                      Sys.Date(), ".csv")))

write_csv(df_year, here::here(paste0("data/processed/test_wvo_vectors_years_",
                                      Sys.Date(), ".csv")))


