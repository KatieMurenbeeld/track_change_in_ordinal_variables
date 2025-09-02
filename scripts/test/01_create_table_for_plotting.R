# ==============================================================================
# Create Tables for Plotting Time Series
# ==============================================================================

# Load Required Packages
library(tidyverse) # For loading and manipulating data tables
library(ggplot2) # For plotting and visualization

# STEP 1: Load the WVO vector table 
# ------------------------------------------------------------------------------
# 1.1 Load the wildlife value orientation vector coordinates from 00_load_data.R

df_months <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/test_tracking_values/data/processed/test_wvo_vectors_months_2025-08-25.csv")
df_years <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/test_tracking_values/data/processed/test_wvo_vectors_years_2025-08-25.csv")

# STEP 2: Prepare data for use in plotting
# ------------------------------------------------------------------------------
# 2.1 First with a time step of one year

df_vec_sum_yr <- df_years %>%
  group_by(`Publication Title`, year) %>% # change to month_yr or year 
  summarise(head_x = sum(x), 
            head_y = sum(y),
            polarity = length(unique(reg_05_pred_class)) * 30,
            wm_angle = (atan((abs(head_y) / head_x))*(180/pi)) + 270,
            wm_wvo = sum((reg_05_pred_class * mag)/sum(mag)),
            total_articles = sum(mag))

# Test plots
df_vec_sum_yr %>% 
  filter(`Publication Title` == "Great Falls Tribune") %>%
  ggplot(., aes(year, polarity)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()


ggplot(df_vec_sum_yr, aes(year, polarity, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

ggplot(df_vec_sum_yr, aes(year, wm_angle, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

ggplot(df_vec_sum_yr, aes(year, head_y, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

ggplot(df_vec_sum_yr, aes(year, wm_wvo, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

# 2.2: Next with a time step of one month

df_vec_sum_m <- df_months %>%
  group_by(`Publication Title`, month_yr) %>% # change to month_yr or year 
  summarise(head_x = sum(x), 
            head_y = sum(y),
            polarity = length(unique(reg_05_pred_class)) * 30,
            wm_angle = atan((abs(head_y) / head_x))*(-180/pi),
            wm_wvo = sum((reg_05_pred_class * mag)/sum(mag)),
            total_articles = sum(mag))

# create some test plots
df_vec_sum_m %>% 
  filter(`Publication Title` == "Great Falls Tribune") %>%
  ggplot(., aes(month_yr, polarity)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

df_vec_sum_m %>% 
  filter(`Publication Title` == "Great Falls Tribune") %>%
  ggplot(., aes(month_yr, wm_angle)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

df_vec_sum_m %>% 
  filter(`Publication Title` == "Great Falls Tribune") %>%
  ggplot(., aes(month_yr, wm_wvo)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()


ggplot(df_vec_sum_m, aes(month_yr, polarity, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

ggplot(df_vec_sum_m, aes(month_yr, wm_angle, color = `Publication Title`)) + 
  geom_point(aes(size = total_articles)) + 
  geom_line()

df_vec_sum_m %>% 
  #filter(`Publication Title` == "Great Falls Tribune") %>%
ggplot(., aes(month_yr, head_y)) + 
  geom_point(aes(size = total_articles), alpha = 0.6) + 
  geom_line(aes(color = `Publication Title`)) + 
  theme_bw() +
  ylim(-7,7) +
  labs(#title = "Great Falls Tribune",
       x = "Years", 
       y = "Distance from Neutral WVO") +
  facet_wrap(~ `Publication Title`, scales = "free_x")
  #annotate("text", y = 5, "Domination") 
  #annotate("text", y = -5, "Mutualism")

