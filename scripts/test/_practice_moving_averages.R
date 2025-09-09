# ==============================================================================
# Test working with moving averages
# ==============================================================================

# Load Required Packages
library(tidyverse) # For loading and manipulating data tables
library(ggplot2) # For plotting and visualization
library(tidyquant) # For working with various moving averages

# STEP 1: Load the WVO vector table 
# ------------------------------------------------------------------------------
# 1.1 Load the wildlife value orientation vector coordinates from 00_load_data.R

df_months <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/test_tracking_values/data/processed/test_wvo_vectors_months_2025-08-25.csv")
df_years <- read_csv("/Users/katiemurenbeeld/Analysis/VIP_NLP/test_tracking_values/data/processed/test_wvo_vectors_years_2025-08-25.csv")

# STEP 2: Prepare data for use in plotting
# ------------------------------------------------------------------------------
# 2.1 First with a time step of one month

df_vec_sum_m <- df_months %>%
  group_by(`Publication Title`, month_yr) %>% # change to month_yr or year 
  summarise(head_x = sum(x), 
            head_y = sum(y),
            polarity = length(unique(reg_05_pred_class)) * 30,
            wm_angle = atan((abs(head_y) / head_x))*(-180/pi),
            wm_wvo = sum((reg_05_pred_class * mag)/sum(mag)),
            total_articles = sum(mag))

# STEP 3: Using tidyquant test a simple moving average
# ------------------------------------------------------------------------------

df_vec_sum_m %>% 
  filter(`Publication Title` == "Great Falls Tribune") %>%
  ggplot(., aes(month_yr, head_y)) + 
  geom_point(aes(size = total_articles), alpha = 0.6) + 
  geom_line(color = "purple4") + 
  theme_bw() +
  ylim(-7,7) +
  labs(title = "Great Falls Tribune",
    x = "Years", 
    y = "Distance from Neutral WVO")

df_vec_sum_m %>% 
  filter(`Publication Title` == "The Billings Gazette") %>%
  ggplot(., aes(month_yr, head_y)) + 
  geom_point(aes(size = total_articles), alpha = 0.6) + 
  geom_smooth(color = "purple4", alpha = 0.4) + 
  #geom_ma(ma_fun = SMA, n = 3, color = "black") +
  #geom_ma(ma_fun = SMA, n = 6, color = "red") +
  #geom_ma(ma_fun = SMA, n = 12, color = "green") +
  theme_bw() +
  ylim(-7,7) +
  labs(title = "The Billings Gazette",
       x = "Years", 
       y = "Distance from Neutral WVO")

# STEP 3: Try out the auto-regressive conditional heteroskedasticity (ARCH(1)) 
# model
# ------------------------------------------------------------------------------
# Code and explanation here: https://rpubs.com/cyobero/arch

library(dynlm)

test <- df_vec_sum_m %>%
  filter(`Publication Title` == "New York Times") %>%
  dplyr::select(c(wm_wvo, month_yr))

# Step 1: Estimate mean equation r = beta + error (here r = head_y)
test.mean <- dynlm(wm_wvo ~ 1, data = test)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(test.mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
test.arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)

summary(test.arch)

#library(FinTS)
test.archTest <- ArchTest(test$wm_wvo, lags = 1, demean = TRUE)
test.archTest

#library(fGarch)
arch.fit <- garchFit(~garch(1,0), data = test$wm_wvo, trace = F)
summary(arch.fit)


test$ht <- arch.fit@h.t
ggplot(test, aes(y = ht, x = month_yr)) + 
  geom_line(col = 'blue4') + 
  ylim(0, 15) +
  labs(title = "New York Times",
       x = "Date", 
       y = "Conditional Variance")

# None of the papers reject the null hypothesis of "no Arch effects"

# This may be a better option?
# https://www.sourceallies.com/2020/02/tesla-stock-volatility-in-r/


