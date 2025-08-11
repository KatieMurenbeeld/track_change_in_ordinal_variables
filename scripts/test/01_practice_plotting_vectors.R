# ==============================================================================
# Test Plotting Vectors
# ==============================================================================

# Load Required Packages
library(tidyverse) # For loading and manipulating data tables
library(ggplot2) # For plotting and visualization

# STEP 1: Load the WVO vector table 
# -------------------------------------------------------------
# 1. Load the wildlife value orientation vector coordinates from 00_load_data.R

df_plot <- df_month

# STEP 2: Plot two vectors with (0,0) as their origins
# -------------------------------------------------------------
# 2. Practice plotting two vectors

ggplot() +
  geom_point() +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[1], yend = df_plot$y[1]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[2], yend = df_plot$y[2]),
               arrow = arrow()
  ) +
  xlim(-1,1) + 
  ylim(-1,1) + 
  theme_minimal()

# STEP 3: Add the two vectors from Step 2 and plot 
# -------------------------------------------------------------
# 3.1 Vector addition

y_new <- df_plot$y[1] + df_plot$y[2]
x_new <- df_plot$x[1] + df_plot$x[2]

ggplot() +
  geom_point() +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[1], yend = df_plot$y[1]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[2], yend = df_plot$y[2]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = x_new, yend = y_new),
               arrow = arrow(), color = "red"
  ) +
  xlim(-1,1) + 
  ylim(-1,1) + 
  theme_minimal()

# STEP 4: The new vector from Step 3 is now the origin for the next month 
# -------------------------------------------------------------
# 4.1 I think I want the tail of the monthly vector to be at the head of the
# previous monthly vector


ggplot() +
  geom_point() +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[1], yend = df_plot$y[1]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[2], yend = df_plot$y[2]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = x_new, yend = y_new),
               arrow = arrow(), color = "red"
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[3], yend = df_plot$y[3]),
               arrow = arrow(), color = "blue"
  ) +
  geom_segment(aes(x = x_new, y = y_new, xend = df_plot$x[3] + x_new, yend = df_plot$y[3] + y_new),
               arrow = arrow(), color = "orange"
  ) +
  xlim(-3, 3) + 
  ylim(-3, 3) + 
  theme_minimal()

# 4.2 Get the vector sum for each month-year and each news paper

df_vec_sum <- df_plot %>%
  group_by(`Publication Title`, month_yr) %>%
  summarise(month_vec_x = sum(x), 
            month_vec_y = sum(y))

test <- df_vec_sum %>%
  filter(`Publication Title` == "Great Falls Tribune") %>%
  mutate(tail_x = 0, 
         tail_y = 0,
         head_x = month_vec_x,
         head_y = month_vec_y)

test$tail_x[2] <- test$head_x[1] 
test$tail_y[2] <- test$head_y[1] #comment out to have all vector tails start at zero

test$head_x[2] <- test$tail_x[2] + test$month_vec_x[2]
test$head_y[2] <- test$tail_y[2] + test$month_vec_y[2] #comment out to have all vector tails start at zero

test$tail_x[3:nrow(test)] <- test$month_vec_x[3:nrow(test)] + test$tail_x[2:nrow(test)] #comment out to have all vector tails start at zero

test_subset <- test[2:nrow(test),]

test_subset$tail_x[2:nrow(test_subset)] <- test_subset$month_vec_x[2:nrow(test_subset)] + test_subset$tail_x[1:nrow(test_subset)-1] #comment out to have all vector tails start at zero

for (i in 2:nrow(test_subset)){
  test_subset$tail_x[i] <- test_subset$head_x[i-1]
  test_subset$tail_y[i] <- test_subset$head_y[i-1] #comment out to have all vector tails start at zero
  test_subset$head_x[i] <- test_subset$tail_x[i] + test_subset$month_vec_x[i]
  test_subset$head_y[i] <- test_subset$tail_y[i] + test_subset$month_vec_y[i]
}

df_new <- rbind(test[1,], test_subset)

df_new <- df_new %>%
  mutate(end_x = month_vec_x + tail_x,
         end_y = month_vec_y + tail_y)


ggplot() +
  geom_point() +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[1], yend = df_plot$y[1]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[2], yend = df_plot$y[2]),
               arrow = arrow()
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[1] + df_plot$x[2], yend = df_plot$y[1] + df_plot$y[2]),
               arrow = arrow(), color = "red"
  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_plot$x[3], yend = df_plot$y[3]),
               arrow = arrow(), color = "blue", linetype = "dashed"
  ) +
  geom_segment(aes(x = df_plot$x[1] + df_plot$x[2], y = df_plot$y[1] + df_plot$y[2], 
                   xend = df_plot$x[3] + df_plot$x[1] + df_plot$x[2], 
                   yend = df_plot$y[3] + df_plot$y[1] + df_plot$y[2]),
               arrow = arrow(), color = "blue"
  ) +
#  geom_segment(aes(x = 0, y = 0, xend = df_vec_sum$month_vec_x[3], yend = df_vec_sum$month_vec_y[3]),
#               arrow = arrow(), color = "orange", linetype = "dashed"
#  ) +
  geom_segment(aes(x = df_plot$x[3] + df_plot$x[1] + df_plot$x[2], 
                   y = df_plot$y[3] + df_plot$y[1] + df_plot$y[2], 
                   xend = df_vec_sum$month_vec_x[3] + df_plot$x[3] + df_plot$x[2] + df_plot$x[1], 
                   yend = df_vec_sum$month_vec_y[3] + df_plot$y[3] + df_plot$y[2] + df_plot$y[1]),
               arrow = arrow(), color = "orange"
  ) +
#  geom_segment(aes(x = 0, y = 0, xend = df_vec_sum$month_vec_x[4], yend = df_vec_sum$month_vec_y[4]),
#               arrow = arrow(), color = "purple", linetype = "dashed"
#  ) +
  geom_segment(aes(x = df_vec_sum$month_vec_x[3] + df_plot$x[3] + df_plot$x[2] + df_plot$x[1], 
                   y = df_vec_sum$month_vec_y[3] + df_plot$y[3] + df_plot$y[2] + df_plot$y[1], 
                   xend = df_vec_sum$month_vec_x[4]+ df_vec_sum$month_vec_x[3] + df_plot$x[3] + df_plot$x[2] + df_plot$x[1], 
                   yend = df_vec_sum$month_vec_y[4] + df_vec_sum$month_vec_y[3] + df_plot$y[3] + df_plot$y[2] + df_plot$y[1]),
               arrow = arrow(), color = "purple"
  ) +
#  geom_segment(aes(x = 0, y = 0, xend = df_vec_sum$month_vec_x[5]), yend = df_vec_sum$month_vec_y[5],
#               arrow = arrow(), color = "forestgreen", linetype = "dashed"
#  ) +
  geom_segment(aes(x = 0, y = 0, xend = df_vec_sum$month_vec_x[5]), yend = df_vec_sum$month_vec_y[5],
               arrow = arrow(), color = "forestgreen", linetype = "dashed"
  ) +
  geom_segment(aes(x = df_vec_sum$month_vec_x[4]+ df_vec_sum$month_vec_x[3] + df_plot$x[3] + df_plot$x[2] + df_plot$x[1], 
                   y = df_vec_sum$month_vec_y[4] + df_vec_sum$month_vec_y[3] + df_plot$y[3] + df_plot$y[2] + df_plot$y[1], 
                   xend = df_vec_sum$month_vec_x[5] + df_vec_sum$month_vec_x[4]+ df_vec_sum$month_vec_x[3] + df_plot$x[3] + df_plot$x[2] + df_plot$x[1], 
                   yend = df_vec_sum$month_vec_y[5] + df_vec_sum$month_vec_y[4] + df_vec_sum$month_vec_y[3] + df_plot$y[3] + df_plot$y[2] + df_plot$y[1]),
               arrow = arrow(), color = "forestgreen"
  ) +
  xlim(-8, 8) + 
  ylim(-8, 8) + 
  theme_minimal()

ggplot(df_new, aes(head_x, head_y)) + 
  geom_line()

ggplot(df_vec_sum[1:4,], aes(month_vec_x, month_vec_y, color = month_yr)) +
  geom_point()



set.seed(111)
df1 <- data.frame(WD=sample(10))
df1$newColumn <- with(df1, c(WD[1],WD[-1]+WD[-nrow(df1)]))
