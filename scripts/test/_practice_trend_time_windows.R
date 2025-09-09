# ==============================================================================
# Test showing trends for different window sizes
# ==============================================================================

# Load Required Packages
library(tidyverse) # For loading and manipulating data tables
library(ggplot2) # For plotting and visualization
library(tidyquant) # For working with various moving averages
library(paletteer) # For nice color palettes
library(ggthemes) # For nicer looking plots
library(ggtext) # For adding annotations and captions with html
library(colorspace)
library(colorblindr)
library(gghighlight)

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

# 2.2 Then with a time step of one year

df_vec_sum_yr <- df_years %>%
  group_by(`Publication Title`, year) %>% # change to month_yr or year 
  summarise(head_x = sum(x), 
            head_y = sum(y),
            polarity = length(unique(reg_05_pred_class)) * 30,
            wm_angle = (atan((abs(head_y) / head_x))*(180/pi)) + 270,
            wm_wvo = sum((reg_05_pred_class * mag)/sum(mag)),
            total_articles = sum(mag))

# STEP 3: Using the yearly data, step through 1, 5, 10 year increments
# ------------------------------------------------------------------------------
# 3.1 Start looking at the Great Falls Tribune (25 years of data)

tbg_yr <- df_vec_sum_yr %>%
  filter(`Publication Title` == "The Billings Gazette")

gfb_yr <- df_vec_sum_yr %>%
  filter(`Publication Title` == "Great Falls Tribune")

## pseudocode ############### 
## Stepping through ten years 1989-1999, 1990-2000, 1991-2001, etc.

test_colors <- paletteer_c("ggthemes::Orange", 50)
blues <- paletteer_c("ggthemes::Classic Blue", 30)
greys <- paletteer_c("ggthemes::Classic Gray", 30)

## set up data frames with x, y, endx, and endy

gft_yr_df <- gfb_yr %>%
  mutate(x_10 = year, 
         xend_10 = year+10, 
         y_10 = wm_wvo, 
         yend_10 = lead(wm_wvo, n = 10)) %>%
  filter(xend_10 <= max(year))

## Plot the line between the start and end year 10 year gap
p10 <- ggplot(gft_yr_df, aes(x = x_10, y = y_10, xend = xend_10, yend = yend_10)) +
  geom_segment(size = 1.5, arrow = arrow(length = unit(0.3, "cm")), color = test_colors[5]) + # Optional: adjust size and add arrows
  gghighlight(x_10 %in% c(2004, 2007)) +
  labs(title = "Multiple Line Segments",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()
#This approach allows for the efficient plotting of numerous segments, with the ability to differentiate them by aesthetic mappings based on variables within the data frame.


p10 <- ggplot(gfb_yr, aes(alpha = 0.5)) +
  geom_point(aes(year, wm_wvo), color = "grey", alpha = 0.4) +
  geom_segment(aes(x = year[1], y = wm_wvo[1], 
               xend = year[9], yend = wm_wvo[9], alpha = 0.5),
               arrow = arrow(length=unit(0.15,"cm")),
               color = "#F49435FF", linewidth = 0.15
  )
print(p10)
for (i in 1:nrow(gfb_yr)) {
  p10 <- p10 + 
   # geom_point(aes(x = i, y = gfb_yr$wm_wvo[gfb_yr$year == i]), color = "red") + 
    #geom_point(aes(x = i + 10, y = gfb_yr$wm_wvo[gfb_yr$year == i + 10]), color = "blue") + 
    geom_segment(x = gfb_yr$year[i], y = gfb_yr$wm_wvo[i], 
                 xend = gfb_yr$year[i+10], yend = gfb_yr$wm_wvo[i+10],
                 arrow = arrow(length=unit(0.15,"cm")), 
                 color = "#F49435FF", linewidth = 0.15
    ) + 
    gghighlight(i %in% c(2004, 2007)) +
  if (i > nrow(gfb_yr) - 10) {
    break
  }
}
print(p10)

final_p10 <- p10 +
  geom_point(x = 2004, y = gfb_yr$wm_wvo[gfb_yr$year == 2004], color = "#585858FF") +
  geom_point(x = 2014, y = gfb_yr$wm_wvo[gfb_yr$year == 2014], color = "#585858FF") +
  geom_point(x = 2007, y = gfb_yr$wm_wvo[gfb_yr$year == 2007], color = "#585858FF") +
  geom_point(x = 2017, y = gfb_yr$wm_wvo[gfb_yr$year == 2017], color = "#585858FF") +
  geom_segment(x = 2004, y = gfb_yr$wm_wvo[gfb_yr$year == 2004], 
               xend = 2014, yend = gfb_yr$wm_wvo[gfb_yr$year == 2014],
               arrow = arrow(length=unit(0.15,"cm")), , linewidth = 0.5,
               color = "#225188FF") +
  geom_segment(x = 2007, y = gfb_yr$wm_wvo[gfb_yr$year == 2007], 
               xend = 2017, yend = gfb_yr$wm_wvo[gfb_yr$year == 2017],
               arrow = arrow(length=unit(0.15,"cm")), linewidth = 0.5,
               color = "#225188FF") +
  geom_hline(yintercept = 4, color = "grey", linetype = "dashed") + 
  annotate("text", x = 2027, y = 4.15, label = "Neutral") + 
  annotate("text", x = 2027, y = 6.1, label = "Mutualism") + 
  annotate("text", x = 2027, y = 1.65, label = "Domination") + 
  annotate("text", x = 2014, y = gfb_yr$wm_wvo[gfb_yr$year == 2014] - 0.25, 
           label = "2014") +
  annotate("text", x = 2002.7, y = gfb_yr$wm_wvo[gfb_yr$year == 2004], 
           label = "2004") +
  annotate("text", x = 2017, y = gfb_yr$wm_wvo[gfb_yr$year == 2017] + 0.25, 
           label = "2017") +
  annotate("text", x = 2007, y = gfb_yr$wm_wvo[gfb_yr$year == 2007] - 0.25, 
           label = "2007") +
  annotate("richtext", x = 2017, y = gfb_yr$wm_wvo[gfb_yr$year == 2017] + 1.25, 
           label = "<span style = 'font-size:10pt'> 2007 to 2017 shows a<br> shift towards <b>mutualism</b></span>", 
           fill = NA, label.color = NA) +
  annotate("richtext", x = 2014, y = gfb_yr$wm_wvo[gfb_yr$year == 2014] - 1, 
           label = "<span style = 'font-size:10pt'> 2004 to 2014 shows a<br> shift towards <b>domination</b></span>", 
           fill = NA, label.color = NA) +
  annotate("richtext", x = 1998, y = 6.75, 
           label = "<span style = 'font-size:14pt'>Why choice of time frame 
           matters</span><br>
       <span style = 'font-size:10pt'>the choice of the beginning and end data 
       points will<br> reflect different shifts in <b>W</b>ildlife <b>V</b>alue 
       <b>O</b>rientations</span>", 
           fill = NA, label.color = NA, hjust = 0) +
  ylim(1,7) +
  xlim(1998,2028) +
  labs(x = "<b>Year</b>", 
       y = "<b>Weighted Mean WVO</b>") + 
  theme_classic() +
  theme(title = element_markdown(vjust = -5),
        axis.line=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.x = element_markdown(size = 11),
        axis.title.x = element_markdown(), 
        legend.position = "none")

print(final_p10)
ggsave(here::here("outputs/test/gft_10yr_wvo_shift.png"), final_p10, 
       height = 6, width = 8, dpi = 300)

## Plot the line between the start and end year 5 year gap
p5 <- ggplot(gfb_yr) +
  geom_point(aes(year, wm_wvo)) +
  geom_segment(x = 1999, y = gfb_yr$wm_wvo[gfb_yr$year == 1999], 
               xend = 2004, yend = gfb_yr$wm_wvo[gfb_yr$year == 2004],
               arrow = arrow(length=unit(0.15,"cm"))
  )
for (i in 1:nrow(gfb_yr)) {
  p5 <- p5 + 
    # geom_point(aes(x = i, y = gfb_yr$wm_wvo[gfb_yr$year == i]), color = "red") + 
    #geom_point(aes(x = i + 10, y = gfb_yr$wm_wvo[gfb_yr$year == i + 10]), color = "blue") + 
    geom_segment(x = gfb_yr$year[i], y = gfb_yr$wm_wvo[i], 
                 xend = gfb_yr$year[i+5], yend = gfb_yr$wm_wvo[i+5],
                 arrow = arrow(length=unit(0.15,"cm")),
                 color = test_colors[i]
    )
  if (i > nrow(gfb_yr) - 5) {
    break
  }
}

final_p5 <- p5 + 
  geom_hline(yintercept = 4) + 
  ylim(1,7) +
  theme_bw()

print(final_p5)

## Plot the line between the start and end year 2 year gap
p2 <- ggplot(gfb_yr) +
  geom_point(aes(year, wm_wvo)) +
  geom_segment(x = 1999, y = gfb_yr$wm_wvo[gfb_yr$year == 1999], 
               xend = 2001, yend = gfb_yr$wm_wvo[gfb_yr$year == 2001],
               arrow = arrow(length=unit(0.15,"cm"))
  )
for (i in 1:nrow(gfb_yr)) {
  p2 <- p2 + 
    #geom_point(x = gfb_yr$year[i], y = gfb_yr$wm_wvo[i], color = "red") + 
    #geom_point(x = gfb_yr$year[i+2], y = gfb_yr$wm_wvo[i+2], color = "blue") + 
    geom_segment(x = gfb_yr$year[i], y = gfb_yr$wm_wvo[i], 
                 xend = gfb_yr$year[i+2], yend = gfb_yr$wm_wvo[i+2],
                 arrow = arrow(length=unit(0.15,"cm")), 
                 color = test_colors[i]
    )
  if (i > nrow(gfb_yr) - 2) {
    break
  }
}
print(p2)

final_p2 <- p2 + 
  geom_hline(yintercept = 4) + 
  ylim(1,7) +
  theme_bw()

print(final_p2)

