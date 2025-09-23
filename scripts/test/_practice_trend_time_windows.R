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

nyt_yr <- df_vec_sum_yr %>%
  filter(`Publication Title` == "New York Times")

## pseudocode ############### 
## Stepping through ten years 1989-1999, 1990-2000, 1991-2001, etc.

test_colors <- paletteer_c("ggthemes::Orange", 50)
blues <- paletteer_c("ggthemes::Classic Blue", 30)
greys <- paletteer_c("ggthemes::Classic Gray", 30)

## set up data frames with x, y, endx, and endy

gft_10yr_df <- gfb_yr %>%
  mutate(x_10 = year, 
         xend_10 = year+10, 
         y_10 = wm_wvo, 
         yend_10 = lead(wm_wvo, n = 10)) %>%
  filter(xend_10 <= max(year))

nyt_10yr_df <- nyt_yr %>%
  mutate(x_10 = year, 
         xend_10 = year+10, 
         y_10 = wm_wvo, 
         yend_10 = lead(wm_wvo, n = 10)) %>%
  filter(xend_10 <= max(year))

## Plot the line between the start and end year 10 year gap
p10 <- ggplot(nyt_10yr_df, aes(x = x_10, y = y_10, xend = xend_10, yend = yend_10)) +
  geom_segment(size = 1, arrow = arrow(length = unit(0.15, "cm")), color = blues[30]) + # Optional: adjust size and add arrows
  gghighlight(x_10 %in% c(2004, 2007), unhighlighted_params = list(color = NULL, linewidth = 0.5))
#the above code is adapted from the google AI result for "plot multiple geom_segments r"
#This approach allows for the efficient plotting of numerous segments, with the ability to differentiate them by aesthetic mappings based on variables within the data frame.
print(p10)

final_p10 <- p10 +
  geom_point(x = 2004, y = gft_10yr_df$y_10[gft_10yr_df$x_10 == 2004], color = "#585858FF") +
  geom_point(x = 2014, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2014], color = "#585858FF") +
  geom_point(x = 2007, y = gft_10yr_df$y_10[gft_10yr_df$x_10 == 2007], color = "#585858FF") +
  geom_point(x = 2017, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2017], color = "#585858FF") +
  geom_hline(yintercept = 4, color = "grey", linetype = "dashed") + 
  annotate("text", x = 2027, y = 4.15, label = "Neutral") + 
  annotate("text", x = 2027, y = 6.1, label = "Mutualism") + 
  annotate("text", x = 2027, y = 1.65, label = "Domination") + 
  annotate("text", x = 2014, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2014] - 0.25, 
           label = "2014") +
  annotate("text", x = 2002.7, y = gft_10yr_df$y_10[gft_10yr_df$x_10 == 2004], 
           label = "2004") +
  annotate("text", x = 2017, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2017] + 0.25, 
           label = "2017") +
  annotate("text", x = 2007, y = gft_10yr_df$y_10[gft_10yr_df$x_10 == 2007] - 0.25, 
           label = "2007") +
  annotate("richtext", x = 2017, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2017] + 1.25, 
           label = "<span style = 'font-size:10pt'> 2007 to 2017 shows a<br> 
           shift towards <span style = 'color:#26456EFF'><b>mutualism</b></span>
           </span>", 
           fill = NA, label.color = NA) +
  annotate("richtext", x = 2014, y = gft_10yr_df$yend_10[gft_10yr_df$xend_10 == 2014] - 1, 
           label = "<span style = 'font-size:10pt'> 2004 to 2014 shows a<br> 
           shift towards <span style = 'color:#26456EFF'><b>domination</b></span>
           </span>", 
           fill = NA, label.color = NA) +
  annotate("richtext", x = 1998, y = 6.75, 
           label = "<span style = 'font-size:14pt'>Why choice of time frame 
           matters</span><br>
       <span style = 'font-size:10pt'>the beginning and end data 
       points will<br> reflect different shifts in <b>W</b>ildlife <b>V</b>alue 
       <b>O</b>rientations</span>", 
           fill = NA, label.color = NA, hjust = 0) +
  ylim(1,7) +
  xlim(1998,2028) +
  labs(x = "<b>Year</b>", 
       y = "<b>Weighted Mean WVO</b><br>
        <span style = 'font-size:10pt'>from the Great Falls Tribune</span>", 
       caption = "Many combinations of<span style = 'color:#26456EFF'>
       <b>highlighed</b></span><br> segments would show contradictory WVO
       shifts.</span>") + 
  theme_classic() +
  theme(title = element_markdown(vjust = -5),
        axis.line=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.x = element_markdown(size = 11),
        axis.title.x = element_markdown(), 
        legend.position = "none", 
        plot.caption = element_markdown(lineheight = 1.2, hjust = 0))

print(final_p10)
ggsave(here::here("outputs/test/gft_10yr_wvo_shift_blues.png"), final_p10, 
       height = 6, width = 8, dpi = 300)

## Plot the line between the start and end year 5 year gap
## set up data frames with x, y, endx, and endy

gft_5yr_df <- gfb_yr %>%
  mutate(x_5 = year, 
         xend_5 = year+5, 
         y_5 = wm_wvo, 
         yend_5 = lead(wm_wvo, n = 5)) %>%
  filter(xend_5 <= max(year))

p5 <- ggplot(gft_5yr_df, aes(x = x_5, y = y_5, xend = xend_5, yend = yend_5)) +
  geom_segment(size = 1, arrow = arrow(length = unit(0.15, "cm")), color = blues[30]) + # Optional: adjust size and add arrows
  gghighlight(x_5 %in% c(2004:2009), unhighlighted_params = list(color = NULL, linewidth = 0.5))
p5

final_p5 <- p5 +
  geom_point(x = 2004, y = gft_5yr_df$y_5[gft_5yr_df$x_5 == 2004], color = "#585858FF") +
  geom_point(x = 2014, y = gft_5yr_df$yend_5[gft_5yr_df$xend_5 == 2014], color = "#585858FF") +
  geom_segment(x = gft_5yr_df$x_5[gft_5yr_df$x_5 == 2004], y = gft_5yr_df$y_5[gft_5yr_df$x_5 == 2004], 
               xend = gft_5yr_df$xend_5[gft_5yr_df$xend_5 == 2014], yend = gft_5yr_df$yend_5[gft_5yr_df$xend_5 == 2014], 
               color = "black", linetype = "dashed") +
  geom_hline(yintercept = 4, color = "grey", linetype = "dashed") + 
  annotate("text", x = 2027, y = 4.15, label = "Neutral") + 
  annotate("text", x = 2027, y = 6.1, label = "Mutualism") + 
  annotate("text", x = 2027, y = 1.65, label = "Domination") + 
  annotate("text", x = 2014, y = gft_5yr_df$yend_5[gft_5yr_df$xend_5 == 2014] - 0.25, 
           label = "2014") +
  annotate("text", x = 2004, y = gft_5yr_df$yend_5[gft_5yr_df$xend_5 == 2014] - 0.25, 
           label = "2004") +
  annotate("richtext", x = 1998, y = 6.75, 
           label = "<span style = 'font-size:14pt'>Why length of time frame 
           matters:</span><br>
       <span style = 'font-size:10pt'>A 10-year time span misses  
       finer scale shifts in <b>W</b>ildlife <b>V</b>alue 
       <b>O</b>rientations</span>", 
           fill = NA, label.color = NA, hjust = 0) +
  ylim(1,7) +
  xlim(1998,2028) +
  labs(x = "<b>Year</b>", 
       y = "<b>Weighted Mean WVO</b><br>
        <span style = 'font-size:10pt'>from the Great Falls Tribune</span>", 
       caption = "Each segment represents<span style = 'color:#26456EFF'>
       <b>5</b></span> years.</span>") + 
  theme_classic() +
  theme(title = element_markdown(vjust = -5),
        axis.line=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.x = element_markdown(size = 11),
        axis.title.x = element_markdown(), 
        legend.position = "none", 
        plot.caption = element_markdown(lineheight = 1.2, hjust = 0))
final_p5
ggsave(here::here("outputs/test/gft_5yr_wvo_shift_blues.png"), final_p5, 
       height = 6, width = 8, dpi = 300)

## Plot the line between the start and end year 2 year gap
gft_2yr_df <- gfb_yr %>%
  mutate(x_2 = year, 
         xend_2 = year+2, 
         y_2 = wm_wvo, 
         yend_2 = lead(wm_wvo, n = 2)) %>%
  filter(xend_2 <= max(year))


p2 <- ggplot(gft_2yr_df, aes(x = x_2, y = y_2, xend = xend_2, yend = yend_2)) +
  geom_segment(size = 1, arrow = arrow(length = unit(0.15, "cm")), color = blues[30]) + # Optional: adjust size and add arrows
  gghighlight(x_2 %in% c(2004:2012))
p2

final_p2 <- p2 +
  geom_point(x = 2004, y = gft_2yr_df$y_2[gft_2yr_df$x_2 == 2004], color = "#585858FF") +
  geom_point(x = 2014, y = gft_2yr_df$yend_2[gft_2yr_df$xend_2 == 2014], color = "#585858FF") +
  geom_hline(yintercept = 4, color = "grey", linetype = "dashed") + 
  annotate("text", x = 2027, y = 4.15, label = "Neutral") + 
  annotate("text", x = 2027, y = 6.1, label = "Mutualism") + 
  annotate("text", x = 2027, y = 1.65, label = "Domination") + 
  annotate("text", x = 2014, y = gft_2yr_df$yend_2[gft_2yr_df$xend_2 == 2014] - 0.25, 
           label = "2014") +
  annotate("text", x = 2004, y = gft_2yr_df$yend_2[gft_2yr_df$xend_2 == 2014] - 0.25, 
           label = "2004") +
  annotate("richtext", x = 1998, y = 6.75, 
           label = "<span style = 'font-size:14pt'>Why length of time frame 
           matters:</span><br>
       <span style = 'font-size:10pt'>A 10-year time span misses  
       finer scale shifts in <b>W</b>ildlife <b>V</b>alue 
       <b>O</b>rientations</span>", 
           fill = NA, label.color = NA, hjust = 0) +
  ylim(1,7) +
  xlim(1998,2028) +
  labs(x = "<b>Year</b>", 
       y = "<b>Weighted Mean WVO</b><br>
        <span style = 'font-size:10pt'>from the Great Falls Tribune</span>",
       caption = "Each segment represents<span style = 'color:#26456EFF'>
       <b>2</b></span> years.</span>") + 
  theme_classic() +
  theme(title = element_markdown(vjust = -5),
        axis.line=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.x = element_markdown(size = 11),
        axis.title.x = element_markdown(), 
        legend.position = "none", 
        plot.caption = element_markdown(lineheight = 1.2, hjust = 0))
final_p2
ggsave(here::here("outputs/test/gft_2yr_wvo_shift_blues.png"), final_p2, 
       height = 6, width = 8, dpi = 300)

