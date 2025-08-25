# ==============================================================================
# Functions for Plotting WVO Vectors
# ==============================================================================

# Function 1: Plot vectors through time with the tails of vt at the head of vt-1 
# -------------------------------------------------------------

vector_tails_to_head <- function(data, time_step, publication){
  df_vec_sum <- data %>%
    group_by(`Publication Title`, {{time_step}}) %>%
    summarise(month_vec_x = sum(x), 
              month_vec_y = sum(y))
  
  df_tmp <- df_vec_sum %>%
    filter(`Publication Title` == publication) %>%
    mutate(tail_x = 0, 
           tail_y = 0,
           head_x = month_vec_x,
           head_y = month_vec_y)

   df_tmp$tail_x[2] <- df_tmp$head_x[1] 
   df_tmp$tail_y[2] <- df_tmp$head_y[1]
   #return(df_tmp)
   
   df_tmp$head_x[2] <- df_tmp$tail_x[2] + df_tmp$month_vec_x[2]
   df_tmp$head_y[2] <- df_tmp$tail_y[2] + df_tmp$month_vec_y[2] 
   #return(df_tmp)
 
   df_tmp$tail_x[3:nrow(df_tmp)] <- df_tmp$month_vec_x[3:nrow(df_tmp)] + df_tmp$tail_x[2:nrow(df_tmp)]
   return(df_tmp)

   df_subset <- df_tmp[2:nrow(df_tmp),]
   return(df_subset)


   df_subset$tail_x[2:nrow(df_subset)] <- df_subset$month_vec_x[2:nrow(df_subset)] + df_subset$tail_x[1:nrow(df_subset)-1]
   return(df_subset)

   for (i in 2:nrow(df_subset)){
     df_subset$tail_x[i] <- df_subset$head_x[i-1]
     df_subset$tail_y[i] <- df_subset$head_y[i-1]
     df_subset$head_x[i] <- df_subset$tail_x[i] + df_subset$month_vec_x[i]
     df_subset$head_y[i] <- df_subset$tail_y[i] + df_subset$month_vec_y[i]
     }
   return(df_subset)
   
   df_new <- rbind(df_tmp[1,], df_subset)
   return(df_new)
   
   df_new <- df_new %>%
     mutate(end_x = month_vec_x + tail_x,
            end_y = month_vec_y + tail_y)
   return(df_new)
}


# Function 2: Plot vectors through time with all vector tails starting from y = 0 
# -------------------------------------------------------------


vector_tails_to_zero <- function(data, time_step, publication){
  df_plot <- data
  
  df_vec_sum <- df_plot %>%
    group_by(`Publication Title`, {{time_step}}) %>%
    summarise(month_vec_x = sum(x), 
              month_vec_y = sum(y))
  
  df_tmp <- df_vec_sum %>%
    filter(`Publication Title` == publication) %>%
    mutate(tail_x = 0, 
           tail_y = 0,
           head_x = month_vec_x,
           head_y = month_vec_y)
  
  df_tmp$tail_x[2] <- df_tmp$head_x[1] 
  return(df_tmp)
  df_tmp$head_x[2] <- df_tmp$tail_x[2] + df_tmp$month_vec_x[2]
  return(df_tmp)
  
  df_subset <- df_tmp[2:nrow(df_tmp),]
  
  for (i in 2:nrow(df_subset)){
    df_subset$tail_x[i] <- df_subset$head_x[i-1]
    df_subset$head_x[i] <- df_subset$tail_x[i] + df_subset$month_vec_x[i]
    df_subset$head_y[i] <- df_subset$tail_y[i] + df_subset$month_vec_y[i]
  }
  return(df_subset) 
  
  df_new <- rbind(test[1,], df_subset)
  
  df_new <- df_new %>%
    mutate(end_x = month_vec_x + tail_x,
           end_y = month_vec_y + tail_y)
  
  
}
