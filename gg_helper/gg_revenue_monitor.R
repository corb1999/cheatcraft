
# revenue monitoring functions ------------------------------------------

# replace the function variable names with the purchase_ prefix !!!!!!!!!!
# require(tidyverse)
# require(scales)
# require(lubridate)


# trend line of monthly revenue
fun_rev_trend_yrmon <- function(df_func) {
  df_func_agg_time <- df_func %>% 
    group_by(purchase_yrmon) %>% 
    summarise(purchase_count_sum = sum(purchase_count, na.rm = TRUE), 
              purchase_value_sum = sum(purchase_value, na.rm = TRUE), 
              purchase_value_max = max(purchase_value, na.rm = TRUE), 
              purchase_value_med = median(purchase_value, na.rm = TRUE), 
              purchase_value_mean = mean(purchase_value, na.rm = TRUE)) %>% 
    mutate(purchase_yr = year(purchase_yrmon), 
           purchase_mon = month(purchase_yrmon))
  df_fun_agg_yr <- df_func_agg_time %>% 
    group_by(purchase_yr) %>% 
    summarise(purchase_value_sum_mean = mean(purchase_value_sum), 
              purchase_yrmon_min = min(purchase_yrmon), 
              purchase_yrmon_max = max(purchase_yrmon))
  plt1 <- df_func_agg_time %>% 
    ggplot(aes(x = purchase_yrmon, y = purchase_value_sum)) + 
    geom_ribbon(aes(ymax = purchase_value_sum, 
                    ymin = min(purchase_value_sum) * 0.98), alpha = 0.15, 
                fill = "#00A19D") + 
    geom_line(size = 1.25, color = "#00A19D") +
    geom_point(size = 3, color = "#00A19D") +
    geom_hline(aes(yintercept = mean(df_func_agg_time$purchase_value_sum)), 
               linetype = 2) + 
    geom_segment(data = df_fun_agg_yr, 
                 aes(x = purchase_yrmon_min, 
                     xend = purchase_yrmon_max, 
                     y = purchase_value_sum_mean, 
                     yend = purchase_value_sum_mean)) + 
    geom_label(data = df_func_agg_time %>% 
                 filter(purchase_yrmon == max(df_func$purchase_yrmon)), 
               aes(x = purchase_yrmon, y = purchase_value_sum * 1.15, 
                   label = purchase_value_sum), 
               alpha = 0, color = "#00A19D", size = 3) + 
    facet_wrap(vars(purchase_yr), nrow = 1, scales = "free_x") + 
    scale_y_continuous(labels = scales::dollar) + 
    theme_minimal() + coord_cartesian(clip = "off") + 
    labs(x = "", y = "Revenue Name")
  return_me <- plt1
  return(return_me)}

# view of revenue trends
fun_rev_trend_yoy <- function(df_func) {
  df_func_agg_time <- df_func %>% 
    group_by(purchase_yr, purchase_mon) %>% 
    summarise(purchase_count_sum = sum(purchase_count, na.rm = TRUE), 
              purchase_value_sum = sum(purchase_value, na.rm = TRUE), 
              purchase_value_max = max(purchase_value, na.rm = TRUE), 
              purchase_value_med = median(purchase_value, na.rm = TRUE), 
              purchase_value_mean = mean(purchase_value, na.rm = TRUE)) %>% 
    mutate(purchase_yr_cur = ifelse(purchase_yr == max(df_func$purchase_yr), 
                                    TRUE, FALSE))
  df_fun_agg_yr <- df_func_agg_time %>% 
    group_by(purchase_yr) %>% 
    summarise(purchase_value_sum_mean = mean(purchase_value_sum), 
              purchase_yrmon_min = min(purchase_mon), 
              purchase_yrmon_max = max(purchase_mon))
  plt1 <- df_func_agg_time %>% 
    ggplot(aes(x = purchase_mon, 
               y = purchase_value_sum, 
               color = as.factor(purchase_yr))) + 
    geom_line(size = 1.25) +
    geom_point(size = 3) + 
    geom_segment(data = df_fun_agg_yr, 
                 aes(x = purchase_yrmon_min, 
                     xend = purchase_yrmon_max, 
                     y = purchase_value_sum_mean, 
                     yend = purchase_value_sum_mean, 
                     color = as.factor(purchase_yr))) + 
    scale_y_continuous(labels = scales::dollar) + 
    scale_x_continuous(breaks = c(1:12)) + 
    theme_minimal() + theme(legend.position = "top") + 
    coord_cartesian(clip = "off") + 
    labs(x = "", y = "Revenue Name", color = "")
  return_me <- plt1
  return(return_me)}

# ^ -----

# tests ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fun_rev_trend_yoy(df_func = df)

fun_rev_trend_yrmon(df_func = df)


# run the below first to perform the tests
library(tidyverse)
library(lubridate)
df <- data.frame(purchase_yr = sample(c(2018:2021), 
                                      size = 1000, replace = TRUE), 
                 purchase_mon = sample(c(1:12), 
                                       size = 1000, replace = TRUE), 
                 purchase_id1 = sample(letters, 
                                       size = 1000, replace = TRUE), 
                 purchase_id2 = sample(letters, 
                                       size = 1000, replace = TRUE),
                 purchase_id3 = sample(letters, 
                                       size = 1000, replace = TRUE), 
                 purchase_id4 = sample(c(10000:99999), 
                                       size = 1000, replace = TRUE), 
                 purchase_count = rep(1, 1000), 
                 purchase_value = rchisq(1000, df = 0, 
                                         ncp = 2) * 100 + 5, 
                 purchase_profile = sample(c('a', 'a', 'b', 'c', 'c', 'c'), 
                                           size = 1000, replace = TRUE))
df <- df %>% as_tibble() %>% 
  mutate(purchase_id = paste0(purchase_id1, purchase_id2, 
                              purchase_id3, purchase_id4)) %>% 
  select(-c(purchase_id1:purchase_id4)) %>% 
  mutate(purchase_yrmon = ifelse(str_length(purchase_mon) == 1, 
                                 paste0(0, purchase_mon), 
                                 as.character(purchase_mon)), 
         purchase_yrmon = ymd(paste(purchase_yr, 
                                    purchase_yrmon, 
                                    "01", sep = "-")), 
         purchase_value = round(purchase_value * purchase_yr / 2021, 
                                digits = 0)) %>% 
  filter(purchase_yrmon < as.Date('2021-10-01'))
# View(df)
