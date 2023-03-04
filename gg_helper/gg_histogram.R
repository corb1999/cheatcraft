
library(tidyverse)

# a standard histogram plot function ::::::::::::::::
fn_plt_hist1 <- function(arg_df, 
                         arg_dim, 
                         arg_cap, 
                         arg_floor) {
  
  df_fun <- arg_df |> 
    rename(vv1 = !!arg_dim) |> 
    mutate(vv2 = ifelse(vv1 > arg_cap, 
                        arg_cap, 
                        vv1), 
           vv2 = ifelse(vv2 < arg_floor, 
                        arg_floor, 
                        vv2))
  
  p1 <- df_fun |> 
    ggplot(aes(x = vv2)) + 
    geom_histogram(alpha = 0.5, 
                   bins = 30, 
                   color = 'white') + 
    geom_vline(aes(xintercept = median(vv1, 
                                       na.rm = TRUE)), 
               color = 'blue') + 
    geom_vline(aes(xintercept = mean(vv1, 
                                     na.rm = TRUE)), 
               color = 'red') + 
    geom_vline(aes(xintercept = quantile(vv1, 
                                         na.rm = TRUE, 
                                         probs = 0.25)), 
               color = 'black', 
               linetype = 2) + 
    geom_vline(aes(xintercept = quantile(vv1, 
                                         na.rm = TRUE, 
                                         probs = 0.75)), 
               color = 'black', 
               linetype = 2) + 
    geom_vline(aes(xintercept = quantile(vv1, 
                                         na.rm = TRUE, 
                                         probs = 0.05)), 
               color = 'black', 
               linetype = 2) + 
    geom_vline(aes(xintercept = quantile(vv1, 
                                         na.rm = TRUE, 
                                         probs = 0.95)), 
               color = 'black', 
               linetype = 2) + 
    geom_rug(alpha = 0.5) + 
    theme_minimal() + 
    labs(x = arg_dim, 
         y = '', 
         caption = paste0('*Value capped @ ', 
                          arg_cap, 
                          '; Value floor @ ', 
                          arg_floor))
  
  return(p1)
}
# tests ??????????????????????????
fn_plt_hist1(arg_df = mtcars, 
             arg_dim = 'mpg', 
             arg_cap = 50000, 
             arg_floor = 0)

df_test <- data.frame(variable1 = sample(LETTERS, 
                                         1000, 
                                         replace = TRUE), 
                      variable2 = rnorm(1000), 
                      variable3 = rgamma(1000, 
                                         5))
fn_plt_hist1(arg_df = df_test, 
             'variable3', 
             50000, 
             -50000)
