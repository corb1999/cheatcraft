
library(tidyverse)

# plotting function to have half violin and boxplot :::::::
fn_plt_halfviolin <- function(arg_df, 
                              arg_category, 
                              arg_numerical, 
                              arg_offset = 0.025) {
  
  df_agg <- arg_df |> 
    rename(vcategory = !!arg_category, 
           vnumerical = !!arg_numerical) |> 
    mutate(vcategory = as.factor(vcategory)) |> 
    group_by(vcategory) |> 
    summarise(recs = n(), 
              val_min = min(vnumerical, 
                            na.rm = TRUE), 
              val_max = max(vnumerical, 
                            na.rm = TRUE))
  
  pp1 <- arg_df |> 
    rename(vcategory = !!arg_category, 
           vnumerical = !!arg_numerical) |> 
    ggplot(aes(x = as.factor(vcategory), 
               y = vnumerical, 
               color = as.factor(vcategory), 
               fill = as.factor(vcategory))) + 
    # geom_boxplot(alpha = 0.7) + 
    # geom_violin(alpha = 0.7) + 
    theme_minimal() + 
    theme(legend.position = 'none') + 
    labs(x = arg_category, 
         y = arg_numerical)
  
  # nudging components, from @teunbrand ::::::::::::
  # right_nudge <- aes(
  #   xmin = after_scale(x),
  #   x = stage(class, after_stat = x + offset)
  # )
  # left_nudge  <- aes(
  #   xmax = after_scale(x),
  #   x = stage(class, after_stat = x - offset)
  # )
  
  pp2 <- pp1 + 
    geom_violin(aes(x = stage(as.factor(vcategory), 
                              after_stat = x - arg_offset), 
                    xmax = after_scale(x)), 
                alpha = 0.4) + 
    geom_boxplot(aes(x = stage(as.factor(vcategory), 
                              after_stat = x + arg_offset), 
                     xmin = after_scale(x)), 
                 alpha = 0.4, 
                 outlier.shape = NA) + 
    geom_point(aes(x = stage(as.factor(vcategory), 
                              after_stat = x + arg_offset), 
                    xmin = after_scale(x)), 
                alpha = 0.15) + 
    geom_text(data = df_agg, 
              aes(x = vcategory, 
                  y = max(val_max) * 1.05, 
                  label = recs), 
              size = 3.5)
  
  return(pp2)
}

# tests ??????????????????????????????????
fn_plt_halfviolin(arg_df = airquality, 
                  arg_category = 'Month', 
                  arg_numerical = 'Wind')
fn_plt_halfviolin(mtcars, 
                  'cyl', 
                  'mpg')
fn_plt_halfviolin(iris, 
                  'Species', 
                  'Sepal.Length')

