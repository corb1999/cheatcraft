
# simple linear regression line visuals -----------------------------------

library(tidyverse)
library(rlang)
library(patchwork)
xx1 <- runif(100) * 10
dfa <- data.frame(xx_a = xx1, 
                  yy_a = rbinom(100, 100, 0.1) + xx1, 
                  ww_a = runif(100) + xx1 / 5, 
                  zz = sample(c(letters), size = 100, replace = TRUE))
rm(xx1)

# function to take a dataframe, perform simple linear regression
#   and return a visual scatterplot annotated with regression equation
#   can supply a short vector of x values if you want to hone in on
#   the linear relationship with some examples (what happens to y 
#     if x increases by ?)
fun_scatter_regression <- function(dfz, xvar, yvar, weight_var = NA, 
                                   pred_targets = c(NA)) {
  if (is.na(weight_var)) {
    wvar <- NULL
  } else {
    dfz <- dfz %>% rename(wvar = !!weight_var)
  }
  dfz <- dfz %>% rename(xx = !!xvar, yy = !!yvar)
  m1 <- lm(formula = yy ~ xx, data = dfz, weights = wvar)
  m1_summary <- summary(m1)
  if (any(is.na(pred_targets))) {
    pred_tar <- data.frame(pxx = NA, pyy = NA)
    pred_tar <- pred_tar %>% filter(!is.na(pxx))
  } else {
    interim <- predict(m1, data.frame(xx = pred_targets))
    pred_tar <- data.frame(pxx = pred_targets,
                           pyy = interim)
    rm(interim)
  }
  plt_sub <- paste0("n = ", nrow(dfz), "; adjRsqrd = ",
                    round(m1_summary$adj.r.squared, 4),
                    "; y = ", round(m1$coefficients[[2]], 3), "x + ",
                    round(m1$coefficients[[1]], 3))
  p_caption <- paste(pred_targets, collapse = "; ")
  plt1 <- dfz %>% ggplot(aes(x = xx, y = yy)) + 
    geom_vline(aes(xintercept = median(xx)), 
               color = '#334756', linetype = 2) + 
    geom_hline(aes(yintercept = median(yy)), 
               color = '#334756', linetype = 2) + 
    geom_point(aes(x = median(xx), y = median(yy)), 
               color = '#334756', size = 2.5, shape = 1) + 
    geom_rug(alpha = 0.25) + geom_point(alpha = 0.85) +
    geom_abline(slope = m1$coefficients[[2]],
                intercept = m1$coefficients[[1]],
                size = 1, linetype = 2, color = "blue") +
    geom_point(data = pred_tar,
               aes(x = pxx, y = pyy), na.rm = TRUE,
               color = "red", shape = 4, size = 3) +
    geom_text(data = pred_tar,
              aes(x = pxx, y = pyy * 1.05,
                  label = round(pyy, 4)),
              na.rm = TRUE, size = 2.75, color = "red") +
    theme_minimal() + theme(legend.position = "none") +
    labs(subtitle = plt_sub, x = xvar, y = yvar, 
         caption = paste("Prediction x-values = ", p_caption, "\n", 
                         "Weights = ", weight_var))
  plt1x <- dfz %>% ggplot(aes(x = xx)) + 
    geom_histogram(bins = 30, color = 'white', alpha = 0.5) + 
    geom_vline(aes(xintercept = median(xx))) + 
    theme_minimal() + labs(x = xvar, y = '')
  plt1y <- dfz %>% ggplot(aes(x = yy)) + 
    geom_histogram(bins = 30, color = 'white', alpha = 0.5) + 
    geom_vline(aes(xintercept = median(yy))) + 
    theme_minimal() + labs(x = yvar, y = '')
  rr_std <- (residuals(m1) - mean(residuals(m1))) / 
    sd(residuals(m1))
  plt2 <- dfz %>% ggplot(aes(x = rr_std)) + 
    geom_histogram(bins = 30, color = 'white', 
                   fill = 'blue', alpha = 0.5) + 
    geom_vline(aes(xintercept = 0)) + theme_minimal() + 
    labs(x = 'Standardized Residuals', y = 'Freq')
  plt3 <- ggplot() + 
    geom_point(data = dfz, aes(x = xx, y = rr_std, 
                               color = rr_std)) + 
    geom_hline(aes(yintercept = 0)) + 
    scale_color_distiller(palette = 'RdYlBu') + 
    theme_minimal() + theme(legend.position = 'none') + 
    labs(x = xvar, y = 'Standardized Residuals')
  return_me <- plt1 + (plt1x / plt1y) + (plt2 / plt3)
  return(return_me)}

# tests ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fun_scatter_regression(dfa, xvar = 'xx_a', yvar = 'yy_a')
fun_scatter_regression(dfa, xvar = 'xx_a', yvar = 'yy_a', 
                       pred_targets = c(2, 5, 8))
fun_scatter_regression(dfa, xvar = 'xx_a', yvar = 'yy_a', 
                       weight_var = 'ww_a')

fun_scatter_regression(longley, 'Employed', 'GNP')
fun_scatter_regression(swiss, 'Agriculture', 'Fertility')
fun_scatter_regression(trees, 'Girth', 'Height')
fun_scatter_regression(women, 'height', 'weight')

fun_scatter_regression(iris, 'Sepal.Length', 'Sepal.Width')
fun_scatter_regression(USJudgeRatings, 'CONT', 'INTG')
fun_scatter_regression(airquality, 'Temp', 'Wind')
fun_scatter_regression(faithful, 'waiting', 'eruptions')

# ^ -----


