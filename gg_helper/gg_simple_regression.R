
# simple linear regression line visuals -----------------------------------

library(tidyverse)
xx1 <- runif(100) * 10
dfa <- data.frame(xx = xx1, 
                  yy = rbinom(100, 100, 0.1) + xx1, 
                  zz = sample(c(letters), size = 100, replace = TRUE))
rm(xx1)

# function to take a dataframe, perform simple linear regression
#   and return a visual scatterplot annotated with regression equation
#   can supply a short vector of x values if you want to hone in on
#   the linear relationship with some examples (what happens to y 
#     if x increases by ?)
fun_scatter_regression <- function(dfz, pred_targets = c(NA)) {
  m1 <- lm(formula = yy ~ xx, data = dfz, weights = NULL)
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
  return_me <- dfz %>% 
    ggplot(aes(x = xx, y = yy)) + 
    geom_rug(alpha = 0.25) + 
    geom_point(alpha = 0.8) + 
    geom_abline(slope = m1$coefficients[[2]], 
                intercept = m1$coefficients[[1]], 
                size = 1, linetype = 2, color = "black") + 
    geom_point(data = pred_tar, 
               aes(x = pxx, y = pyy), na.rm = TRUE, 
               color = "red", shape = 4, size = 3) + 
    geom_text(data = pred_tar, 
              aes(x = pxx, y = pyy * 1.05, 
                  label = round(pyy, 4)), 
              na.rm = TRUE, size = 2.75, color = "red") + 
    theme_minimal() + theme(legend.position = "none") + 
    labs(subtitle = plt_sub, 
         caption = paste("Prediction x-values = ", p_caption))
  return(return_me)}

# tests ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fun_scatter_regression(dfa)
fun_scatter_regression(dfa, c(2, 5, 8))

# ^ -----


