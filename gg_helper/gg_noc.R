
# program a clean txt annotation in ggplot ----------------------------
# from tjmahr
library(tidyverse)

mtcars %>% ggplot(aes(x = wt, y = qsec)) + geom_point() + 
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::
  annotate("label", label = 'LABEL HERE', 
    x = Inf, y = -Inf, hjust = "inward", vjust = "inward",
    fill = alpha("white", 0.5), label.size = 0)  
  # ::::::::::::::::::::::::::::::::::::::::::::::::::::

