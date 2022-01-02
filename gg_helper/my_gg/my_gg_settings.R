
# a custom theme script customized to my preferences ::::::
# library(tidyverse)
my_gg <- function(feedplot) {
  return_me <- feedplot + theme_minimal() + 
    theme(legend.position = 'top', 
          legend.title = element_text(size = 10), 
          plot.title.position = 'plot', 
          plot.title = element_text(size = 12, 
                                    face = 'bold'), 
          plot.subtitle = element_text(size = 10)) + 
    guides(fill = guide_colorbar(title.position = 'left', 
                                 title.hjust = 0.5, 
                                 barwidth = unit(20, 'lines'), 
                                 barheight = unit(0.5, 'lines')), 
           color = guide_colorbar(title.position = 'left', 
                                  title.hjust = 0.5, 
                                  barwidth = unit(20, 'lines'), 
                                  barheight = unit(0.5, 'lines'))) + 
    coord_cartesian(clip = 'off')
  return(return_me)}


# tests ??????????????????????????????????????????????

# (plt1 <- mtcars %>% 
#   ggplot(aes(x = mpg, y = qsec, color = disp)) + 
#   geom_point() + 
#   labs(title = 'asdf', subtitle = 'qwerty', 
#        caption = 'hello'))
# 
# my_gg(plt1)
