
# a custom theme script customized to my preferences ::::::
# library(tidyverse)
my_gg <- function(feedplot) {
  render_time <- Sys.time()
  return_me <- feedplot + theme_minimal() + 
    theme(legend.position = 'top', 
          legend.title = element_text(size = 10), 
          plot.title.position = 'plot', 
          plot.title = element_text(size = 12, 
                                    face = 'bold'), 
          plot.subtitle = element_text(size = 10), 
          plot.tag.position = 'bottom', 
          plot.tag = element_text(size = 6), 
          axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10), 
          plot.caption.position = 'plot', 
          plot.caption = element_text(size = 8, 
                                      face = 'italic'), 
          panel.grid.minor = element_blank()) + 
    guides(color = guide_legend(direction = 'horizontal')) + 
    coord_cartesian(clip = 'off') + 
    labs(tag = paste0('rendered @ ', render_time))
  return(return_me)}


# tests ??????????????????????????????????????????????

# (plt1 <- mtcars %>%
#   ggplot(aes(x = mpg, y = qsec, color = disp)) +
#   geom_point() +
#   labs(title = 'asdf', subtitle = 'qwerty',
#        caption = 'hello'))
# 
# my_gg(plt1)
# 
# (plt2 <- mtcars %>%
#     ggplot(aes(x = mpg, y = disp, color = as.factor(cyl))) +
#     geom_point() +
#     labs(title = 'asdf', subtitle = 'qwerty',
#          caption = 'hello'))
# 
# my_gg(plt2)
