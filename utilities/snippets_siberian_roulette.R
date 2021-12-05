
# a function to perform one-hot encoding on a categorical variable 
#   in a dataframe. siberian roulette because its all 1s and 0s, 
#   just a name nothing to it couldnt think of anything better

library(tidyverse)

# function to turn a categorical var in a df into one-hot
#   encoded variables
siberian_roulette <- function(df_func, encode_var) {
  df_onehot <- df_func %>% 
    rename(onehotvar = !!encode_var) %>% 
    mutate(onehotvar = ifelse(is.na(onehotvar), 
                              'naval', onehotvar), 
           onehotvar = ifelse(onehotvar == '', 
                              'blankval', onehotvar)) %>% 
    select(onehotvar)
  hott <- data.frame(the_labels = unique(df_onehot$onehotvar)) %>% 
    mutate(new_col_names = paste0('hot1_', encode_var, '_', 
                                  tolower(the_labels)), 
           new_col_names = str_remove_all(new_col_names, ' '))
  duplicator <- function(z) {df_onehot}
  df_onehot <- lapply(seq_along(1:nrow(hott)), duplicator)
  fun_onehot_test <- function(dfz, aa, bb) {
    dff <- dfz %>% mutate(interim = ifelse(onehotvar == aa, 1, 0))
    colnames(dff) <- c('onehotvar', bb)
    dff <- dff %>% select(-onehotvar)
    return(dff)}
  out1 <- pmap(list(df_onehot, hott$the_labels, hott$new_col_name), 
               fun_onehot_test)
  out1 <- reduce(out1, cbind) %>% as_tibble()
  return_me <- cbind(df_func, out1) %>% as_tibble()
  return(return_me)}


# tests ???????????????????????????????????????????

siberian_roulette(df_test, 'bravo')
asdf <- siberian_roulette(df_test, 'bravo')
View(asdf[1:100, ])

rows_for_test <- 1000
category_length <- min(80, 7)

df_test <- data.frame(alpha = c(1:rows_for_test), 
                      bravo = sample(fruit[1:category_length], 
                                     rows_for_test, 
                                     replace = TRUE), 
                      charlie = runif(rows_for_test, 1, 100), 
                      foxtrot = runif(rows_for_test), 
                      golf = rnorm(rows_for_test), 
                      juliet = sample(LETTERS, rows_for_test, 
                                      replace = TRUE), 
                      oscar = sample(c(fruit[1:category_length], 
                                       NA, ''), 
                                     rows_for_test, 
                                     replace = TRUE)) %>% 
  as_tibble()

head(df_test)
unique(df_test$bravo)
unique(df_test$juliet)
unique(df_test$oscar)
