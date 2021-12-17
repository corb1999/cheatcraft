
library(tidyverse)

# a function to generate a list of bootstrap samples from a vector
#   which can then be used in map-reduce functions from purrr

bootstrapper <- function(x, n_bootstraps = 1, sample_size = 1){
  # make a fun to duplicate the vector multiple times into a list
  duplicator <- function(z) {x}
  # use lapply to duplicate the vector
  aa <- lapply(seq_along(1:n_bootstraps), duplicator)
  # make a function to draw samples with replacement
  sampler <- function(z) {
    sample(z, size = sample_size, replace = TRUE)}
  # use lapply to take samples from the copies of the vector
  bb <- lapply(aa, sampler)
  return(bb)}

# df version of bootstrapper (warning may not be fast and makes a large object)
bootstrapper_df <- function(x, n_bootstraps = 1, sample_size = 1){
  # make a fun to duplicate the df multiple times into a list
  duplicator <- function(z) {x}
  # use lapply to duplicate the dataframe
  aa <- lapply(seq_along(1:n_bootstraps), duplicator)
  # make a function to draw samples with replacement
  sampler <- function(z) {
    sample_n(z, size = sample_size, replace = TRUE)}
  # use lapply to take samples from the copies of the df
  bb <- lapply(aa, sampler)
  return(bb)}

# examples/tests -----------------------------
me <- runif(20)
you <- bootstrapper(me, 10, 5)
us <- lapply(you, sum) # or...
require(purrr)
us <- map_dbl(you, mean) # or...
us <- map_dbl(bootstrapper(me, 10, 5), mean)

me <- data.frame(var1 = letters, var2 = LETTERS, 
                 var3 = 1:26, var4 = runif(26), var5 = rbinom(26, 1, 0.5))
bootstrapper_df(me, 5, 5)
