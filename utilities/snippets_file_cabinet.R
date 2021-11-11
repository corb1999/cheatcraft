
# function to pull a dataframe of files in a working directory

library(tidyverse)


file_cabinet <- function(search_path = getwd()) {
  aa <- data.frame(search_path = search_path, 
                   dir_object = list.files(path = search_path, 
                                           all.files = TRUE))
  bb <- aa %>% 
    mutate(object_suffix = str_extract(dir_object, 
                                "\\.[:alpha:]*$"), 
           dir_isend = ifelse(is.na(object_suffix), 
                              FALSE, TRUE), 
           dir_path = ifelse(dir_isend == FALSE, 
                             paste0(search_path, '/', dir_object), 
                             NA))
  return(bb)}

file_cabinet()
file_cabinet(paste0(getwd(), '/utilities'))

