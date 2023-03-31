
# bulk column selector script --------------------------------------
# require(tidyverse)
# require(janitor)

# View(colnames(raw_df))
old_cols <- c(colnames(raw_df))

# manually type the desired column names !!!!!!!!!!!!!!!!!!!!!!!!!!
new_cols <- as.vector(c('apple', 'ZEBRA'))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# run checks to confirm the results :::::::::::::::::::::::::
paste("Number of new columns:", length(new_cols))
paste("Number of cols eliminated:", length(old_cols) - length(new_cols))
col_matched <- new_cols %in% old_cols
col_checker <- cbind(new_cols, col_matched) %>% as_tibble()
paste("Incorrect columns:", sum(col_checker$col_matched == FALSE))
col_checker %>% filter(col_matched == FALSE)
# View(col_checker %>% filter(col_matched == FALSE))

# bulk select columns into a new df1 object ::::::::::::::::::
df1 <- raw_df %>% as_tibble() %>% 
  select(all_of(new_cols)) %>% clean_names()
dim(df1)

# cleanup ???????????????????????????????????????????????
rm(old_cols, new_cols, col_matched, col_checker)
rm(raw_df)

# ^ -----


# tests ++++++++++++++++++++++++++++++++++++++++++++++++++
raw_df <- data.frame(aa = runif(10), 
                     bb = runif(10), 
                     apple = runif(10), 
                     ZEBRA = runif(10), 
                     column_number_8923 = runif(10))
raw_df
# run through the script above to confirm functionality
