
# unionizer script --------------------------------------------
# a script to load and UNION ALL a number of datasets together
# require(purrr)

# start the clock timer
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}
# end the clock timer
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# identify dropzone where files are stored and vector the filenames
filepath_prefix_payload <- paste0(getwd(), "/unionizer/payload")

# put the files list into a dataframe
payload <- data.frame(file_nm = list.files(path = filepath_prefix_payload)) %>% 
  mutate(file_nm_full = paste0(filepath_prefix_payload, "/", 
                               file_nm))

# test +++++++++++++++++++++++++++
dim(payload)
# payload[1, 1]
# payload[1, 2]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# write a function to read each file the same way into r
fun_readfiles <- function(filepaths) {
  xx <- read_excel(path = filepaths, 
                   sheet = "Data - Item")
  # xx <- read.csv(filepaths, stringsAsFactors = FALSE)
  return(xx)}

# test +++++++++++++++++++++++++++
# fun_readfiles(payload[1, 2])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute the file reading purrr, and time the execution
clockin()
payload_list <- lapply(payload[, 2], FUN = fun_readfiles)
clockout()

# test +++++++++++++++++++++++++++
# payload_list[[1]]
# dim(payload_list[[1]])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run checks and validation on the loaded files
payload_stats <- data.frame(col_num_chk = map_dbl(payload_list, 
                                                  ncol), 
                            row_num_chk = map_dbl(payload_list, 
                                                  nrow))

fun_colnames_chk <- function(x) {
  aa <- colnames(x)
  bb <- reduce(aa, paste0)
  return(bb)}

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
payload_stats
length(unique(payload_stats[, 1])) == 1
length(unique(map_chr(payload_list, fun_colnames_chk))) == 1

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combine into a single dataframe, as long as checks pass
clockin()
df <- map_dfr(payload_list, rbind)
clockout()

# test +++++++++++++++++++++++++++
dim(df)

# perform checks !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
sum(payload_stats$row_num_chk) == nrow(df)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# execute any additional filter and manipulation before writing

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create and append a metadata tag
metadata_tag <- paste0("unionizer script metadata; ", 
                       "files consumed = ", 
                       nrow(payload_stats), 
                       "; runtime = ", Sys.time(), 
                       "; nrow = ", nrow(df), 
                       "; ncol = ", ncol(df))
metadata_tag
df <- df %>% mutate(metadata_tag = "NA")
df[1, "metadata_tag"] <- metadata_tag

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# write the unionized dataframe out
filename <- paste0(getwd(), "/unionizer/unionizer_outcome.csv")
clockin()
write.csv(df, file = filename, row.names = FALSE)
clockout()

# ^ -----