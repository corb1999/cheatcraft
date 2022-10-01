
# data loaders -----------------------------------------------------

# load a csv file
loader_path1 <- paste0(getwd(), "/raw_data.csv")
clockin()
raw_df <- read.csv(loader_path1, stringsAsFactors = FALSE)
clockout()
dim(raw_df)

# load a rds file
loader_path1 <- paste0(getwd(), "/raw_data.rds")
clockin()
raw_df <- readRDS(loader_path1)
clockout()
dim(raw_df)

# load an excel file
loader_path1 <- paste0(getwd(), "/raw_data.xlsx")
clockin()
raw_df <- read_excel(path = loader_path1, sheet = 1)
clockout()
dim(raw_df)

# load a shape file
# require(sf)
loader_path1 <- paste0(getwd(), "/shapefile_folder")
clockin()
dfsf_a <- st_read(dsn = loader_path1)
clockout()
head(dfsf_a)

# source another script
sourcerpath <- paste0(getwd(), '/script.R')
clockin()
source(file = sourcerpath)
clockout()

# unzip snippet
unzip_this <- paste0(getwd(), '/zipped')
unzip_to <- paste0(getwd(), '/unzipped')
clockin()
unzip(zipfile = unzip_this, exdir = unzip_to)
clockout()

# ^ -----

# data writes -------------------------------------------------

# write to rds
filename <- paste0(getwd(), "/dataframe.rds")
clockin()
saveRDS(df, file = filename)
clockout()

# write to csv
filename <- paste0(getwd(), "/dataframe.csv")
clockin()
write.csv(df, file = filename, row.names = FALSE)
clockout()

# ^ -----

# quick plot saving function -------------------------------------------

# require(lubridate)
qp <- function(pltname, pltpath_suffix = NA, plt_inch = 5) {
  plt_timestamp <- paste(year(Sys.time()), month(Sys.time()),  
                         day(Sys.time()), hour(Sys.time()), 
                         minute(Sys.time()), 
                         floor(second(Sys.time())), sep = "-")
  aa <- ifelse(is.na(pltpath_suffix), "", pltpath_suffix)
  plt_filepath <- paste0(getwd(), aa)
  plt_name <- paste0("plt_", pltname, "_", plt_timestamp, ".png")
  ggsave(filename = plt_name, plot = last_plot(), 
         path = plt_filepath, scale = 1, device = "png", 
         height = plt_inch, width = plt_inch * 1.61803399, units = "in")}


# test the plot saver +++++++++++++++++++++++++++++++++++++
# ggplot(data = mtcars, aes(mpg)) + geom_histogram()
# qp(pltname = "test")

# ^ -----

# saving function for gt tables -------------------------------------

# require(tidyverse)
# require(lubridate)
# require(gt)
gt_printer <- function(arg_gt, 
                       arg_filenm = '', arg_path_suffix = '') {
  plt_timestamp <- paste(year(Sys.time()), month(Sys.time()),  
                         day(Sys.time()), hour(Sys.time()), 
                         minute(Sys.time()), 
                         floor(second(Sys.time())), sep = "-")
  final_filenm <- paste0('ggtable_', arg_filenm, '_', 
                         plt_timestamp, '.html')
  final_path <- paste0(getwd(), arg_path_suffix)
  gtsave(data = arg_gt, filename = final_filenm, path = final_path)
}

# test ??????????????????????????????????????
# mtcars |> as_tibble() |> group_by(cyl) |> 
#   summarise(avg_mpg = mean(mpg), avg_hp = mean(hp)) |> gt() |> 
  # gt_printer()
  # gt_printer(arg_filenm = 'alpha_test', 
  #            arg_path_suffix = '/utilities')

# ^ -----