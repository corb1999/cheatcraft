
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
                         day(Sys.time()), hour(Sys.time()), minute(Sys.time()), 
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