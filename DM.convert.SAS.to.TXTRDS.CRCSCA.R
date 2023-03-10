# -------------------------------------------------------------------------
#' ---
#' title: "DM.convert.SAS.to.TXTRDS.CRCSCA.R"
#' author: "Christian"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
# 2020-07-15 

# setup -------------------------------------------------------------------
rm(list = ls())
library(haven)

path     <- '../DATA/CRCSCA/20221206/sasv9/'
path.out <- '../DATA/CRCSCA/current/'

# load all datasets -------------------------------------------------------

file_list <- list.files(path = path, pattern = 'sas7bdat')

# file_list <- file_list[c(3,7,10,17,18,45)]

for (i in 1:length(file_list)){
  ds.tmp <- read_sas(paste0(path, file_list[i]))
  names(ds.tmp) <- tolower(names(ds.tmp))
  
  name   <- print(str_replace(file_list[i], pattern = "sas7bdat", replacement = ""))
  assign(name, ds.tmp)
  
  # ifelse ( name != 'trtment' ) {
  #   ds %>% 
  #     select(-studyid, -usubjid)
  # }
  
  saveRDS(ds.tmp, paste(path.out, name, 'rds', sep=''))
  .wt    (ds.tmp, paste(path.out, name, 'txt', sep=''))
  
  rm(ds.tmp)
}
