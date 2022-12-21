# -------------------------------------------------------------------------
#' ---
#' title: "DM.convert.clean.sas2rdb"
#' author: "Christian"
#' date: "`r format(Sys.time(), '%B %d, %Y')`"
#' ---
# 2022-02-21
#
# Converts SAS files to RDS in current-directory
# 1) Converts files not to be checked for double entries (don't have eventid/infodt)
# 2) In one step, 
#    - removes documented double entries
#    - checks and lists new double entries in  ".double.entries.list.txt"
# ".double.entries.list.documented.txt" is read during this process 
# Changed .ds.FACOMS to read from RDS files

# setup -------------------------------------------------------------------

rm(list = ls())
library(haven)

path    <- '../DATA/FACOMS/2022-04/'
pathout <- '../DATA/FACOMS/current/'

# datasets ----------------------------------------------------------------

file_list.all <- list.files(path = path, pattern = 'sas7bdat')

not_checked <- c('conmed.sas7bdat', 
                 'currcond.sas7bdat', 
                 'famp.sas7bdat',
                 'famed.sas7bdat',
                 'icflog.sas7bdat',
                 'mhxgen.sas7bdat',
                 'pregoutc.sas7bdat',
                 'random.sas7bdat',
                 'redenlab.sas7bdat',
                 'screen.sas7bdat',
                 'sig.sas7bdat',
                 'clatb.sas7bdat', # both need to be checked manually (14 lines / visit)
                 'clat1.sas7bdat',  #
                 'uid.sas7bdat'
)

file_list <- file_list.all[! file_list.all %in% not_checked]

# convert non_checked -----------------------------------------------------

cat('converting non-checked\n')

for (i in 1:length(not_checked)){
  ds.tmp <- read_sas(paste0(path, not_checked[i]))
  name   <- str_replace(not_checked[i], pattern = "sas7bdat", replacement = "")
  cat(name)
  assign(name, ds.tmp)
  name <- gsub('\\.','',name )
  ds.tmp %>%  .wds(paste(pathout, name, sep=''))
}

# checking datasets -------------------------------------------------------

de.doc <- .rt('../DATA/FACOMS/2021-11/.double.entries.list.documented.txt') %>%
  mutate_at('c_infodt', ~as.Date(., format="%Y-%m-%d")) %>%  mutate_at('patno', ~as.character(.)) %>% 
  select(patno, event_id, c_infodt, pag_name, name, delete.line) %>% 
  filter(delete.line)

double.entries.list <- as_tibble(data.frame())

for ( i in 1:length(file_list) ){
  
  ds.tmp        <- read_sas(paste0(path, file_list[i]))
  names(ds.tmp) <- tolower(names(ds.tmp))
  
  name   <- str_replace(file_list[i], pattern = "sas7bdat", replacement = "")
  
  if( nrow( filter(de.doc, name == name) ) > 0) { # checks 
    ds.tmp %<>%
      mutate(name = name) %>% 
      left_join(de.doc, by = c("patno", "event_id", 'pag_name', "name", "c_infodt")) %>%
      filter(is.na(delete.line))
  }
  
  dp.list <- ds.tmp %>% group_by(patno, c_infodt) %>% # this list is per file
    filter(n()>1) %>% 
    mutate(name = name) %>% 
    select(patno, event_id, c_infodt, pag_name, name) %>% 
    mutate(delete.line = NA)
    
  if ( nrow (dp.list) > 0) {
    double.entries.list %<>%
      bind_rows(dp.list)
  }
  
  cat(name)
  assign(name, ds.tmp)
  name <- gsub('\\.','',name )
  ds.tmp %>% 
    select(-delete.line, -name) %>% 
    .wds(paste(pathout, name, sep=''))
    
}

# write remaining double entries ------------------------------------------

double.entries.list %>% 
  unique() %>%
  .wt(paste(path, '.double.entries.list', '.txt', sep=''))

# . -----------------------------------------------------------------------
# 
# read_sas(paste0(path, file_list[1])) %>% 
#   group_by(PATNO, C_INFODT) %>% filter(n()>1) %>% summarise(l = length((PATNO)))
# 
# read_sas(paste0(path, file_list[1])) %>% 
#   group_by(PATNO, C_INFODT) %>% filter(n()>1) 

# .rt('../DATA/FACOMS/2021-11/all_dups.txt') %>%
#   unique %>% 
#   ggplot()+geom_bar()+
#   aes(x = pag_name)+
#   aes(fill = factor(patno))
# 
# table(all_dups.txt$pag_name)


# . -----------------------------------------------------------------------

# .rt('../DATA/FACOMS/2021-11/all_dups.txt') %>%
#   unique %>% 
#   select(patno, event_id, pag_name, c_infodt) %>% 
#   group_by(patno, c_infodt, pag_name) %>% 
#   mutate(rank = rank(event_id, ties.method = 'first')) %>%
#   spread(rank, event_id) %>% 
#   arrange(c_infodt, patno) %>% 
#   .ct

  
