
# setup -------------------------------------------------------------------

rm(list = ls())

# path <- '../DATA/EFACTS/current/'
path <-'../DATA/EFACTS/2022-07-06 export8/'
# datasets ----------------------------------------------------------------

filename_list.all <- list.files(path = path, pattern = 'csv') %>%
  .[!grepl('_codes', .)] %>% 
  gsub('_export8_20220706_temp.csv','',.)

# none of these should have a patient-visit scheme
# neurological_exam - is a special case - should be done at BL, but has done many times at later visits

crfs.exclude <- as.list(
  c(
    # 'TimeZone','AE','ADL','CE','CCFS','DM','EQ5D','FACHSC','FAH','FAHISCR','FA-HI','FA','FACR-HI','MH','MFIS',
    # 'MFISP','P_ADL','P_EQ5D','P_FA-HI','P_SF10','PFACR','RSCR','SCO','SEF','SF10','SF10SC','SF36','SF36P','VF','VFQ','VFQP',
    'SARA'
  # 'concl','conmed','currcond','icflog','random',
  # 'famed','facmstat','famp','screen','pregoutc',
  # 'sig',
  # 'visstat',
  # 'mhcnsnsus'
  ))


filename_list <- filename_list.all[! filename_list.all %in% paste0(crfs.exclude)]

# sara is base (although not longest dt) ----------------------------------

visit.dates <- .ds.EFACTS('sara') %>% 
  mutate(crf = 'sara') %>% 
  select(sjid, avisit, avisitn, crf, adt)

# file_list <- file_list[c(22,23)]

for (i in 1:length(filename_list)){
  name   <- filename_list[i]
  
  cat(name);cat("\n")
  
  ds.tmp <- .ds.EFACTS( name ) %>% 
    filter(avisitn != '')
	
  if (nrow(ds.tmp)==0) {next}
  
  # if ( name %in% c(
  #   'faneuro',
  #   crfs.exclude
  # ) ) {next}
  
  # if ( name == '_9hpt' ) { name <- 'nhpt'}
  # 
  # if ( name == 'clat1')  { 
  #   ds.tmp %<>%
  #     select(-od100_1, -os100_2) %>%  spread(acuity, ou100_1)
  # }
  
  # if ( name == 'clatb')  { 
  #   ds.tmp %<>%
  #     select(-ou25) %>% spread(seq_no, ou125)
  # }
  
  # if ( name == 'mhxgen')  { 
  #   ds.tmp %<>%
  #     select(sjid, avisit, avisitn, adt) %>% 
  #     unique
    
  # }
  
  ds.tmp %<>% 
    mutate(crf = name) %>% 
    select(sjid, avisit, avisitn, crf, adt)
  
  # cat(name);cat("\n")
  
  visit.dates %<>%
    bind_rows(ds.tmp)
}

rm(ds.tmp, crfs.exclude, filename_list, filename_list.all, i, name, path)

visit.dates %<>%
  arrange(sjid, crf, avisitn) %>% 
  mutate(study = 'EFACTS') %>%
  select(study, sjid, avisit, avisitn, everything())

# derive flags ------------------------------------------------------------

crf.list <- unique(visit.dates$crf)

# summarise CRFs ----------------------------------------------------------

crf.summary <- visit.dates %>%
  group_by(sjid, avisit, avisitn, adt) %>%
  mutate(n = n()) %>% 
  group_by(study, sjid, avisit, avisitn, adt, n) %>%
  summarise_all( ~toString(na.omit(.)) )

# select double ages only -------------------------------------------------

mult.date.visits <- visit.dates %>% 
  select(-crf) %>% 
  group_by(study, sjid, avisit, avisitn) %>% 
  unique() %>% 
  filter(n()>1) %>%
  arrange(sjid, avisitn) %>% 
  mutate( diff = as.numeric(max(adt)-min(adt))) %>%
  mutate( flagged = T ) %>% 
  left_join(crf.summary)

# write flags -------------------------------------------------------------

mult.date.visits %>% 
  ungroup %>% 
  select(study, sjid, avisit, avisitn, adt, diff, flagged, n, crf) %>% 
  arrange(sjid, avisitn) %>% 
  .wds ('../DATA derived/visit.flags.EFACTS', add.date = T)

# quick summary -----------------------------------------------------------

mult.date.visits %>% 
  select(sjid, avisitn, diff) %>% 
  unique %>%
  group_by(six.months = cut(diff, c(0,31,90,180,1000,2000,3000,1000000000000000000))) %>% 
  summarise(n())

# write -------------------------------------------------------------------

visit.dates %<>% 
  left_join(
    mult.date.visits %>% 
      ungroup %>% 
      select(study, sjid, avisitn, n, diff, flagged) %>% 
      unique %>%
      group_by(study, sjid, avisitn, diff, flagged) %>% 
      summarise_all( ~toString(na.omit( paste(.) )) )
  ) %>% 
  arrange(study, sjid, avisitn, adt) %>% 
  select(study, sjid, avisit, avisitn, adt, n, diff, crf, everything())

visit.dates %>% 
  .wds('../DATA derived/visit.dates.EFACTS', add.date = T)			

# visit.datvisit.dateses -------------------------------------------------------------

visit.dates %>% 
  filter( avisitn != (-1) ) %>% 
  select(-crf) %>% 
  unique %>% 
  group_by(sjid, avisit, avisitn ) %>% 
  filter(n()>1)

