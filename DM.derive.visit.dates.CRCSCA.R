
# setup -------------------------------------------------------------------

rm(list = ls())

path <- '../DATA/CRCSCA/current/'

# datasets ----------------------------------------------------------------

filename_list.all <- list.files(path = path, pattern = 'rds') %>% 
  gsub('.rds','',.)

# none of these should have a patient-visit scheme
# neurological_exam - is a special case - should be done at BL, but has done many times at later visits

crfs.exclude <- c(
  'age_at_onset','conclusionofstudyparticipation','dsn_names7301','registration','general_physical_exam',
  'covid_19_visit_impact_form', 'deathrecord','eligibility','genetic_results',
  'physician_global_impression','patient_global_impression','concomitant_meds','demographics',
  'demographics_supplement','medical_history',
  'neurological_exam',
  'dsn_names7301', 'registration',
  'change_of_status'
  )

filename_list <- filename_list.all[! filename_list.all %in% paste0(crfs.exclude)]					 

visit.dates <- data.frame() %>% as_tibble()

for (i in 1:length(filename_list)){
  name   <- filename_list[i]
  ds.tmp <- .rd.CRCSCA( name, all = T ) %>% 
    filter(avisit != 'PRN') 
  
  if ( !('sjid'   %in% names(ds.tmp) ) ) {next}
  if ( !('avisit' %in% names(ds.tmp) ) ) {next}
  if ( name %in% c(
    # 'sara',
    crfs.exclude
  ) ) {next}
  
  if ( name == '_9hpt' ) { name <- 'nhpt'}
  
  ds.tmp %<>% 
    mutate(crf = name) %>% 
    select(crf, sjid, avisit, avisitn, adt)
  
  cat(name);cat("\n")
  
  visit.dates %<>%
    bind_rows(ds.tmp)
}

rm(ds.tmp, crfs.exclude, filename_list, i, name, path)

visit.dates %<>%
  arrange(sjid, crf, avisit) %>% 
  mutate(study = 'CRCSCA') %>%
  select(study, sjid, avisit, avisitn, everything())

# derive flags ---------------------------------------------------------------

crf.summary <- visit.dates %>%
  group_by(sjid, avisit, avisitn, adt) %>%
  mutate(n = n()) %>% 
  group_by(study, sjid, adt) %>% select(-c(avisit, avisitn, n)) %>%
  # group_by(study, sjid, avisitn) %>% select(-c(adt, avisitn)) %>% 
  # group_by(study, sjid, adt, avisit, avisitn, n) %>%
  summarise_all( ~toString(na.omit(.)) )

crf.summary.sara <- visit.dates %>%
  filter(crf == 'sara') %>% 
  group_by(sjid, avisit, avisitn, adt) %>%
  mutate(n = n()) %>% 
  group_by(study, sjid, adt) %>% select(-c(avisit, avisitn, n)) %>%
  # group_by(study, sjid, avisitn) %>% select(-c(adt, avisitn)) %>% 
  # group_by(study, sjid, adt, avisit, avisitn, n) %>%
  summarise_all( ~toString(na.omit(.)) )

# multiple ages (now dates) only ------------------------------------------------------

mult.date.visits <- visit.dates %>% 
  select(-crf) %>%
  group_by(study, sjid, adt) %>%
  unique() %>% 
  mutate(avisit.n = length(avisit), adt.n = length(adt), avisitn.n = length(avisitn)) %>% 
  ungroup %>%
  filter( rowSums(.[c('avisit.n', 'adt.n', 'avisitn.n')]) > 3 )

mult.date.visits %>% select(study, sjid, avisit) %>% unique %>% nrow
mult.date.visits %>% select(study, sjid, adt) %>% unique %>% nrow

mult.date.visits$avisit.n

mult.date.visits %>% 
  group_by(study, sjid, avisit) %>% 
  summarise(diff = as.numeric(max(adt)-min(adt))) %>% 
  filter(diff > 0)

# mult.date.visits %>% 
#   unique() %>% 
#   left_join(crf.summary, by = c('study', 'sjid', 'adt') ) %>% 
#   arrange(sjid, avisit)
#   
#   # unique() %>% 
#   # group_by(study, sjid, adt, avisit) %>%
#   # filter(n()>1) %>% 
#   # mutate(l = length(adt)) %>% 
#   # filter(l > 1)
#   # filter(length(unique(adt))>1)
#   # filter(n()>1) %>%
#   # arrange(sjid, avisitn, age) %>% 
#   # mutate( diff = as.numeric(max(age)-min(age))) %>%
#   # mutate( flagged = T ) %>% 
#   # left_join(crf.summary)
# 
# # write this flag -------------------------------------------------------------
# 
# mult.date.visits %>% 
#   ungroup %>% 
#   select(study, sjid, avisit, avisitn, age, diff, flagged, n, crf) %>% 
#   arrange(sjid, avisitn, age) %>% 
#   .wds ('../DATA derived/CRCSCA.flag.mult.visit', add.date = T)
# 
# # quick summary -----------------------------------------------------------
# 
# mult.date.visits %>% 
#   select(sjid, avisitn, diff) %>% 
#   unique %>%
#   group_by(six.months = cut(diff, c(0,31,90,180,1000,2000,3000,1000000000000000000))) %>% 
#   summarise(n())
# 
# bl.ages <- visit.dates %>% 
#   select(study, sjid, bl.age = age) %>% unique %>% 
#   group_by(study, sjid) %>% 
#   filter(bl.age == min(bl.age))
# 
# # add age deviation in days -----------------------------------------------
# # done by crf 
# # these are not discrepancies between CRFs, but visit window deviations!
# 
# age.devs.by.visit <- visit.dates %>%
#   left_join(bl.ages) %>%
#   ungroup %>%
#   mutate(stdy = as.character(avisit)) %>%
#   mutate(stdy = ifelse(stdy == 'Baseline', 0, stdy)) %>%
#   mutate(stdy = gsub(' months', '', stdy)) %>%
#   mutate(stdy = (as.numeric(stdy)*30.4375)) %>%
#   mutate(vs.age = bl.age + stdy) %>%
#   mutate(age.dev = vs.age-age)

# tol <- 365.25/4
# tol <- 0
# 
# age.devs.by.visit %<>% 
#   filter(!is.na(age)) %>% 
#   group_by(crf) %>%
#   mutate  ( N = n()) %>% 
#   mutate(flag.window = ifelse(abs(age.dev)>tol, F, T))
# 
# age.devs.by.visit %>%
#   group_by(crf, N, flag.window) %>% 
#   summarise(sjids = length(sjid)) %>% 
#   spread(flag.window, sjids) %>% 
#   mutate (pct_exl = round(100*`FALSE`/N,0))

# age.devs.by.visit %>%
#   # filter(crf %in% c('sara','functional_staging')) %>% 
#   # spread(crf, age.dev) %>% filter(sara != functional_staging)
#   mutate(age.dev = ifelse(age.dev>365.25*2, 365.25*2, age.dev)) %>% 
#   mutate(age.dev = ifelse(age.dev<(-365.25*2), (-365.25*2), age.dev)) %>% 
#   # filter(avisitn == 0, age.dev!=0) %>% group_by(sjid)
#   mutate(flag = ifelse(abs(age.dev)>tol, T, F)) %>% 
#   # select(-crf) %>% unique %>% 
#   ggplot()+geom_jitter(width = .1, height = 0)+
#   aes(x = stdy/365.25, y = age.dev/365.25)+
#   aes(color = flag)+
#   facet_wrap(~crf)+
#   geom_hline(yintercept = c(-tol/365.25,0,tol/365.25), linetype = 3)

# write this flag -------------------------------------------------------------

# age.devs.by.visit %>% 
#   ungroup %>% 
#   select(study, sjid, avisit, avisitn, crf, age.dev, flag.window) %>% 
#   arrange(sjid, avisitn, crf) %>% 
#   .wds ('../DATA derived/CRCSCA.flag.age.off', add.date = T)

# write -------------------------------------------------------------------

mult.visit.per.adt <- visit.dates %>% 
  # filter(sjid == 'CU-01-039-7301', adt == '2020-09-01') %>%
  ungroup %>% 
  select(-avisitn) %>% 
  group_by(study, sjid, avisit, adt) %>% 
  summarise_all( ~toString(na.omit( paste(.) )) ) %>%
  group_by(study, sjid, adt) %>% 
  unique %>%
  filter(n()>1)
# %>% 
#   group_by(study, sjid, adt) %>% 
#   group_by(study, sjid, adt) %>% 
#   filter(n()>1) %>% 
#   arrange(study, sjid, adt)
mult.visit.per.adt %>% 
  arrange(adt, sjid) %>% 
  .ct

visit.dates %>% 
  arrange(sjid, avisitn, adt) %>% 
  .wds('../DATA derived/visit.dates.CRCSCA', add.date = T)			

