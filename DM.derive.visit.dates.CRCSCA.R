
# setup -------------------------------------------------------------------

rm(list = ls())

path <- '../DATA/CRCSCA/current/'

# CRCSCA get ages and starting dates --------------------------------------

reg. <- readRDS ('../DATA/CRCSCA/current/registration.rds') %>% 
  select( sjid = local_id, start_age, registration_age ) %>%
  .gs

dem. <- readRDS ('../DATA/CRCSCA/current/demographics.rds') %>% 
  select( sjid = local_id, ageatregistration, dm.date = visit_date ) %>% 
  unique() %>% 
  .gs

# start ages,  April 4th
ages.CRCSCA <- reg. %>% 
  left_join(dem.) %>%
  slice(1) %>% # removes one dup
  arrange( start_age ) %>% # 117 have none
  mutate ( start_age   = ifelse( is.na(start_age), registration_age, start_age  ) ) %>% # to 111
  mutate ( dm.age.used = ifelse( is.na(start_age), dm.date, NA        ) ) %>% # 
  mutate ( start_age   = ifelse( is.na(start_age), ageatregistration, start_age ) ) %>% # to 35, but keep date
  filter ( start_age > 1) %>% # removes these 35
  select ( sjid, start_age, dm.age.used ) %>% 
  mutate ( dm.age.used = as.Date(dm.age.used, '%m/%d/%Y'))

# datasets ----------------------------------------------------------------

filename_list.all <- list.files(path = path, pattern = 'rds') %>% 
  gsub('.rds','',.)

# none of these should have a patient-visit scheme
# neurological_exam - is a special case - should be done at BL, but has done many times at later visits

crfs.exclude <- c(
  # 'ccas','cnrs',
  'age_at_onset','conclusionofstudyparticipation','dsn_names7301','registration','general_physical_exam',
  'covid_19_visit_impact_form', 'deathrecord','eligibility','genetic_results',
  'physician_global_impression','patient_global_impression','concomitant_meds','demographics',
  'demographics_supplement','medical_history',
  'neurological_exam',
  'dsn_names7301', 'registration',
  'change_of_status'
  )

filename_list <- filename_list.all[! filename_list.all %in% paste0(crfs.exclude)]					 

# filename_list <- filename_list[c(1,2,3,4)]

visit.dates <- data.frame() %>% as_tibble()

for (i in 1:length(filename_list)){
  name   <- filename_list[i]
  ds.tmp <- .ds.CRCSCA( name, all = T, raw = T ) %>% 
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


# add age and remove no-age-pts -------------------------------------------------------

visit.dates %<>% 
  .gs %>% 
  # filter(is.na(adt))
  inner_join( ages.CRCSCA ) %>% 
  # filter   ( !is.na(dm.age.used)) %>%     
  filter   ( !is.na(start_age) ) %>%       # 
  rename (age_bl = start_age)

visit.dates %<>% 
  .gs %>% 
  mutate( time. = as.numeric( adt -min(adt) ) / 365.25 ) %>% 
  mutate( age   = age_bl + time. ) %>% 
  # filter( is.na(age)) %>% 
  droplevels()

visit.dates %<>% 
  select(-dm.age.used)

# clean -------------------------------------------------------------------

# remove double entry
visit.dates %<>% 
  group_by( study, sjid, avisitn, adt, crf) %>% 
  # filter(n()>1) %>% 
  slice(1)

# remove double entry, wrong date (1)
visit.dates %<>% 
  group_by( study, sjid, avisitn, crf ) %>% 
  # filter(n()>1)
  slice(1)

# if multiple dates per visit, use the one with most crfs -----------------

# visit.dates.unique <- 
visit.dates %<>% 
  group_by ( study, sjid, avisitn ) %>% 
  # filter   ( length(unique(adt))>1 ) %>% 
  group_by ( study, sjid, avisitn, adt ) %>% 
  summarise( n.crf_date = length (unique(crf))) %>% 
  group_by ( study, sjid, avisitn ) %>% 
  # will return the first row in the current data order among those with 
  # the maximum value.
  arrange( study, sjid, avisitn, n.crf_date ) %>% 
  slice_max(n.crf_date, with_ties = FALSE) %>% 
  select(-n.crf_date)


# check <- visit.dates %>% 
#   # filter(sjid == 'HA-01-097-7301') %>% 
#   group_by(study, sjid, avisitn) %>% 
#   mutate( adts_vis = length(unique(adt)) ) %>% 
#   .gs %>% 
#   # filter( max (adts_vis) > 1) %>% 
#   left_join( 
#     visit.dates.clean %>% 
#       mutate(visit.clean = T) %>% 
#       rename(adt.clean = adt)
#     ) %>% 
#   mutate( adt.old = adt ) %>% 
#   mutate( adt = adt.clean ) %>% 
#   .gsv() %>% 
#   mutate( adt = mean(adt),na.rm=T)


# check %<>% 
#   group_by( sjid, crf ) %>% 
#   arrange ( sjid, crf, avisitn) %>% 
#   mutate( lag.avisitn = avisitn - lag (avisitn) ) %>% 
#   mutate( lag.adt     = as.numeric(adt     - lag (adt) ) ) 
# 
# check %>%
#   # filter( avisitn > 0 ) %>% 
#   filter( min (lag.adt) < 0) %>% 
#   print()
# 
# 
# check %<>% 
#   .gsv %>% 
#   mutate(diff = adt.old - adt ) %>% 
#   .gs %>% 
#   mutate(diff = max(diff)) %>% 
#   filter(diff >0 )
# 
# check %>% 
#   select(sjid, avisitn, crf, diff, adt, adt.old) %>%
#   mutate( adt.old = if_else(adt == adt.old, NA, adt.old) ) %>% 
#   gather( dates, values, adt, adt.old) %>% 
#   filter( !is.na(values)) %>% 
#   filter(diff > 10, diff<60) %>% 
#   mutate(flag = ifelse(is.na(diff), NA, T )) %>% 
#   # print(n=80)
#   ggplot()+geom_point()+geom_line()+
#   aes(x = values)+aes(y = avisitn)+
#   aes(color = dates)+
#   facet_wrap(~sjid)


# consider taking mean if < 30days
# 1) make sure it is reproducible - ok
# 2) check that at least dates  are increasint  with visit
# 3) need to recalc ages , etc, then take care of sjids. 


# multiple ages (now dates) only ------------------------------------------------------

# mult.date.visits <- visit.dates %>% 
#   select(-crf) %>%
#   group_by(study, sjid, adt) %>%
#   unique() %>% 
#   mutate(avisit.n = length(avisit), adt.n = length(adt), avisitn.n = length(avisitn)) %>% 
#   ungroup %>%
#   filter( rowSums(.[c('avisit.n', 'adt.n', 'avisitn.n')]) > 3 )
# 
# mult.date.visits %>% select(study, sjid, avisit) %>% unique %>% nrow
# mult.date.visits %>% select(study, sjid, adt) %>% unique %>% nrow
# 
# mult.date.visits$avisit.n
# 
# mult.date.visits %>% 
#   group_by(study, sjid, avisit) %>% 
#   summarise(diff.adt = as.numeric(max(adt)-min(adt))) %>% 
#   filter (diff.adt > 0)
# 
# mult.date.visits %>% 
#   group_by(study, sjid, adt) %>% 
#   mutate( diff.visit = as.numeric(max(avisitn)-min(avisitn))) %>% 
#   filter   ( diff.visit > 0) %>% 
#   arrange  ( -diff.visit, sjid, avisitn )
# 
# visit.dates %>% 
#   filter( sjid == 'CU-01-048-7301' ) %>% 
#   group_by(sjid, avisitn, avisit, adt) %>% 
#   summarise( crf = toString(na.omit(crf)) )
#   select( -crf ) %>% unique %>% 
#   arrange( avisitn, adt)


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

# this only shows that some dates spread over differebt avisitns
visit.dates %>% 
  group_by(study, sjid, avisitn, adt) %>% 
  summarise_all( ~toString(na.omit( paste(unique(.)) )) ) %>%
  group_by(study, sjid, adt) %>% 
  unique %>%
  filter(n()>1)


# mult.visit.per.adt <- visit.dates %>% 
#   # filter(sjid == 'CU-01-039-7301', adt == '2020-09-01') %>%
#   ungroup %>% 
#   select(-avisitn) %>% 
#   group_by(study, sjid, avisit, adt) %>% 
#   summarise_all( ~toString(na.omit( paste(.) )) ) %>%
#   group_by(study, sjid, adt) %>% 
#   unique %>%
#   filter(n()>1)
# %>% 
#   group_by(study, sjid, adt) %>% 
#   group_by(study, sjid, adt) %>% 
#   filter(n()>1) %>% 
#   arrange(study, sjid, adt)

# mult.visit.per.adt %>% 
#   arrange(adt, sjid) %>% 
#   .ct



visit.dates %>% 
  arrange(sjid, avisitn, adt) %>% 
  .wds('../DATA derived/visit.dates.CRCSCA', add.date = T)			

