
# setup -------------------------------------------------------------------

rm(list = ls())

# path <- '../DATA/CRCSCA/202202/'
path <- '../DATA/CRCSCA/20221118/'

# datasets ----------------------------------------------------------------

filename_list.all <- list.files(path = path, pattern = 'rds') %>% 
  gsub('.rds','',.)

# none of these should have a patient-visit scheme
# neurological_exam - is a special case - should be done at BL, but has done many times at later visits

crfs.exclude <- c('age_at_onset','conclusionofstudyparticipation','dsn_names7301','registration','general_physical_exam',
                 'covid_19_visit_impact_form', 'deathrecord','eligibility','genetic_results',
                 'physician_global_impression','patient_global_impression','concomitant_meds','demographics',
                 'demographics_supplement','medical_history',
                 'neurological_exam',
                 'dsn_names7301', 'registration',
				 'change_of_status'
                 )

filename_list <- filename_list.all[! filename_list.all %in% paste0(crfs.exclude)]

# sara is base (although not longest dt) ----------------------------------

registration <- readRDS(paste0(path, 'registration.rds'))

.ds.CRCSCA.old <- function( ds, path, all = F ) {
  
  ds <- readRDS(paste( path, ds , '.rds', sep = ''))
  
  names(ds) <- tolower(names(ds))
  
  if ( 'visit' %in% names(ds) ) {
    ds %<>%
      rename( visit = visit )
  } else {
    ds %<>%
      select( -dmcc_record_id, -form_name )
  }
  
  ds %<>% 
    mutate( study = 'CRCSCA') %>% 
    left_join(registration) %>% 
    mutate_at('maskid', ~as.character(.)) %>% 
    mutate ( age = visit_age/365.25 ) %>% #select( -visit_age ) %>% 
    mutate ( age_bl = start_age/365.25 ) %>% 
    mutate ( adt = as.Date(visit_age-start_age, origin= as.Date('2000-01-01') )) %>%
    arrange(study, maskid, visit_age) %>% 
    mutate ( visit = factor(visit, c("Screening", "Baseline", "6 months", "12 months", "18 months", "24 months", "36 months", "48 months", 
                                     "60 months", "72 months", "84 months", "96 months", "108 months", "120 months","132 months", "144 months",
                                     "192 months", "PRN"))) %>% 
    mutate( avisitn = gsub('Screening','-1', gsub('Baseline','0', gsub(' months', '', gsub('PRN', NA, visit)) ))) %>%
    mutate( avisitn = as.numeric(avisitn)/12 ) %>% 
    select ( study, sjid = maskid, avisit = visit, avisitn, age, start_age, visit_age, visit, everything()) %>% 
    droplevels()
  
  if ( all == F ) {
    ds %<>%
      filter( !(avisitn %in% c(0.5, 1.5)) )
  }
  
  ds %<>% 
    arrange(sjid, avisitn)
  
  return ( ds )
}

visit.dates <- data.frame()

for (i in 1:length(filename_list)){
  name   <- filename_list[i]
  ds.tmp <- .ds.CRCSCA.old( name, path  ) %>% 
    filter(avisit != 'PRN') 
  
  if ( !('sjid'   %in% names(ds.tmp) ) ) {next}
  if ( !('avisit' %in% names(ds.tmp) ) ) {next}
  if ( name %in% c(
    # 'sara',
    crfs.exclude
  ) ) {next}
  
  if ( name == '_9hpt' ) { name <- 'nhpt'}
  
  ds.tmp %<>% 
    select(sjid, avisit, avisitn, age = visit_age ) %>% 
    mutate(crf = name) %>% 
    select(sjid, avisit, avisitn, crf, age)
  
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
  group_by(sjid, avisit, avisitn, age) %>%
  mutate(n = n()) %>% 
  group_by(study, sjid, avisit, avisitn, age, n) %>%
  summarise_all( ~toString(na.omit(.)) )

# multiple ages only ------------------------------------------------------

mult.date.visits <- visit.dates %>% 
  select(-crf) %>% 
  group_by(study, sjid, avisit, avisitn) %>% 
  unique() %>% 
  filter(n()>1) %>%
  arrange(sjid, avisitn, age) %>% 
  mutate( diff = as.numeric(max(age)-min(age))) %>%
  mutate( flagged = T ) %>% 
  left_join(crf.summary)

# write this flag -------------------------------------------------------------

mult.date.visits %>% 
  ungroup %>% 
  select(study, sjid, avisit, avisitn, age, diff, flagged, n, crf) %>% 
  arrange(sjid, avisitn, age) %>% 
  .wds ('../DATA derived/CRCSCA.flag.mult.visit.old.202211', add.date = T)

# quick summary -----------------------------------------------------------

mult.date.visits %>% 
  select(sjid, avisitn, diff) %>% 
  unique %>%
  group_by(six.months = cut(diff, c(0,31,90,180,1000,2000,3000,1000000000000000000))) %>% 
  summarise(n())

bl.ages <- visit.dates %>% 
  select(study, sjid, bl.age = age) %>% unique %>% 
  group_by(study, sjid) %>% 
  filter(bl.age == min(bl.age))

# add age deviation in days -----------------------------------------------
# done by crf 

age.devs.by.visit <- visit.dates %>%
  left_join(bl.ages) %>%
  ungroup %>%
  mutate(stdy = as.character(avisit)) %>%
  mutate(stdy = ifelse(stdy == 'Baseline', 0, stdy)) %>%
  mutate(stdy = gsub(' months', '', stdy)) %>%
  mutate(stdy = (as.numeric(stdy)*30.4375)) %>%
  mutate(vs.age = bl.age + stdy) %>%
  mutate(age.dev = vs.age-age)

# tol <- 365.25/4
tol <- 0

age.devs.by.visit %<>% 
  filter(!is.na(age)) %>% 
  group_by(crf) %>%
  mutate  ( N = n()) %>% 
  mutate(flag.window = ifelse(abs(age.dev)>tol, F, T))

age.devs.by.visit %>%
  group_by(crf, N, flag.window) %>% 
  summarise(sjids = length(sjid)) %>% 
  spread(flag.window, sjids) %>% 
  mutate (pct_exl = round(100*`FALSE`/N,0))

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

age.devs.by.visit %>% 
  ungroup %>% 
  select(study, sjid, avisit, avisitn, crf, age.dev, flag.window) %>% 
  arrange(sjid, avisitn, crf) %>% 
  .wds ('../DATA derived/CRCSCA.flag.age.off.old.202211', add.date = T)

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
  arrange(study, sjid, avisitn, age) %>% 
  select(study, sjid, avisit, avisitn, age, n, diff, crf, everything())

visit.dates %>% 
  arrange(sjid, avisitn, age) %>% 
  .wds('../DATA derived/CRCSCA.visit.dates.old.202211', add.date = T)			

