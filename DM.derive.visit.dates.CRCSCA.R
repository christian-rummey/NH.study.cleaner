
# setup -------------------------------------------------------------------

rm(list = ls())

path <- '../DATA/CRCSCA/current/'

# CRCSCA get ages and starting dates --------------------------------------

reg. <- .ds.CRCSCA('registration') %>% 
  .gs

dem. <- .ds.CRCSCA('demographics') %>% 
  select( sjid, ageatregistration, dm.date = visit_date ) %>% 
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
  select ( study, sjid, start_age, dm.age.used ) %>% 
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
  ds.tmp <- .ds.CRCSCA( name, all = T, raw = T, raw.dates = F ) %>% 
    filter(avisit != 'PRN') 
  
  # check what lines are completely identical
  ds.tmp %<>%
    mutate(
      unique_row = !duplicated(
        select(
          ., 
          -adt, -avisit, -avisitn, 
          -visit_age, -visityear
        ) %>%
          relocate(study, sjid, visit_date)
      )
    ) %>% 
    filter(unique_row == T)

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

# 1) clean -------------------------------------------------------------------

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


# GPT Cleaning ----------------------------------------------------------------

# simplify visit.dates, add count by adt

visit.dates %<>% 
  select ( study, sjid, avisitn, adt, crf ) %>% 
  group_by(study, sjid, avisitn, adt) %>%
  mutate( n.crf_date = n_distinct(crf)) %>%
  ungroup()

# library(stringdist)

# 2) correct dates with wrong avisitn. crf can not match -------------------------

# ".tbc" -> to be corrected

inconsistent. <- visit.dates %>%
  # filter( sjid == 'CU-01-018-7301') %>% print(n=40)
  rename( crf.tbc        = crf        ) %>% 
  rename( n.crf_date.tbc = n.crf_date ) %>%
  rename( avisitn.tbc    = avisitn    ) %>% 
  # select( - n.crf_date ) %>%
  left_join(
    visit.dates,
    by = c( 'study', 'sjid', 'adt' ),
    relationship = "many-to-many"
  ) %>% 
  filter( avisitn.tbc != avisitn ) %>% 
  filter( crf.tbc     != crf ) %>% 
  select( -crf ) %>% 
  # what's here needs correction
  unique( )

corrections. <- inconsistent. %>%
  # filter( sjid %in% c('CU-01-003-7301','CU-01-032-7301') ) %>% 
  arrange(sjid, adt) %>% 
  group_by( sjid, adt ) %>%
  # filter to the avisitn, with correct adt
  filter( n.crf_date == max( n.crf_date     ) ) %>% 
  group_by( sjid, avisitn ) %>%
  filter( n.crf_date.tbc == min( n.crf_date.tbc ) ) %>% 
  select( study, avisitn = avisitn.tbc, avisitn.new = avisitn, crf = crf.tbc)

# apply avisitn corrections 

visit.dates %<>% 
  # filter( sjid == 'CU-01-018-7301') %>%
  # filter( sjid == 'CU-01-003-7301') %>%
  left_join(
    corrections.
    ) %>% 
  mutate(
    avisitn.corrected = coalesce(avisitn.new, avisitn),
    avisitn.original  = if_else(is.na(avisitn.new), NA_real_, avisitn)
  ) %>%
  select(study, sjid, avisitn = avisitn.corrected, adt, crf, n.crf_date, avisitn.original)

# when more than one crf per visit is created, remove the changed one

visit.dates %<>% #print(n=39)
  arrange ( study, sjid, avisitn, adt, crf, n.crf_date) %>% 
  group_by( study, sjid, avisitn, adt, crf ) %>% 
  mutate  ( rk = rank(crf, ties.method = 'last') ) %>% 
  filter  ( max(rk)==1) %>% 
  ungroup
# %>% 
#   filter( !is.na(avisitn.original))
#   mutate( x = length((crf)))

visit.dates %<>% 
  select( -rk )

# 3) if diff <= 31 days, use median -------------------------------------------
unique(visit.dates$adt) %>% length

visit.dates %<>%
  group_by ( study, sjid, avisitn ) %>%
  mutate  ( diff = as.numeric(max(adt)-min(adt))  ) %>%
  mutate  ( adt.before_median = adt ) %>% 
  mutate  ( adt  = if_else(diff <= 31, median(adt), adt )) %>%
  group_by ( study, sjid, avisitn, adt  ) %>%
  mutate  ( n.crf_date = length (unique(crf))) %>%
  group_by ( study, sjid, avisitn  ) %>%
  mutate  ( diff = as.numeric(max(adt)-min(adt))  )

# counts ------------------------------------------------------------------

.svs <- function(df) {
  df <- df %>% ungroup()  # Ensure it's ungrouped
  
  tibble::tibble(
    total_rows           = nrow(df),
    unique_subjects      = nrow(df %>% select(study, sjid) %>% distinct()),
    unique_visits        = nrow(df %>% select(study, sjid, avisitn) %>% distinct()),
    unique_dates         = nrow(df %>% select(study, sjid, adt) %>% distinct()),
    unique_crf_records   = nrow(df %>% select(study, sjid, avisitn, adt, crf) %>% distinct())
  )
}

# figure out the rest -----------------------------------------------------


library(dplyr)
library(purrr)

# --- Step 1: Identify both types of conflicts ---
conflict_rows <- bind_rows(
  visit.dates %>%
    group_by(study, sjid, adt) %>%
    filter(n_distinct(avisitn) > 1) %>%
    ungroup() %>%
    mutate(conflict_type = "same adt, multiple avisitn"),
  
  visit.dates %>%
    group_by(study, sjid, avisitn) %>%
    filter(n_distinct(adt) > 1) %>%
    ungroup() %>%
    mutate(conflict_type = "same avisitn, multiple adt")
) %>%
  distinct(study, sjid, avisitn, adt, crf, conflict_type)

# --- Step 2: Collapse CRFs per visit entry ---
conflict_summary <- conflict_rows %>%
  group_by(study, sjid, avisitn, adt, conflict_type) %>%
  summarise(
    n_crf = n_distinct(crf),
    distinct_crfs = list(sort(unique(crf))),
    .groups = "drop"
  )

# --- Step 3: Compute shared CRFs across conflicting entries within same patient ---
conflict_enriched <- conflict_summary %>%
  mutate(group_key = if_else(conflict_type == "same adt, multiple avisitn",
                             as.character(adt), as.character(avisitn))) %>%
  group_by(sjid, conflict_type, group_key) %>%
  filter(n() > 1) %>%
  mutate(
    shared_crfs = list(reduce(distinct_crfs, intersect)),
    n_shared_crfs = length(shared_crfs[[1]])
  ) %>%
  ungroup() %>%
  # select(-group_key) %>%   # optional: drop if not needed
  droplevels()

# --- Step 4: Make readable ---
conflict_enriched <- conflict_enriched %>%
  mutate(
    distinct_crfs = sapply(distinct_crfs, function(x) paste(sort(x), collapse = ", ")),
    shared_crfs   = sapply(shared_crfs,   function(x) paste(sort(x), collapse = ", "))
  )

# --- Step 5: Show unified result ---
cat("\n--- Visits with conflicting AVISITNs or ADTs and shared CRFs ---\n")
print(conflict_enriched, n = 1050)

# use this to have unique avisitn/adt combinations ------------------------

library(dplyr)

# STEP 1: Start from combined conflict data (conflict_summary)
conflict_resolved <- conflict_summary %>%
  group_by(study, sjid, conflict_type, ifelse(conflict_type == "same adt, multiple avisitn", as.character(adt), as.character(avisitn))) %>%
  slice_max(n_crf, with_ties = FALSE) %>%
  ungroup()

# STEP 2: Prepare to merge back with unaffected data
# Create a lookup of affected sjid + adt + avisitn combinations
affected_keys <- conflict_summary %>%
  distinct(study, sjid, adt, avisitn)

# STEP 3: Get all unaffected records from visit.dates
unaffected <- visit.dates %>%
  distinct(study, sjid, adt, avisitn, crf) %>%
  anti_join(affected_keys, by = c("study", "sjid", "adt", "avisitn"))

# STEP 4: Combine resolved + unaffected
final_clean_visits <- bind_rows(
  conflict_resolved %>%
    select(study, sjid, adt, avisitn),  # just the unique combos to match structure
  unaffected
) %>%
  arrange(study, sjid, adt, avisitn)

# Optional: Add back n_crf and crf list if needed

# . -----------------------------------------------------------------------

final_clean_visits %>% .svs %>% .ct

visit.dates <- final_clean_visits

# # --- STEP 1: Prepare both datasets ---
# original <- visit.dates %>%
#   select(study, sjid, adt, avisitn, crf) %>%
#   mutate(from = "original")
# 
# cleaned <- final_clean_visits %>%
#   select(study, sjid, adt, avisitn, crf) %>%
#   mutate(from = "cleaned")
# 
# # --- STEP 2: Stack both and identify changes ---
# comparison <- bind_rows(original, cleaned) %>%
#   distinct() %>%
#   group_by(study, sjid, adt, avisitn, crf) %>%
#   mutate(n_sources = n_distinct(from)) %>%
#   ungroup()
# 
# # --- STEP 3: Filter for entries that appear only in one dataset ---
# changes <- comparison %>%
#   group_by(study, sjid, crf) %>%
#   filter(n_distinct(from) == 1) %>%
#   ungroup()
# 
# 
# 
# 
# # --- STEP 4: Identify affected sjid/crf combos ---
# affected_sjid_crf <- changes %>%
#   distinct(sjid, crf)
# 
# # --- STEP 5A: Extract all rows for affected sjid/crf from original data ---
# comparison %>% 
#   left_join( 
#     affected_sjid_crf %>% 
#       mutate(x = 1)
#     ) %>% 
#   filter(x == 1) %>% 
#   arrange
# 
# original_rows <-  %>%
#   right_join(visit.dates, by = c("sjid", "crf")) %>%
#   mutate(from = "original")
# 
# # --- STEP 5B: Do the same for cleaned data ---
# cleaned_rows <- affected_sjid_crf %>%
#   right_join(final_clean_visits, by = c("sjid", "crf")) %>%
#   mutate(from = "cleaned")
# 
# # Combine
# data_plot_combined <- bind_rows(original_rows, cleaned_rows)
# 
# data_plot_combined %>% 
#   filter(sjid == 'CU-01-006-7301')
#   ggplot(aes(x = adt, y = avisitn, color = from, shape = from)) +
#   geom_point(size = 3, alpha = 0.9) +
#   facet_grid(crf ~ sjid, scales = "free", space = "free") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(
#     title = "CRF-wise Changes in AVISITN Assignment by Patient",
#     x = "Visit Date (ADT)",
#     y = "AVISITN",
#     color = "Dataset",
#     shape = "Dataset"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "bottom",
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     strip.text.y = element_text(angle = 0)
#   )

# if multiple dates per visit, use the one with most crfs -----------------
# 
# # visit.dates.unique <- 
# visit.dates %<>% 
#   group_by ( study, sjid, avisitn ) %>% 
#   # filter   ( length(unique(adt))>1 ) %>% 
#   group_by ( study, sjid, avisitn, adt ) %>% 
#   summarise( n.crf_date = length (unique(crf))) %>% 
#   group_by ( study, sjid, avisitn ) %>% 
#   # will return the first row in the current data order among those with 
#   # the maximum value.
#   arrange( study, sjid, avisitn, n.crf_date ) %>% 
#   slice_max(n.crf_date, with_ties = FALSE) %>% 
#   select(-n.crf_date)
# 

# write -------------------------------------------------------------------

visit.dates %>% 
  select(-crf) %>% unique %>% 
  .gs %>% 
  left_join(
    ages.CRCSCA %>% 
      select( sjid, age_bl = start_age )
  ) %>% 
  mutate( time. = as.numeric( adt - min(adt) )/365.25 ) %>% 
  mutate( age   = age_bl + time. ) %>% 
  arrange( sjid, avisitn, adt ) %>% 
  .wds('../DATA derived/visit.dates.CRCSCA', add.date = T)			

