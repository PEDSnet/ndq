
vs_test <- tibble('check_id' = c('race', 'ethnicity'),
                  'check_description' = c('race', 'ethnicity'),
                  'schema' = c('cdm', 'cdm'),
                  'table' = c('person', 'person'),
                  'valueset_name' = c('valueset_race', 'valueset_ethnicity'),
                  'concept_field' = c('race_concept_id', 'ethnicity_concept_id'),
                  'filter_logic' = c(NA, 'person_id %in% c(1,2,3,4,5,6)'))

test_that('omop_or_pcornet limited inputs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)

  expect_error(check_vs(vs_tbl = vs_test,
                        omop_or_pcornet = 'test'))

  })

test_that('check_vs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))
  config('cache_enabled', FALSE)
  config('results_name_tag', '')
  config('retain_intermediates', FALSE)
  config('db_trace', FALSE)
  config('vocabulary_schema', NA)

  race <- read_codeset('valueset_race')
  eth <- read_codeset('valueset_race')
  tbl <- race %>% dplyr::union(eth)

  DBI::dbWriteTable(conn, 'concept', tbl, append = TRUE)

  expect_no_error(check_vs(vs_tbl = vs_test,
                           omop_or_pcornet = 'omop'))

})


test_that('process_vs local', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))
  config('cache_enabled', FALSE)
  config('results_name_tag', '')
  config('retain_intermediates', FALSE)
  config('db_trace', FALSE)
  config('vocabulary_schema', NA)

  race <- read_codeset('valueset_race')
  eth <- read_codeset('valueset_race')
  tbl <- race %>% dplyr::union(eth)

  DBI::dbWriteTable(conn, 'concept', tbl, append = TRUE)

  vs_opt <- check_vs(vs_tbl = vs_test,
                     omop_or_pcornet = 'omop')

  expect_no_error(process_vs(vs_results = vs_opt,
                             rslt_source = 'local'))

})


test_that('process_vs remote', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))
  config('cache_enabled', FALSE)
  config('results_name_tag', '')
  config('retain_intermediates', FALSE)
  config('db_trace', FALSE)
  config('vocabulary_schema', NA)

  race <- read_codeset('valueset_race')
  eth <- read_codeset('valueset_race')
  tbl <- race %>% dplyr::union(eth)

  DBI::dbWriteTable(conn, 'concept', tbl, append = TRUE)

  vs_opt <- check_vs(vs_tbl = vs_test,
                     omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'vs_output', vs_opt)

  expect_no_error(process_vs(vs_results = 'vs_output',
                             rslt_source = 'remote'))

})
