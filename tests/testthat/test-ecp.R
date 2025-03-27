
ecp_test <- tibble('check_id' = c('hypertension'),
                   'cohort_definition' = c('all patients'),
                   'cohort_schema' = c('cdm'),
                   'cohort_table' = c('person'),
                   'schema' = c('cdm'),
                   'table' = c('condition_occurrence'),
                   'concept_field' = c('condition_concept_id'),
                   'conceptset_name' = c('synthea_hypertension'),
                   'filter_logic' = c('condition_concept_id == 320128'))

test_that('omop_or_pcornet limited inputs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)
  config('results_name_tag', '')
  config('current_version', '1')
  config('retain_intermediates', FALSE)

  expect_error(check_ecp(ecp_tbl = ecp_test,
                         omop_or_pcornet = 'test'))

})


test_that('check_ecp', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)
  config('results_name_tag', '')
  config('current_version', '1')
  config('retain_intermediates', FALSE)
  config('cache_enabled', FALSE)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))

  expect_no_error(check_ecp(ecp_tbl = ecp_test,
                            omop_or_pcornet = 'omop'))
  expect_no_error(check_ecp(ecp_tbl = ecp_test %>% dplyr::mutate(filter_logic = NA),
                            omop_or_pcornet = 'omop'))

})


test_that('process_ecp local', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)
  config('results_name_tag', '')
  config('current_version', '1')
  config('retain_intermediates', FALSE)
  config('cache_enabled', FALSE)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))

  ecp_opt <- check_ecp(ecp_tbl = ecp_test,
                       omop_or_pcornet = 'omop')

  expect_no_error(process_ecp(ecp_results = ecp_opt,
                              rslt_source = 'local'))

})


test_that('process_ecp remote', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)
  config('results_name_tag', '')
  config('current_version', '1')
  config('retain_intermediates', FALSE)
  config('cache_enabled', FALSE)
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))

  ecp_opt <- check_ecp(ecp_tbl = ecp_test,
                       omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'ecp_output', ecp_opt)

  expect_no_error(process_ecp(ecp_results = 'ecp_output',
                              rslt_source = 'remote'))

})
