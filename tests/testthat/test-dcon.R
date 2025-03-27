
dcon_test <- tibble('cohort_id' = c('htn_dx', 'op_visit'),
                    'check_id' = c('htn_dx_op_visit', 'htn_dx_op_visit'),
                    'check_description' = c('hypertension diagnosis + outpatient visit',
                                            'hypertension diagnosis + outpatient visit'),
                    'schema' = c('cdm', 'cdm'),
                    'table' = c('condition_occurrence', 'visit_occurrence'),
                    'date_field' = c('condition_start_date', 'visit_start_date'),
                    'concept_field' = c('condition_concept_id', 'visit_concept_id'),
                    'conceptset_name' = c('synthea_hypertension', NA),
                    'filter_logic' = c(NA, 'visit_concept_id == 9202'),
                    'time_between_events' = c(30,30))

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

  expect_error(check_dcon(dcon_tbl = dcon_test,
                          omop_or_pcornet = 'omop'))

})

test_that('time_between_events match', {

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

  expect_error(check_dcon(dcon_tbl = dcon_test %>%
                            dplyr::mutate(time_between_events = ifelse(cohort_id == 'htn_dx', 15, 30)),
                          omop_or_pcornet = 'omop'))

})


test_that('check_dcon', {

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
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))
  config('cache_enabled', FALSE)

  check_dcon(dcon_tbl = dcon_test,
             omop_or_pcornet = 'omop')

  dcon_test2 <- tibble('cohort_id' = c('htn_dx', 'op_visit'),
                      'check_id' = c('htn_dx_op_visit', 'htn_dx_op_visit'),
                      'check_description' = c('hypertension diagnosis + outpatient visit',
                                              'hypertension diagnosis + outpatient visit'),
                      'schema' = c('cdm', 'cdm'),
                      'table' = c('condition_occurrence', 'visit_occurrence'),
                      'date_field' = c('condition_start_date', 'visit_start_date'),
                      'concept_field' = c('condition_concept_id', 'visit_concept_id'),
                      'conceptset_name' = c(NA, 'synthea_outpatient'),
                      'filter_logic' = c('person_id == 2', NA),
                      'time_between_events' = c(NA, NA))

  expect_no_error(check_dcon(dcon_tbl = dcon_test,
                             omop_or_pcornet = 'omop'))
  expect_no_error(check_dcon(dcon_tbl = dcon_test2,
                             omop_or_pcornet = 'omop'))
  expect_no_error(check_dcon(dcon_tbl = dcon_test,
                             compute_level = 'visit',
                             omop_or_pcornet = 'omop'))

})


test_that('process_dcon', {

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
  config('base_dir', getwd())
  config('subdirs', list('spec_dir' = 'testspecs'))
  config('cache_enabled', FALSE)

  dcon_opt <- check_dcon(dcon_tbl = dcon_test,
                          omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'dcon_output', dcon_opt)

  expect_no_error(process_dcon(dcon_results = dcon_opt,
                               rslt_source = 'local'))
  expect_no_error(process_dcon(dcon_results = 'dcon_output',
                               rslt_source = 'remote'))



})
