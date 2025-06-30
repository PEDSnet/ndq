

cfd_test <- tibble('check_id' = c('co'),
                  'check_description' = c('conditions'),
                  'schema' = c('cdm'),
                  'table' = c('condition_occurrence'),
                  'filter_logic' = c(NA))

cfd_visits <- tibble('visit_type' = 'outpatient',
                    'visit_concept_id' = 9202)

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

  expect_error(check_cfd(cfd_tbl = cfd_test,
                        visit_type_filter = c('all', 'outpatient'),
                        visit_type_tbl = cfd_visits,
                        omop_or_pcornet = 'test'))

})

test_that('check_cfd', {

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

  # print(check_cfd(cfd_tbl = cfd_test,
  #          visit_type_string = 'all',
  #          omop_or_pcornet = 'omop'))

  expect_no_error(check_cfd(cfd_tbl = cfd_test,
                           visit_type_filter = c('all', 'outpatient'),
                           visit_type_tbl = cfd_visits,
                           omop_or_pcornet = 'omop'))

  expect_no_error(check_cfd(cfd_tbl = cfd_test %>% mutate(filter_logic = 'person_id %in% c(1,2,3)'),
                           visit_type_filter = c('all', 'outpatient'),
                           visit_type_tbl = cfd_visits,
                           omop_or_pcornet = 'omop'))
})


test_that('check_cfd local', {

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

  cfd_opt <- check_cfd(cfd_tbl = cfd_test,
                     visit_type_filter = c('all', 'outpatient'),
                     visit_type_tbl = cfd_visits,
                     omop_or_pcornet = 'omop')

  expect_no_error(process_cfd(cfd_results = cfd_opt,
                             rslt_source = 'local'))

})

test_that('check_cfd remote', {

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

  cfd_opt <- check_cfd(cfd_tbl = cfd_test,
                     visit_type_filter = c('all', 'outpatient'),
                     visit_type_tbl = cfd_visits,
                     omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'cfd_output', cfd_opt)

  expect_no_error(process_cfd(cfd_results = 'cfd_output',
                             rslt_source = 'remote'))

})
