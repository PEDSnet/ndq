
mf_test <- tibble('check_id' = c('co'),
                  'check_description' = c('inpatient conditions'),
                  'schema' = c('cdm'),
                  'table' = c('condition_occurrence'),
                  'filter_logic' = c(NA))


test_that('omop_or_pcornet limited input', {

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

  expect_error(check_mf_visitid(mf_tbl = mf_test,
                                omop_or_pcornet = 'test'))

})

test_that('check_mf_visitid', {

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

  expect_no_error(check_mf_visitid(mf_tbl = mf_test,
                                   omop_or_pcornet = 'omop'))

})

test_that('process_mf_visitid remote', {

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

  mf_opt <- check_mf_visitid(mf_tbl = mf_test,
                             omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'mf_output', mf_opt)

  expect_no_error(process_mf_visitid('mf_output'))

})

test_that('process_mf_visitid local', {

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

  mf_opt <- check_mf_visitid(mf_tbl = mf_test,
                             omop_or_pcornet = 'omop')

  expect_no_error(process_mf_visitid(mf_opt, rslt_source = 'local'))

})
