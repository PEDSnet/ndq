
dp_input <- tibble('check_id' = 'co',
                   'check_description' = 'all conditions',
                   'schema' = 'cdm',
                   'table' = 'condition_occurrence',
                   'date_field' = 'condition_start_date',
                   'filter_logic' = NA)

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

  expect_error(check_dp(dp_tbl = dp_input,
                        omop_or_pcornet = 'test'))

})


test_that('check_dp', {

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

  expect_no_error(check_dp(dp_tbl = dp_input,
                           omop_or_pcornet = 'omop'))
  expect_no_error(check_dp(dp_tbl = dp_input %>% dplyr::mutate(filter_logic = 'condition_concept_id == 4090111'),
                           omop_or_pcornet = 'omop'))

})


test_that('process_dp local', {

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

  dp_opt <- check_dp(dp_tbl = dp_input,
                     omop_or_pcornet = 'omop')

  expect_no_error(process_dp(dp_results = dp_opt,
                             rslt_source = 'local'))

})


test_that('process_dp remote', {

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

  dp_opt <- check_dp(dp_tbl = dp_input,
                     omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'dp_output', dp_opt)

  expect_no_error(process_dp(dp_results = 'dp_output',
                             rslt_source = 'remote'))

})
