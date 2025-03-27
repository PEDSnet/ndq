
fot_test <- tibble('check_id' = c('ip visits'),
                   'check_description' = c('inpatient visits'),
                   'schema' = c('cdm'),
                   'table' = c('visit_occurrence'),
                   'date_field' = c('visit_start_date'),
                   'filter_logic' = c('visit_concept_id == 9201'))

test_that('test erroring', {

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

  expect_error(check_fot(fot_tbl = fot_test,
                         omop_or_pcornet = 'test'))
  expect_error(check_fot(fot_tbl = fot_test,
                         omop_or_pcornet = 'omop',
                         compute_method = 'test'))
  expect_error(check_fot(fot_tbl = fot_test,
                         omop_or_pcornet = 'omop',
                         compute_method = 'group'))
})


test_that('check_fot month', {

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

  expect_no_error(check_fot(fot_tbl = fot_test,
                            omop_or_pcornet = 'omop',
                            compute_method = 'loop',
                            time_span = c('2024-01-01', '2024-12-01'),
                            time_period = 'month',
                            distinct_visits = TRUE,
                            visits_only = FALSE))

  expect_no_error(check_fot(fot_tbl = fot_test,
                            omop_or_pcornet = 'omop',
                            compute_method = 'loop',
                            time_span = c('2024-01-01', '2024-12-01'),
                            time_period = 'month',
                            distinct_visits = TRUE,
                            visits_only = TRUE))

  expect_no_error(check_fot(fot_tbl = fot_test,
                            omop_or_pcornet = 'omop',
                            compute_method = 'loop',
                            time_span = c('2024-01-01', '2024-12-01'),
                            time_period = 'month',
                            distinct_visits = FALSE,
                            visits_only = TRUE))

  expect_no_error(check_fot(fot_tbl = fot_test,
                            omop_or_pcornet = 'omop',
                            compute_method = 'loop',
                            time_span = c('2024-01-01', '2024-12-01'),
                            time_period = 'month',
                            distinct_visits = FALSE,
                            visits_only = FALSE))

})


test_that('check_fot year', {

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

  expect_no_error(check_fot(fot_tbl = fot_test %>% mutate(filter_logic = NA),
                            omop_or_pcornet = 'omop',
                            compute_method = 'loop',
                            time_span = c('2022-01-01', '2024-01-01'),
                            time_period = 'year',
                            lookback_weeks = 1,
                            distinct_visits = TRUE,
                            visits_only = FALSE))

})


test_that('check_fot month', {

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

  fot_opt <- check_fot(fot_tbl = fot_test,
                       omop_or_pcornet = 'omop',
                       compute_method = 'loop',
                       time_span = c('2012-01-01', '2012-12-01'),
                       time_period = 'month',
                       distinct_visits = TRUE,
                       visits_only = FALSE)

  DBI::dbWriteTable(conn, 'fot_output', fot_opt)

  fot_totals <- fot_opt %>%
    rename('total_pt' = 'row_pts',
           'total_row' = 'row_cts',
           'total_visit' = 'row_visits')

  expect_no_error(process_fot(fot_results = fot_opt,
                              add_ratios = FALSE,
                              rslt_source = 'local'))
  expect_no_error(process_fot(fot_results = 'fot_output',
                              add_ratios = FALSE,
                              rslt_source = 'remote'))
  expect_no_error(process_fot(fot_results = fot_opt %>% left_join(fot_totals),
                              add_ratios = TRUE,
                              rslt_source = 'local'))

})
