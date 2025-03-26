
dc_test <- dplyr::tibble(schema_current = c('cdm', 'cdm'),
                         table_current = c('person', 'visit_occurrence'),
                         schema_prev = c('cdm', 'cdm'),
                         table_prev = c('person', 'visit_occurrence'),
                         filter_logic = c(NA, 'visit_concept_id == 9201'),
                         check_domain = c('person', 'visit_ip'),
                         check_id = c('pd', 'vip'),
                         check_description = c('all patients', 'inpatient visits'))


test_that('omop_or_pcornet limited inputs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)

  expect_error(check_dc(dc_tbl = dc_test,
                        omop_or_pcornet = 'test',
                        prev_db_string = 'v55',
                        current_db_string = 'v56',
                        prev_ct_src = 'cdm',
                        prev_rslt_tbl = 'dc_output',
                        prev_rslt_schema = 'dqa_rox',
                        prev_db = config('db_src'),
                        check_string = 'dc'))

})

test_that('check_dc cdm', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('results_tag', '')

  expect_no_error(check_dc(dc_tbl = dc_test,
                           omop_or_pcornet = 'omop',
                           prev_db_string = 'v55',
                           current_db_string = 'v56',
                           prev_ct_src = 'cdm',
                           prev_rslt_tbl = 'dc_output',
                           prev_rslt_schema = 'dqa_rox',
                           prev_db = config('db_src'),
                           check_string = 'dc'))

})

test_that('check_dc result', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)

  db_sample <- dplyr::tibble('site' = c('test', 'test'),
                             'total_ct' = c(500, 1200),
                             'total_pt_ct' = c(9, 15),
                             'database_version' = c('v55', 'v55'),
                             'domain' = c('person', 'visit_ip'),
                             'check_name' = c('dc_pd', 'dc_vip'),
                             'check_type' = c('dc', 'dc'))

  DBI::dbWriteTable(conn, 'dc_output', db_sample)

  expect_no_error(check_dc(dc_tbl = dc_test,
                           omop_or_pcornet = 'omop',
                           prev_db_string = 'v55',
                           current_db_string = 'v56',
                           prev_ct_src = 'result',
                           prev_rslt_tbl = 'dc_output',
                           prev_rslt_schema = NA,
                           prev_db = config('db_src'),
                           check_string = 'dc'))

})

test_that('rslt_source limited inputs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)


  dc_opt <- check_dc(dc_tbl = dc_test,
                     omop_or_pcornet = 'omop',
                     prev_db_string = 'v55',
                     current_db_string = 'v56',
                     prev_ct_src = 'cdm',
                     prev_rslt_tbl = 'dc_output',
                     prev_rslt_schema = NA,
                     prev_db = config('db_src'),
                     check_string = 'dc')

  expect_error(process_dc(dc_ct_results = dc_opt$dc_cts,
                          dc_meta_results = dc_opt$dc_meta,
                          rslt_source = 'test'))


})

test_that('process_dc local', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)


  dc_opt <- check_dc(dc_tbl = dc_test,
                     omop_or_pcornet = 'omop',
                     prev_db_string = 'v55',
                     current_db_string = 'v56',
                     prev_ct_src = 'cdm',
                     prev_rslt_tbl = 'dc_output',
                     prev_rslt_schema = NA,
                     prev_db = config('db_src'),
                     check_string = 'dc')

  expect_no_error(process_dc(dc_ct_results = dc_opt$dc_cts,
                             dc_meta_results = dc_opt$dc_meta,
                             rslt_source = 'local'))


})


test_that('process_dc remote', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('results_name_tag', '')


  dc_opt <- check_dc(dc_tbl = dc_test,
                     omop_or_pcornet = 'omop',
                     prev_db_string = 'v55',
                     current_db_string = 'v56',
                     prev_ct_src = 'cdm',
                     prev_rslt_tbl = 'dc_output',
                     prev_rslt_schema = NA,
                     prev_db = config('db_src'),
                     check_string = 'dc')

  DBI::dbWriteTable(conn, 'dc_output', dc_opt$dc_cts, overwrite = TRUE)
  DBI::dbWriteTable(conn, 'dc_meta', dc_opt$dc_meta)


  expect_no_error(process_dc(dc_ct_results = 'dc_output',
                             dc_meta_results = 'dc_meta',
                             rslt_source = 'remote'))


})
