
uc_test <- tibble('check_id' = c('co', 'vo'),
                  'check_description' = c('inpatient conditions', 'visits'),
                  'schema' = c('cdm', 'cdm'),
                  'table' = c('condition_occurrence', 'visit_occurrence'),
                  'concept_field' = c('condition_concept_id', 'visit_concept_id'),
                  'source_value_field' = c('condition_source_value', 'visit_source_value'),
                  'date_field' = c('condition_start_date', 'visit_start_date'),
                  'filter_logic' = c(NA, 'visit_concept_id == 9202'))

test_that('check_uc no time, no list', {

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

  expect_no_error(check_uc(uc_tbl = uc_test,
                           produce_mapped_list = FALSE,
                           by_year = FALSE))

})

# test_that('check_uc no time, with list', {
#
#   conn <- mk_testdb_omop()
#
#   new_argos <- argos$new()
#   set_argos_default(new_argos)
#   config('db_src', conn)
#   config('cdm_schema', NA)
#   config('qry_site', 'test')
#   config('results_schema', NA)
#   config('vocabulary_schema', NA)
#   config('db_trace', FALSE)
#   config('results_name_tag', '')
#   config('results_target', TRUE)
#
#   expect_no_error(check_uc(uc_tbl = uc_test,
#                            produce_mapped_list = TRUE,
#                            by_year = FALSE))
#
# })

test_that('check_uc time', {

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
  config('results_target', TRUE)

  expect_no_error(check_uc(uc_tbl = uc_test,
                           produce_mapped_list = FALSE,
                           by_year = TRUE))

})


test_that('process_uc no time, local', {

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

  uc_opt <- check_uc(uc_tbl = uc_test,
                     produce_mapped_list = FALSE,
                     by_year = FALSE)

  expect_no_error(process_uc(uc_results = uc_opt,
                             rslt_source = 'local'))

})


test_that('process_uc time, remote', {

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

  uc_opt <- check_uc(uc_tbl = uc_test,
                     produce_mapped_list = FALSE,
                     by_year = TRUE)

  DBI::dbWriteTable(conn, 'uc_output', uc_opt)

  expect_no_error(process_uc(uc_results = 'uc_output',
                             rslt_source = 'remote'))

})
