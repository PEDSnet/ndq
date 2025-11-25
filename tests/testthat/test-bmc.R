
bmc_test <- tibble('check_id' = c('co'),
                   'check_description' = c('conditions'),
                   'schema' = c('cdm'),
                   'table' = c('condition_occurrence'),
                   'concept_field' = c('condition_concept_id'),
                   'concept_table_field' = c('concept_name'),
                   'filter_logic' = c(NA))

bmc_bnb <- tibble('check_name' = 'co',
                  'concept' = '4281516',
                  'best_notbest' = 1,
                  'default_to' = 'notbest')

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

  expect_error(check_bmc(bmc_tbl = bmc_test,
                         best_notbest_tbl = bmc_bnb,
                         omop_or_pcornet = 'test'))

})


test_that('check_bmc', {

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

  expect_no_error(check_bmc(bmc_tbl = bmc_test,
                            best_notbest_tbl = bmc_bnb,
                            omop_or_pcornet = 'omop'))
  expect_no_error(check_bmc(bmc_tbl = bmc_test %>% dplyr::mutate(filter_logic = 'condition_concept_id == 4281516'),
                            best_notbest_tbl = bmc_bnb,
                            omop_or_pcornet = 'omop'))

})


test_that('process_bmc local', {

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

  bmc_opt <- check_bmc(bmc_tbl = bmc_test,
                       best_notbest_tbl = bmc_bnb,
                       omop_or_pcornet = 'omop')

  expect_no_error(process_bmc(bmc_results = bmc_opt,
                              rslt_source = 'local'))

})


test_that('process_bmc local', {

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

  bmc_opt <- check_bmc(bmc_tbl = bmc_test,
                       best_notbest_tbl = bmc_bnb,
                       omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'bmc_output', bmc_opt)

  expect_no_error(process_bmc(bmc_results = 'bmc_output',
                              rslt_source = 'remote'))

})
