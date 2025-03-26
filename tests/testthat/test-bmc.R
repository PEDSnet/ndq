
bmc_test <- tibble('check_id' = c('co'),
                   'check_description' = c('conditions'),
                   'schema' = c('cdm'),
                   'table' = c('condition_occurrence'),
                   'concept_field' = c('condition_concept_id'),
                   'concept_table_field' = c('concept_name'),
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

  expect_error(check_bmc(bmc_tbl = bmc_test,
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
                       omop_or_pcornet = 'omop')

  bmc_opt$bmc_concepts <- bmc_opt$bmc_concepts %>% mutate(include = NA)

  expect_no_error(process_bmc(bmc_results = bmc_opt$bmc_counts,
                              bmc_concepts = bmc_opt$bmc_concepts,
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
                       omop_or_pcornet = 'omop')

  bmc_opt$bmc_concepts <- bmc_opt$bmc_concepts %>% mutate(include = NA)

  DBI::dbWriteTable(conn, 'bmc_output', bmc_opt$bmc_counts)
  DBI::dbWriteTable(conn, 'bmc_concepts', bmc_opt$bmc_concepts)

  expect_no_error(process_bmc(bmc_results = 'bmc_output',
                              bmc_concepts = 'bmc_concepts',
                              rslt_source = 'remote'))

})
