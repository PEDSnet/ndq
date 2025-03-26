
vc_test <- tibble('check_id' = c('conditions'),
                  'schema' = c('cdm'),
                  'table' = c('condition_occurrence'),
                  'acceptable_vocabularies' = c("SNOMED, OMOP Extension"),
                  'concept_field' = c('condition_concept_id'),
                  'filter_logic' = c(NA))

test_that('omop_or_pcornet limited inputs', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)

  expect_error(check_vc(vc_tbl = vc_test,
                        omop_or_pcornet = 'test'))

})

test_that('check_vc', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)

  expect_no_error(check_vc(vc_tbl = vc_test,
                           omop_or_pcornet = 'omop'))

})


test_that('process_vc local', {

  conn <- mk_testdb_omop()

  new_argos <- argos$new()
  set_argos_default(new_argos)
  config('db_src', conn)
  config('cdm_schema', NA)
  config('qry_site', 'test')
  config('results_schema', NA)
  config('vocabulary_schema', NA)
  config('db_trace', FALSE)

  vc_opt <- check_vc(vc_tbl = vc_test,
                     omop_or_pcornet = 'omop')

  expect_no_error(process_vc(vc_results = vc_opt,
                             rslt_source = 'local'))

})

test_that('process_vc remote', {

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

  vc_opt <- check_vc(vc_tbl = vc_test,
                     omop_or_pcornet = 'omop')

  DBI::dbWriteTable(conn, 'vc_output', vc_opt)

  expect_no_error(process_vc(vc_results = 'vc_output',
                             rslt_source = 'remote'))

})
