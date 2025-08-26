
test_that('output_tbl_append works', {

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
  config('results_target', TRUE)

  ecp_rslt <- tibble('total_pt_ct' = c(100, 200),
                     'concept_pt_ct' = c(50, 12),
                     'concept_group' = c('test1', 'test2'),
                     'prop_w_concept' = c(.1, .01),
                     'site' = c('a', 'a'),
                     'database_version' = c('1', '1'),
                     'check_type' = c('ecp', 'ecp'),
                     'check_name' = c('ecp_test1', 'ecp_test2'),
                     'cohort_denom' = c('mycohort', 'mycohort'))

  expect_no_error(output_tbl_append(ecp_rslt, 'ecp_rslt'))
  expect_no_error(output_tbl_append(ecp_rslt, 'ecp_rslt'))

})


test_that('create_check_metadata works', {

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

  ecp_rslt <- tibble('total_pt_ct' = c(100, 200),
                     'concept_pt_ct' = c(50, 12),
                     'concept_group' = c('test1', 'test2'),
                     'prop_w_concept' = c(.1, .01),
                     'site' = c('a', 'a'),
                     'database_version' = c('1', '1'),
                     'check_type' = c('ecp', 'ecp'),
                     'check_name' = c('ecp_test1', 'ecp_test2'),
                     'cohort_denom' = c('mycohort', 'mycohort'))

  expect_no_error(create_check_metadata(check_tbls = list(ecp_rslt),
                                        rslt_source = 'local'))

})


test_that('anonymization', {

  ecp_rslt <- tibble('total_pt_ct' = c(100, 200),
                     'concept_pt_ct' = c(50, 12),
                     'concept_group' = c('test1', 'test2'),
                     'prop_w_concept' = c(.1, .01),
                     'site' = c('a', 'b'),
                     'database_version' = c('1', '1'),
                     'check_type' = c('ecp', 'ecp'),
                     'check_name' = c('ecp_test1', 'ecp_test2'),
                     'cohort_denom' = c('mycohort', 'mycohort'))

  ecp_rslt2 <- tibble('total_pt_ct' = c(100, 200),
                     'concept_pt_ct' = c(50, 12),
                     'concept_group' = c('test1', 'test2'),
                     'prop_w_concept' = c(.1, .01),
                     'site' = c('a', 'b'),
                     'site_anon' = c('1', '2'),
                     'sitenum' = c(1,2),
                     'database_version' = c('1', '1'),
                     'check_type' = c('ecp', 'ecp'),
                     'check_name' = c('ecp_test1', 'ecp_test2'),
                     'cohort_denom' = c('mycohort', 'mycohort'))

  expect_no_error(attach_anon_id(all_sites_tbl = ecp_rslt,
                                 tbls_to_anon = list('t1' = ecp_rslt,
                                                     't2' = ecp_rslt2)))

})


test_that('large n', {

  ecp_rslt <- tibble('total_pt_ct' = c(100, 200),
                     'concept_pt_ct' = c(50, 12),
                     'concept_group' = c('test1', 'test2'),
                     'prop_with_concept' = c(.1, .01),
                     'site' = c('a', 'b'),
                     'database_version' = c('1', '1'),
                     'check_type' = c('ecp', 'ecp'),
                     'check_name' = c('ecp_test1', 'ecp_test2'),
                     'cohort_denom' = c('mycohort', 'mycohort'))

  ecp_anon <- attach_anon_id(all_sites_tbl = ecp_rslt,
                             tbls_to_anon = list('t1' = ecp_rslt))

  expect_no_error(summarize_large_n(dq_output = ecp_anon$t1,
                                    check_string = 'ecp',
                                    num_col = 'prop_with_concept',
                                    grp_vars = c('concept_group', 'check_name'),
                                    shape = 'wide'))

  expect_no_error(summarize_large_n(dq_output = ecp_anon$t1,
                                    check_string = 'ecp',
                                    num_col = 'prop_with_concept',
                                    grp_vars = c('concept_group', 'check_name'),
                                    time = TRUE,
                                    shape = 'wide'))

  vc_rslt <- tibble('vocabulary_id' = c('SNOMED', 'ICD10'),
                    'table_application' = c('condition_occurrence', 'procedure_occurrence'),
                    'measurement_column' = c('condition_concept_id', 'procedure_concept_id'),
                    'concepts' = c(0,0),
                    'total_viol_ct' = c(10, 15),
                    'total_viol_pt_ct' = c(1, 3),
                    'total_viol_concept_ct' = c(1,2),
                    'site' = c('a', 'a'),
                    'database_version' = c('1', '1'),
                    'check_name' = c('vc_conds', 'vc_procs'),
                    'check_type' = c('vc', 'vc'),
                    'accepted_value' = c(FALSE, FALSE),
                    'concept_name' = c('SNOMED', 'ICD10'),
                    'total_denom_ct' = c(100, 100),
                    'prop_viol' = c(0.1,0.1),
                    'tot_ct' = c(500, 500),
                    'total_concept_ct' = c(12, 14))

  vc_anon <- attach_anon_id(all_sites_tbl = vc_rslt,
                             tbls_to_anon = list('t1' = vc_rslt))

  expect_no_error(summarize_large_n(dq_output = vc_anon$t1,
                                    check_string = 'vc',
                                    num_col = 'prop_viol',
                                    grp_vars = c('table_application', 'measurement_column',
                                                 'check_type', 'check_name'),
                                    shape = 'wide'))
  expect_no_error(summarize_large_n(dq_output = vc_anon$t1,
                                    check_string = 'vc',
                                    num_col = 'prop_viol',
                                    grp_vars = c('table_application', 'measurement_column',
                                                 'check_type', 'check_name'),
                                    shape = 'long'))

})
