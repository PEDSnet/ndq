
#' Expected Concepts Present
#'
#' This function will iterate through the provided input table to identify
#' the count of patients who have the at least one occurrence of the concept defined in the
#' user-provided concept set and the proportion of patients who have the concept
#' based on the user-defined denominator cohort.
#'
#' @param ecp_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the concepts
#'  that should be identified and the denominator cohort to be used for the computation.
#'  see `?ecp_input_omop` or `?ecp_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param check_string *string* || defaults to `ecp`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @return
#'
#'  This function will return a table with the total patient count, the count of
#'  patients with a particular concept, the proportion of total patients with the
#'  concept, plus some additional descriptive metadata
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::ecp_input_omop
#' ndq::ecp_input_pcornet
#'
#' # Use this as your input to the ECP function
#' ## To execute the check at the patient level:
#' \dontrun{
#' my_ecp_rslt <- check_ecp(ecp_tbl = ndq::ecp_input_omop,
#'                          omop_or_pcornet = 'omop',
#'                          check_string = 'ecp')
#' }
#'
check_ecp <- function(ecp_tbl,
                      omop_or_pcornet = 'omop',
                      check_string = 'ecp'){

  site_nm <- config('qry_site')

  ecp_list <- split(ecp_tbl, seq(nrow(ecp_tbl)))

  result <- list()

  for(i in 1:length(ecp_list)){

    concept_group <- ecp_list[[i]]$conceptset_name

    message(paste0('Starting ', concept_group))

    if(tolower(omop_or_pcornet) == 'omop'){
      pt_col <- 'person_id'
      pt_tbl <- 'person'
      join_cols <- set_names('concept_id', ecp_list[[i]]$concept_field)
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      pt_col <- 'patid'
      pt_tbl <- 'demographic'
      join_cols <- set_names('concept_code', ecp_list[[i]]$concept_field)
      if(!is.na(ecp_list[[i]]$vocabulary_field)){
        join_cols2 <- set_names('vocabulary_id', ecp_list[[i]]$vocabulary_field)
        join_cols <- join_cols %>% append(join_cols2)
      }
    }

    cohort_tbl <- pick_schema(schema = ecp_list[[i]]$cohort_schema,
                              table = ecp_list[[i]]$cohort_table,
                              db = config('db_src')) %>%
      select(!!sym(pt_col))

    total_pts <- cohort_tbl %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      summarise(total_pt_ct = n_distinct(!!sym(pt_col))) %>%
      collect()


    if(!is.na(ecp_list[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = ecp_list[[i]]$schema,
                                table = ecp_list[[i]]$table,
                                db = config('db_src')) %>%
        filter(!! rlang::parse_expr(ecp_list[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = ecp_list[[i]]$schema,
                                table = ecp_list[[i]]$table,
                                db = config('db_src'))
    }

    fact_pts <- tbl_use %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      inner_join(cohort_tbl) %>%
      inner_join(load_codeset(ecp_list[[i]]$conceptset_name,
                              indexes = NULL), by = join_cols) %>%
      summarise(concept_pt_ct = n_distinct(!!sym(pt_col))) %>% collect()

    pt_cohort <- ecp_list[[i]]$cohort_definition
    if(length(pt_cohort) > 0){pt_cohort <- pt_cohort}else{pt_cohort <- 'placeholder'}

    final_tbl <- total_pts %>%
      mutate(concept_pt_ct = fact_pts$concept_pt_ct,
             concept_group = concept_group,
             prop_with_concept = as.numeric(concept_pt_ct/total_pt_ct),
             check_name = paste0(check_string, '_', ecp_list[[i]]$check_id),
             cohort_denominator = pt_cohort) %>%
      add_meta(check_lib = 'ecp')


    result[[paste0(ecp_list[[i]]$check_id)]] <- final_tbl
  }

  compress <- reduce(.x = result,
                     .f = dplyr::union)

  return(compress)

}



#' Expected Concepts Present -- Processing
#'
#' Intakes the output of `check_ecp` in order to apply additional processing. This
#' includes creating a new `check_name_app` column to specify that the check
#' was computed at the person level.
#'
#' @param ecp_results *tabular input* || **required**
#'
#'  The tabular output of `check_ecp`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `ecp_results` table.
#'  Acceptable values are
#'  - `local` - table is stored as a dataframe in the local R environment
#'  - `csv` - table is stored as a CSV file
#'  - `remote` - table is stored on a remote database
#'
#' @param csv_rslt_path *string* || defaults to `NULL`
#'
#'  If `rslt_source` has been set to `csv`, this parameter should indicate the path to
#'  the result file(s). Otherwise, this parameter can be left as `NULL`
#'
#' @returns
#'
#'  This function will return the `ecp_results` table with and additional
#'  `check_name_app` column to indicate application level
#'
#' @export
#'
#' @examples
#' # This function should be run after check_ecp has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_ecp_process <- process_ecp(ecp_results = my_ecp_rslts,
#'                               rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_ecp_process <- process_ecp(ecp_results = 'my_ecp_rslts',
#'                               rslt_source = 'csv',
#'                               csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_ecp_process <- process_ecp(ecp_results = 'my_ecp_rslts',
#'                               rslt_source = 'remote')
#' }
#'
process_ecp <- function(ecp_results,
                        rslt_source = 'remote',
                        csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    ecp_int <- results_tbl(ecp_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    ecp_int <- readr::read_csv(paste0(csv_rslt_path, ecp_results))
  }else if(tolower(rslt_source) == 'local'){
    ecp_int <- ecp_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  ecp_final <- ecp_int %>%
    mutate(check_name_app = paste0(check_name, '_person'))

  return(ecp_final)
}

