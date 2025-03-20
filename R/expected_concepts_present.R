
#' Check Expected Concept Presence
#'
#' This function will loop through the provided table to identify
#' the count of patients who have the concept identified in the list element and the proportion
#' of patients who have the concept based on the user-provided denominator table.
#'
#' @param ecp_tbl table with information regarding the variables and associated fact tables
#'                that should be evaluated
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `ecp`
#'
#' @return a table with the total patient count, the count of patients with a particular concept,
#'         the proportion of total patients with the concept, and relevant metadata
#'
check_ecp <- function(ecp_tbl,
                      omop_or_pcornet = 'omop',
                      check_string = 'ecp'){

  site_nm <- config('site')

  ecp_list <- split(ecp_tbl, seq(nrow(ecp_tbl)))

  result <- list()

  for(i in 1:length(ecp_list)){

    concept_group <- ecp_list[[i]]$conceptset_name

    message(paste0('Starting ', concept_group))

    if(tolower(omop_or_pcornet) == 'omop'){
      pt_col <- 'person_id'
      join_cols <- set_names('concept_id', ecp_list[[i]]$concept_field)
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      pt_col <- 'patid'
      join_cols <- set_names('concept_code', ecp_list[[i]]$concept_field)
      join_cols2 <- set_names('vocabulary_id', ecp_list[[i]]$vocabulary_field)
      join_cols <- join_cols %>% append(join_cols2)
    }

    cohort_tbl <- pick_schema(schema = ecp_list[[i]]$cohort_schema,
                              table = ecp_list[[i]]$cohort_table,
                              db = config('db_src'))

    total_pts <- cohort_tbl %>%
      add_site() %>% filter(site == site_nm) %>%
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
      add_site() %>% filter(site == site_nm) %>%
      inner_join(cohort_tbl) %>%
      inner_join(load_codeset(ecp_list[[i]]$conceptset_name), by = join_cols) %>%
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
#' @param ecp_results table output by check_ecp
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @returns same input table with additional check_name_app column to indicate application level
#'
#' @export
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

