
#' Valueset Conformance
#'
#' This function will intake a limited valueset that is expected to make up the
#' entire contents of a field (minus the specified `null_values`) and identify
#' if any non-permitted values exist in the field (and how often).
#'
#' @param vs_tbl a table with the table, field, and valueset information for each
#'               check
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `vs`
#' @param concept_tbl a vocabulary table, like the OMOP concept table, with at least the concept column of interest (concept_id or concept_code),
#'                    the concept name, and the vocabulary id
#' @param null_values a vector of NULL values (or other values that are not part of the valueset but are broadly accepted)
#'                    that should be excluded when identifying non-valueset concepts
#'
#' @return a dataframe with summary information about each value that does not
#'         appear in the valueset, or a row with dummy information if no violations are identified
#'
#' @importFrom rlang set_names
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
check_vs <- function(vs_tbl,
                     omop_or_pcornet = 'omop',
                     check_string = 'vs',
                     concept_tbl = vocabulary_tbl('concept'),
                     null_values = c(44814650L,0L,44814653L,44814649L)) {

  site_nm <- config('qry_site')

  valuesets <- split(vs_tbl, seq(nrow(vs_tbl)))

  check_valueset <- list()

  for(i in 1:length(valuesets)) {

    cli::cli_inform(paste0('Starting ', valuesets[[i]]$valueset_name))

    codeset_round <- load_codeset(valuesets[[i]]$valueset_name, col_types = 'icccc')

    concept_id_fn <- valuesets[[i]]$concept_field

    if(tolower(omop_or_pcornet) == 'omop'){
      join_cols <- set_names('concept_id', paste0(concept_id_fn))
      pt_col <- 'person_id'
      concept_col <- 'concept_id'
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      join_cols <- set_names('concept_code', paste0(concept_id_fn))
      join_cols2 <- set_names('vocabulary_id', valuesets[[i]]$vocabulary_field)
      join_cols <- join_cols %>% append(join_cols2)
      pt_col <- 'patid'
      concept_col <- 'concept_code'
    }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

    if(!is.na(valuesets[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = valuesets[[i]]$schema,
                             table = valuesets[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(valuesets[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = valuesets[[i]]$schema,
                             table = valuesets[[i]]$table,
                             db = config('db_src'))
    }

    total_rows <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(!!sym(pt_col))) %>% collect() %>%
      add_meta(check_lib = check_string)

    if(!is.null(concept_tbl)){
      illegal_values <-
        tbl_use %>%
        add_site() %>% filter(site == site_nm) %>%
        anti_join(codeset_round,
                  by = join_cols) %>%
        filter(! .data[[concept_id_fn]] %in% null_values) %>%
        group_by(!!! rlang::syms(concept_id_fn)) %>%
        summarise(total_viol_ct = n(),
                  total_viol_pt_ct = n_distinct(!!sym(pt_col))) %>%
        ungroup() %>%
        inner_join(select(
          concept_tbl,
          !!sym(concept_col), concept_name, vocabulary_id
        ), by = join_cols) %>% collect()
    }else{
      illegal_values <-
        tbl_use %>%
        add_site() %>% filter(site == site_nm) %>%
        anti_join(codeset_round,
                  by = join_cols) %>%
        filter(! .data[[concept_id_fn]] %in% null_values) %>%
        group_by(!!! rlang::syms(concept_id_fn)) %>%
        summarise(total_viol_ct = n(),
                  total_viol_pt_ct = n_distinct(!!sym(pt_col))) %>%
        ungroup() %>% collect()

      if(nrow(illegal_values) > 0){
        illegal_values <- illegal_values %>%
          mutate(concept_name = 'No vocabulary table',
                 vocabulary_id = 'No vocabulary table')
      }
    }


    if(nrow(illegal_values) > 0){

      illegal_final <- illegal_values %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', valuesets[[i]]$check_id),
               table_application = valuesets[[i]]$table,
               accepted_value = FALSE) %>%
        left_join(total_rows)

    }else{

      illegal_final <- total_rows %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', valuesets[[i]]$check_id),
               table_application = valuesets[[i]]$table,
               accepted_value = TRUE,
               measurement_column = -999,
               total_viol_ct = 0,
               total_viol_pt_ct = 0,
               concept_name = 'No violations',
               vocabulary_id = 'PEDSnet',
               accepted_value = TRUE) %>%
        relocate(measurement_column) %>%
        rename_with(~valuesets[[i]]$concept_field, measurement_column)

    }

    check_valueset[[valuesets[[i]]$check_id]] <- illegal_final

  }

  check_valueset

  check_valueset[!sapply(check_valueset, function(x) all(is.na(x)))]

  final_vals <- create_vc_vs_output(tbl_list = check_valueset,
                                    check_string = check_string)

  final_vals_red <- purrr::reduce(.x = final_vals,
                                  .f = dplyr::union) %>%
    add_meta(check_lib = check_string)

  return(final_vals_red)

}

#' Valueset Conformance -- Processing
#'
#' Intakes the output of check_vs in order to apply additional processing. This
#' includes computing row and patient proportions and computing overall totals
#' across all sites included in the input.
#'
#' @param vs_results the output of check_vs
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return a list that contains two dataframes:
#' - `vs_processed`: a dataframe with additional columns that include proportions of violations
#'                   and the overall summary
#' - `vs_violations`: a dataframe with ONLY violating values that do not appear in the valueset
#'
#' @export
#'
process_vs <-function(vs_results,
                      rslt_source = 'remote',
                      csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    vs_int <- results_tbl(vs_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    vs_int <- readr::read_csv(paste0(csv_rslt_path, vs_results))
  }else if(tolower(rslt_source) == 'local'){
    vs_int <- vs_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  vs_process <- vs_int %>%
    mutate(prop_total_viol=as.numeric(total_viol_ct/total_denom_ct),
           prop_total_pt_viol=as.numeric(total_viol_pt_ct/total_pt_ct)) %>%
    group_by(site, table_application, measurement_column, vocabulary_id, check_type,
             check_name, total_denom_ct, accepted_value) %>%
    summarise(tot_ct = sum(total_viol_ct),
              tot_prop = sum(prop_total_viol)) %>%
    ungroup()

  vs_violations <- vs_process %>%
    filter(!accepted_value)%>%
    group_by(site, table_application, measurement_column, check_type, check_name, total_denom_ct) %>%
    summarise(tot_ct=sum(tot_ct),
              tot_prop=sum(tot_prop))%>%
    ungroup()%>%
    mutate(check_name_app=paste0(check_name, "_rows"))

  opt <- list('vs_processed' = vs_process,
              'vs_violations' = vs_violations)

  return(opt)
}
