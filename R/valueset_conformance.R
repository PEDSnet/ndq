
#' Valueset Conformance
#'
#' This function will intake a limited valueset that is expected to make up the
#' entire contents of a field (minus the specified `null_values`) and identify
#' if any non-permitted values exist in the field (and how often).
#'
#' @param vs_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the fields and
#'  associated valuesets of interest.
#'  see `?vs_input_omop` or `?vs_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param concept_tbl *tabular input* || defaults to `NULL`
#'
#'  An optional parameter used to define a vocabulary table with concept definitions
#'  (for example, the OHDSI concept table). If left NULL, the concepts as they exist
#'  in the fact table will be returned to the user.
#'
#' @param null_values *string / vector* || defaults to 44814650, 0, 44814653, & 44814649
#'
#'  A string or vector listing the concept(s) that indicate a NULL value, which will be
#'  excluding when assessing for the presence of values not present in the valueset.
#'
#' @param check_string *string* || defaults to `vs`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @return
#'
#'  This function will return a dataframe with summary information about each value
#'  that appears in the data but does not comply with the valueset definition. If no violations are
#'  identified for a particular check, a placeholder row will dummy information will be inserted
#'  instead.
#'
#' @importFrom rlang set_names
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::vs_input_omop
#' ndq::vs_input_pcornet
#'
#' # Use this as your input to the VS function
#' \dontrun{
#' my_vs_rslt <- check_vs(vs_tbl = ndq::vs_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
#'                        null_values = c(44814650L,0L,
#'                                        44814653L,44814649L), # ignored illegal values
#'                        check_string = 'vs')
#' }
#'
check_vs <- function(vs_tbl,
                     omop_or_pcornet = 'omop',
                     concept_tbl = NULL,
                     null_values = c(44814650L,0L,44814653L,44814649L),
                     check_string = 'vs') {

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
      pt_tbl <- 'person'
      concept_col <- 'concept_id'
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      join_cols <- set_names('concept_code', paste0(concept_id_fn))
      if(!is.na(valuesets[[i]]$vocabulary_field)){
        join_cols2 <- set_names('vocabulary_id', valuesets[[i]]$vocabulary_field)
        join_cols <- join_cols %>% append(join_cols2)
      }
      pt_col <- 'patid'
      pt_tbl <- 'demographic'
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
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(!!sym(pt_col))) %>% collect() %>%
      add_meta(check_lib = check_string)

    if(!is.null(concept_tbl)){
      illegal_values <-
        tbl_use %>%
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = pt_col) %>% filter(site == site_nm) %>%
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
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = pt_col) %>% filter(site == site_nm) %>%
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
               check_description = valuesets[[i]]$check_description,
               table_application = valuesets[[i]]$table,
               accepted_value = FALSE) %>%
        left_join(total_rows)

    }else{

      illegal_final <- total_rows %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', valuesets[[i]]$check_id),
               check_description = valuesets[[i]]$check_description,
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
#' Intakes the output of `check_vs` in order to apply additional processing. This
#' includes computing row and patient proportions and computing overall totals
#' across all sites included in the input.
#'
#' @param vs_results *tabular input* || **required**
#'
#'  The tabular output of `check_vs`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `vs_results` table.
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
#' @return
#'
#'  This function will return a list that contains two dataframes:
#'  -`vs_processed`: The `vs_results` table with additional rows reflecting the overall / "network level"
#'  counts and a computed proportion of violating concepts
#'  -`vs_violations`: A table listing all of the violating values that exist in the data that
#'  do not comply with the provided valueset definition
#'
#' @export
#'
#' @examples
#' # This function should be run after check_vs has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_vs_process <- process_vs(vs_results = my_vs_rslts,
#'                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_vs_process <- process_vs(vs_results = 'my_vs_rslts',
#'                             rslt_source = 'csv',
#'                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_vs_process <- process_vs(vs_results = 'my_vs_rslts',
#'                             rslt_source = 'remote')
#' }
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
             check_name, total_denom_ct, accepted_value, check_description) %>%
    summarise(tot_ct = sum(total_viol_ct),
              tot_prop = sum(prop_total_viol)) %>%
    ungroup()

  vs_violations <- vs_process %>%
    filter(!accepted_value)%>%
    group_by(site, table_application, measurement_column, check_type,
             check_name, total_denom_ct, check_description) %>%
    summarise(tot_ct=sum(tot_ct),
              tot_prop=sum(tot_prop))%>%
    ungroup()%>%
    mutate(check_name_app=paste0(check_name, "_rows"))

  opt <- list('vs_processed' = vs_process,
              'vs_violations' = vs_violations)

  return(opt)
}
