
#' Vocabulary Conformance
#'
#' This function will use a provided table with concept to vocabulary mappings to
#' identify the vocabulary of each concept and determine how many rows comply with
#' the standard vocabularies expected for that field and how many rows violate these expectations.
#'
#' @param vc_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for database table fields and
#'  the expected vocabularies for that field.
#'  see `?vc_input_omop` or `?vc_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet somop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param concept_tbl *tabular input* || defaults to `vocabulary_tbl('concept')`
#'
#'  A vocabulary table with concept definitions (for example, the OHDSI concept table) that includes
#'  the vocabulary to which the concept belongs. This table should at a minimum have the columns:
#'  `concept_id`, `concept_name`, `vocabulary_id`
#'
#' @param null_values *string / vector* || defaults to 44814650, 0, 44814653, & 44814649
#'
#'  A string or vector listing the concept(s) that indicate a NULL value, which will be
#'  excluding when assessing for the presence of vocabularies not accepted in the field.
#'
#' @param check_string *string* || defaults to `vc`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @return
#'
#'  This function will return a table with summary information about each vocabulary that
#'  appears in the field, with violations marked in a T/F field. These summaries are computed at
#'  both the row and concept levels. This is to account for any cases where a large quantity of rows are
#'  in violation of the acceptable vocabularies, but it is made up of only 1-2 distinct concepts.
#'
#'  Note that vocabularies associated with the indicated `null_values` are ignored, so proportions may
#'  not add up to 1 as a result.
#'
#' @importFrom stringr str_split
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::vc_input_omop
#' ndq::vc_input_pcornet
#'
#' # Use this as your input to the vc function
#' \dontrun{
#' my_vc_rslt <- check_vc(vc_tbl = ndq::vc_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
#'                        null_values = c(44814650L,0L,
#'                                        44814653L,44814649L), ## ignored illegal vocabs
#'                        check_string = 'vc')
#' }
#'
#'
check_vc <- function(vc_tbl,
                     omop_or_pcornet = 'omop',
                     check_string='vc',
                     concept_tbl = vocabulary_tbl('concept'),
                     null_values = c(44814650L,0L,44814653L,44814649L)) {

  site_nm <- config('qry_site')

  vocabvals <- split(vc_tbl, seq(nrow(vc_tbl)))

  vocab_illegals <- list()

  for(i in 1:length(vocabvals)) {

    cli::cli_inform(paste0('Starting ', vocabvals[[i]]$check_id))

    input_list <- list()

    values <- vocabvals[[i]]$acceptable_vocabularies %>%
      gsub(", ", ",", .) %>%
      # str_split(', ') %>%
      str_split(',') %>% unlist()

    concept_id_fn <- vocabvals[[i]]$concept_field

    if(omop_or_pcornet == 'omop'){
      join_cols <- set_names('concept_id', paste0(concept_id_fn))
      pt_col <- 'person_id'
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      join_cols <- set_names('concept_code', paste0(concept_id_fn))
      pt_col <- 'patid'
    }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

    if(!is.na(vocabvals[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = vocabvals[[i]]$schema,
                             table = vocabvals[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(vocabvals[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = vocabvals[[i]]$schema,
                             table = vocabvals[[i]]$table,
                             db = config('db_src'))
    }

    total_rows <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      summarise(total_denom_ct=n(),
                total_pt_ct=n_distinct(!!sym(pt_col)),
                total_concept_ct=n_distinct(!!sym(concept_id_fn))) %>% collect() %>%
      add_meta(check_lib = check_string)

    illegal_values <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      filter(! .data[[concept_id_fn]] %in% null_values) %>%
      inner_join(select(
        concept_tbl,
        concept_id, concept_name, vocabulary_id
      ), by = join_cols) %>%
      group_by(vocabulary_id) %>%
      summarise(total_viol_ct = n(),
                total_viol_pt_ct = n_distinct(!!sym(pt_col)),
                total_viol_concept_ct = n_distinct(!!sym(concept_id_fn))) %>%
      ungroup() %>%
      collect() %>%
      add_meta(check_lib = check_string) %>%
      mutate(check_name = paste0(check_string, '_', vocabvals[[i]]$check_id),
             table_application = vocabvals[[i]]$table,
             accepted_value = ifelse(vocabulary_id %in% values, TRUE, FALSE),
             concept_name = paste0('Vocabulary Identifier - ', vocabulary_id),
             temp = 0) %>%
      relocate(temp) %>%
      rename_with(~concept_id_fn, temp) %>%
      left_join(total_rows)

    vocab_illegals[[vocabvals[[i]]$check_id]] <- illegal_values

  }

  vocab_illegals

  vocab_illegals[!sapply(vocab_illegals, function(x) all(is.na(x)))]

  final_vocabs <- create_vc_vs_output(tbl_list = vocab_illegals,
                                      check_string = check_string)

  final_vocabs_red <- purrr::reduce(.x = final_vocabs,
                                    .f = dplyr::union)

  return(final_vocabs_red)

}


#' Vocabulary Conformance -- Processing
#'
#' Intakes the output of `check_vc` in order to apply additional processing. This
#' includes computing row and patient proportions and computing overall totals
#' across all sites included in the input.
#'
#' @param vc_results *tabular input* || **required**
#'
#'  The tabular output of `check_vc`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `vc_results` table.
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
#'  -`vc_processed`: The `vc_results` table with additional rows reflecting the overall / "network level"
#'  counts and a computed proportion of violating vocabularies
#'  -`vc_violations`: A table listing all of the vocabulary violations
#'
#' @export
#'
#' @examples
#' # This function should be run after check_vc has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_vc_process <- process_vc(vc_results = my_vc_rslts,
#'                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_vc_process <- process_vc(vc_results = 'my_vc_rslts',
#'                             rslt_source = 'csv',
#'                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_vc_process <- process_vc(vc_results = 'my_vc_rslts',
#'                             rslt_source = 'remote')
#' }
#'
process_vc <-function(vc_results,
                      rslt_source = 'remote',
                      csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    vc_int <- results_tbl(vc_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    vc_int <- readr::read_csv(paste0(csv_rslt_path, vc_results))
  }else if(tolower(rslt_source) == 'local'){
    vc_int <- vc_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  vc_process <- vc_int %>%
    mutate(prop_total_viol=as.numeric(total_viol_ct/total_denom_ct),
           prop_total_pt_viol=as.numeric(total_viol_pt_ct/total_pt_ct)) %>%
    group_by(site, table_application, measurement_column, vocabulary_id,
             check_type, check_name, total_denom_ct, total_concept_ct, accepted_value) %>%
    summarise(tot_ct = sum(total_viol_ct),
              tot_prop = sum(prop_total_viol),
              tot_dist_concept_ct=sum(total_viol_concept_ct)) %>%
    ungroup()%>%
    mutate(tot_dist_concept_prop=tot_dist_concept_ct/total_concept_ct)

  vc_violations <- vc_process %>%
    filter(!accepted_value)%>%
    group_by(site, table_application, measurement_column, check_type, check_name, total_denom_ct) %>%
    summarise(tot_ct=sum(tot_ct),
              tot_prop=sum(tot_prop))%>%
    ungroup()%>%
    mutate(check_name_app=paste0(check_name, "_rows"))

  opt <- list('vc_processed' = vc_process,
              'vc_violations' = vc_violations)

  return(opt)
}
