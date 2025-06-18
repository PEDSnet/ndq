
#' Vocabulary Conformance
#'
#' This function will use the `vocabulary.concept` table to identify the vocabulary
#' of each concept and determine how many rows comply with the standard vocabularies
#' expected for that field and how many rows violate these expectations.
#'
#' @param vc_tbl a table with the table, field, and vocabulary information for each
#'               check
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `vc`
#' @param concept_tbl a vocabulary table, like the OMOP concept table, with at least the concept column of interest (concept_id or concept_code),
#'                    the concept name, and the vocabulary id
#' @param null_values a vector of NULL values (or other values that would not belong to accepted vocabularies
#'                    but are broadly accepted) that should be excluded when identifying non-valueset concepts
#'
#' @return a dataframe with summary information about each vocabulary that appears in the field,
#'         with violations marked in a T/F field; vocabularies associated with the indicated
#'         null_values are ignored, so proportions may not add up to 1 as a result
#'
#' @importFrom stringr str_split
#'
#' @export
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
                total_concept_ct=n_distinct(!!sym(concept_id_fn))) %>% collect_new() %>%
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
      collect_new() %>%
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
#' Intakes the output of check_vc in order to apply additional processing. This
#' includes computing row and patient proportions and computing overall totals
#' across all sites included in the input.
#'
#' @param vc_results the output of check_vs
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return a list that contains two dataframes:
#' - `vc_processed`: a dataframe with additional columns that include proportions of violations
#'                   and the overall summary
#' - `vc_violations`: a dataframe with ONLY violating vocabularies
#'
#' @export
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
