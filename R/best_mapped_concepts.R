

#' Best Mapped Concepts
#'
#' This function will identify the existing concepts within the specified field
#' so the user can assess which of these concepts are acceptable ("best") or should
#' not be used in that field ("not best")
#'
#' @param bmc_tbl *tabular input* || *required*
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions the fields that should be
#'  evaluated to determine if they only include "best" concepts.
#'  see `?bmc_input_omop` or `?bmc_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param concept_tbl *tabular input* || defaults to `NULL`
#'
#'  An optional parameter used to define a vocabulary table with concept definitions
#'  (for example, the OHDSI concept table). If left NULL, the concepts as they exist
#'  in the fact table will be returned to the user.
#'
#' @param check_string *string* || defaults to `bmc`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @return
#'
#'  This function will return a list of two dataframes:
#'  - `bmc_counts`: A table with one row for each concept present in each user-defined field
#'  and the associated row and patient counts/proportions
#'  - `bmc_concepts`: A table with just the concepts from `bmc_counts`. This output is
#'  should be labelled with "best" (1) vs "not best" (0) indicators in a column
#'  called `include` for use in the processing step
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::bmc_input_omop
#' ndq::bmc_input_pcornet
#'
#' # Use this as your input to the BMC function
#' \dontrun{
#' my_bmc_rslt <- check_bmc(bmc_tbl = ndq::bmc_input_omop,
#'                          omop_or_pcornet = 'omop',
#'                          concept_tbl = vocabulary_tbl("concept"), ## points to OHDSI concept table
#'                          check_string = 'bmc')
#' }
#'
#'
check_bmc <- function(bmc_tbl,
                      omop_or_pcornet = 'omop',
                      concept_tbl = NULL,
                      check_string='bmc') {

  site_nm <- config('qry_site')

  if(omop_or_pcornet == 'omop'){
    pt_col <- 'person_id'
    pt_tbl <- 'person'
  }else if(omop_or_pcornet == 'pcornet'){
    pt_col <- 'patid'
    pt_tbl <- 'demographic'
  }else{
    cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

  fact_tbl_list_args <- split(bmc_tbl, seq(nrow(bmc_tbl)))

  results <- list()

  for(i in 1:length(fact_tbl_list_args)) {

    cli::cli_inform(paste0('Starting ', fact_tbl_list_args[[i]]$check_description))

    if(!is.na(fact_tbl_list_args[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = fact_tbl_list_args[[i]]$schema,
                             table = fact_tbl_list_args[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(fact_tbl_list_args[[i]]$filter_logic)) %>%
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = pt_col) %>% filter(site == site_nm)
    }else{
      tbl_use <- pick_schema(schema = fact_tbl_list_args[[i]]$schema,
                             table = fact_tbl_list_args[[i]]$table,
                             db = config('db_src')) %>%
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = pt_col) %>% filter(site == site_nm)
    }

    if(is.null(concept_tbl)){
      xwalk <- tbl_use %>%
        rename(concept_type=!!sym(fact_tbl_list_args[[i]]$concept_field))
    }else{

      xwalk <-
        find_concept_names(fact_tbl = tbl_use,
                           omop_or_pcornet = omop_or_pcornet,
                           fact_concept_id = fact_tbl_list_args[[i]]$concept_field,
                           concept_field = fact_tbl_list_args[[i]]$concept_table_field,
                           concept_tbl = concept_tbl)
    }

    total_cts <-
      xwalk %>%
      summarise(total_rows=n(),
                total_pts=n_distinct(!!sym(pt_col))) %>% collect()

    grps <- dplyr::group_vars(xwalk)

    concept_grpd <- c(grps, 'concept_type')

    concept_cts <-
      xwalk %>%
      group_by(!!! syms(concept_grpd)) %>%
      summarise(concept_rows=n(),
                concept_pts=n_distinct(!!sym(pt_col))) %>%
      rename('concept' = !!sym(concept_grpd)) %>%
      mutate(concept = as.character(concept)) %>%
      collect()

    if(length(concept_grpd) > 1) {

      props <-
        concept_cts %>%
        left_join(total_cts) %>% ungroup() %>%
        mutate(row_proportions=round(concept_rows/total_rows,2)) %>%
        mutate(person_proportions=round(concept_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', fact_tbl_list_args[[i]]$check_id)) %>%
        mutate(check_desc = fact_tbl_list_args[[i]]$check_description) #%>%
        #mutate(check_desc_short =fact_tbl_name)

    } else {

      props <-
        concept_cts %>%
        mutate(total_rows=total_cts$total_rows,
               total_pts=total_cts$total_pts) %>%
        mutate(row_proportions=round(concept_rows/total_rows,2)) %>%
        mutate(person_proportions=round(concept_pts/total_pts,2)) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', fact_tbl_list_args[[i]]$check_id)) %>%
        mutate(check_desc = fact_tbl_list_args[[i]]$check_description) #%>%
        #mutate(check_desc_short=fact_tbl_name)

    }


    results[[paste0(check_string,'_',fact_tbl_list_args[[i]]$check_id)]] <- props

  }

  results_red <- purrr::reduce(.x = results,
                               .f = dplyr::union)

  bmc_concepts <- results_red %>% distinct(check_name, concept)

  opt <- list('bmc_counts' = results_red,
              'bmc_concepts' = bmc_concepts)

  cli::cli_alert_info(stringr::str_wrap(cli::col_br_magenta('To prepare the concept set for the processing step,
                                        please add a column called "include" to the bmc_concepts output and mark any
                                        concepts that are NOT best with a 0.')))

  return(opt)

}


#' Find concept names for existing concept_ids
#'
#' @param fact_tbl the fact table associated with the field
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param fact_concept_id the concept field in the fact table (i.e. condition_concept_id or dx)
#' @param concept_field the field in the concept table to be used in the analysis; will typically be
#'                      concept_name, but concept_class_id is used for RxNorm class identifiers
#' @param concept_tbl defaults to `vocabulary_tbl('concept')`
#'
#' @return the provided fact_tbl with additional information from the specified concept_field
#'
#' @keywords internal
#'
find_concept_names <- function(fact_tbl,
                               omop_or_pcornet,
                               fact_concept_id,
                               concept_field,
                               concept_tbl=vocabulary_tbl('concept')) {


  if(omop_or_pcornet == 'omop'){
    fact_tbl_new <- fact_tbl %>%
      select(person_id, !!sym(fact_concept_id)) %>%
      rename(concept_id = !!sym(fact_concept_id)) %>%
      inner_join(
        select(concept_tbl,
               concept_id,
               !!sym(concept_field)),
        by=c('concept_id')
      ) %>% rename(concept_type=!!sym(concept_field)) %>%
      compute_new(name = 'temp_bmc', overwrite = TRUE)
  }else if(omop_or_pcornet == 'pcornet'){
    fact_tbl_new <- fact_tbl %>%
      select(patid, !!sym(fact_concept_id)) %>%
      rename(concept_id = !!sym(fact_concept_id)) %>%
      inner_join(
        select(concept_tbl,
               concept_code,
               !!sym(concept_field)),
        by=c('concept_code' = 'concept_id')
      ) %>% rename(concept_type=!!sym(concept_field)) %>%
      compute_new(name = 'temp_bmc', overwrite = TRUE)
  }

  return(fact_tbl_new)

}


#' assign "best" "not best" labels to concepts
#'
#' @param bmc_output the bmc_counts output from check_bmc
#' @param conceptset the bmc_concepts output from check_bmc with an additional
#'                   column called "include" added with "not best" or unideal concepts
#'                   marked with a 0
#'
#' @returns a dataframe with the include designations attached to all concepts; also checks
#'          to ensure no additional values are present
#'
#' @keywords internal
#'
bmc_assign <- function(bmc_output,
                       conceptset){

  bmc_w_best <- bmc_output %>%
    inner_join(conceptset, by = c('check_name', 'concept'))%>%
    mutate(include=case_when(is.na(include) | is.null(include) ~1L, # add 1s for NA include designations
                             !is.na(include)~include)) %>%
    collect()

  dist_val <- bmc_w_best %>% distinct(include) %>% pull()

  if(length(dist_val) > 2){cli::cli_abort(stringr::str_wrap('The "include" column should only include 0s (not best) or optionally 1s (best).
                                                    Please remove any additional values from that column.'))}

  return(bmc_w_best)

}

#' Function to compute proportion of "best" based on output from bmc check
#'
#' @param bmc_output_pp table output from the bmc_assign function, which has all the
#' columns output from the bmc check + an indicator column for whether the concept
#' should be in the "best" category
#' @return table with the cols: site, check_type, database_version, check_name,
#' check_desc,  count_best, include, total_rows, total_pts, best_row_prop, best_pts_prop
#'
#' @keywords internal
#'
bmc_rollup <- function(bmc_output_pp){
  # find proportions of best mapped for each site
  bmc_sites <- bmc_output_pp %>%
    group_by(across(c(site, include, check_type, database_version,
                      starts_with("check_name"), check_desc, total_rows)))%>%
    summarise(best_rows=sum(concept_rows)) %>%
    ungroup()%>%
    mutate(best_row_prop=best_rows/total_rows)

  # finding instances where no best mapped rows in table for site
  bmc_no_sites <- bmc_sites %>%
    group_by(across(c(site, check_type, database_version, starts_with("check_name"), check_desc, total_rows)))%>%
    summarise(include=max(include)) %>%
    ungroup()%>%
    filter(include==0L)%>%
    mutate(best_rows=0L,
           best_row_prop=0,
           include=1L)

  # put together all of the site-level info
  bmc_all <- bmc_sites %>%
    bind_rows(bmc_no_sites)

  # add up site counts to get overall proportions
  bmc_overall <- bmc_all %>%
    filter(include==1L)%>%
    group_by(check_type, database_version, check_name, check_desc)%>%
    summarise(best_rows=sum(best_rows),
              total_rows=sum(total_rows))%>%
    ungroup()%>%
    mutate(best_row_prop=best_rows/total_rows,
           site='total')

  bind_rows(bmc_all, bmc_overall)%>%
    mutate(check_name_app=paste0(check_name, "_rows"))

}


#' Best Mapped Concepts -- Processing
#'
#' Intakes the output of `check_bmc` in order to apply additional processing. This
#' includes applying the user-specified best/not best labels that were added to
#' the `bmc_concepts` table, then using those labels to compute proportions of
#' best vs not best concept representation in each check.
#'
#' @param bmc_results *tabular input* || **required**
#'
#'  The `bmc_counts` output of `check_bmc`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param bmc_concepts_labelled *tabular input* || **required**
#'
#'  The `bmc_concepts` output of `check_bmc`, with an additional  column called `include`
#'  added with "not best" or non-ideal concepts marked with a 0
#'  (optionally, "best" concepts can also be marked with a 1)
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `bmc_results` table.
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
#'  This function will return a list of two dataframes:
#'  - `bmc_output_pp`: A table summarizing the proportion of best vs not best
#'  concepts for a given check, indicated by the user designation in the
#'  `bmc_concepts_labelled` table
#'  - `bmc_concepts_pp`: The `bmc_results` input table with the best / not best designations
#'  added
#'
#' @importFrom stringr str_wrap
#'
#' @export
#'
#' @examples
#' # This function should be run after check_bmc has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # All returned concepts should also be labelled to indicate whether they
#' # should be included as a "best" concept or not:
#'
#' readr::read_csv(system.file('extdata', 'bmc_concept_examples.csv', package = 'ndq'))
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_bmc_process <- process_bmc(bmc_results = my_bmc_rslts,
#'                               bmc_concepts_labelled = my_bmc_concepts,
#'                               rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
#'                               bmc_concepts_labelled = 'my_bmc_concepts',
#'                               rslt_source = 'csv',
#'                               csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_bmc_process <- process_bmc(bmc_results = 'my_bmc_rslts',
#'                               bmc_concepts_labelled = 'my_bmc_concepts',
#'                               rslt_source = 'remote')
#' }
#'
process_bmc <- function(bmc_results,
                        bmc_concepts_labelled,
                        rslt_source = 'remote',
                        csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    bmc_int <- results_tbl(bmc_results) %>%
      collect()
    bmc_concepts <- results_tbl(bmc_concepts_labelled) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    bmc_int <- readr::read_csv(paste0(csv_rslt_path, bmc_results))
    bmc_concepts <- readr::read_csv(paste0(csv_rslt_path, bmc_concepts_labelled))
  }else if(tolower(rslt_source) == 'local'){
    bmc_int <- bmc_results %>% collect()
    bmc_concepts <- bmc_concepts_labelled %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}


  bmc_assigned <- bmc_assign(bmc_output = bmc_int,
                             conceptset = bmc_concepts)

  bmc_rolled <- bmc_rollup(bmc_output_pp = bmc_assigned)

  opt <- list('bmc_concepts_pp' = bmc_assigned,
              'bmc_output_pp' = bmc_rolled)

  return(opt)

}
