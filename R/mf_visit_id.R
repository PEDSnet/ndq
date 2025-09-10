
#' Missing Field: Visit ID
#'
#' This function will check to see if the `visit_occurrence_id`/`encounterid` in a given fact table
#' also exists in the `visit_occurrence`/`encounter` table and identify cases where these IDs
#' are missing entirely (NULL). There may be cases where this is expected
#' (for example, immunizations imported from an external registry) but generally the
#' `visit_occurrence_id`/`encounterid` should be populated and exist as a primary key in the
#' `visit_occurrence`/`encounter` table.
#'
#' @param mf_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the clinical fact
#'  types with visit IDs that should be evaluated against the visit_tbl.
#'  see `?mf_visitid_input_omop` or `?mf_visitid_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param visit_tbl *tabular input* || defaults to `cdm_tbl('visit_occurrence')`
#'
#'  The CDM table with the visit primary key IDs. Typically, this will
#'  be either the OMOP `visit_occurrence` or PCORnet `encounter` table
#'
#' @param check_string *string* || defaults to `mf_visitid`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @return
#'
#'  This function will return a table summarizing the total number of visits in the fact table,
#'  the number of NULL visit IDs, and the number of visits that do not have an associated entry
#'  in the visit table
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::mf_input_omop
#' ndq::mf_input_pcornet
#'
#' # Use this as your input to the MF: Visit ID function
#' \dontrun{
#' my_mf_rslt <- check_mf(mf_tbl = ndq::mf_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        visit_tbl = cdm_tbl('visit_occurrence'), ## table with visit ID primary keys
#'                        check_string = 'mf_visitid')
#' }
#'
check_mf_visitid <- function(mf_tbl,
                             omop_or_pcornet = 'omop',
                             visit_tbl = cdm_tbl('visit_occurrence'),
                             check_string='mf_visitid') {

  site_nm <- config('qry_site')

  check_visit_list <- split(mf_tbl, seq(nrow(mf_tbl)))

  tbl_visits <- list()

  for(i in 1:length(check_visit_list)) {

    cli::cli_inform(paste0('Starting ', check_visit_list[[i]]$check_description))

    if(tolower(omop_or_pcornet) == 'omop'){
      visit_col <- 'visit_occurrence_id'
      pt_col <- 'person_id'
      pt_tbl <- 'person'
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      visit_col <- 'encounterid'
      pt_col <- 'patid'
      pt_tbl <- 'demographic'
    }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

    if(!is.na(check_visit_list[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = check_visit_list[[i]]$schema,
                             table = check_visit_list[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(check_visit_list[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = check_visit_list[[i]]$schema,
                             table = check_visit_list[[i]]$table,
                             db = config('db_src'))
    }

    total_rows <-
      tbl_use %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      summarise(total_ct = as.numeric(n())) %>%
      collect()

    total_visit_ids <-
      tbl_use %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      summarise(
        total_visits=as.numeric(n_distinct(!!sym(visit_col)))
      ) %>% collect()

    tbl_visit_ids <-
      tbl_use %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      anti_join(
        visit_tbl,
        by=visit_col
      )

    tbl_visit_flags <-
      tbl_visit_ids %>%
      mutate(missing_flag =
               case_when(
                 is.na(!!sym(visit_col)) ~ 'visit_na',
                 TRUE ~ 'visit_id'
               ))

    visit_summaries <-
      tbl_visit_ids %>%
      summarise(
        missing_visits_total = as.numeric(n()),
        missing_visits_distinct = as.numeric(n_distinct(!!sym(visit_col)))
      ) %>% collect()

    visit_summaries_nas <-
      tbl_visit_flags %>%
      group_by(
        missing_flag
      ) %>% summarise(
        total_ct = as.numeric(n())
      ) %>% collect()

    if(dim(visit_summaries_nas)[1] == 0) {

      visit_summaries_nas_all <-
        visit_summaries_nas %>%
        add_row(
          missing_flag = 'visit_na',
          total_ct = as.numeric(0)
        ) %>%
        add_row(
          missing_flag = 'visit_id',
          total_ct = as.numeric(0)
        )

    } else if(dim(visit_summaries_nas)[1] == 1) {

      if(visit_summaries_nas$missing_flag == 'visit_na') {
        visit_summaries_nas_all <-
          visit_summaries_nas %>%
          add_row(
            missing_flag = 'visit_id',
            total_ct = as.numeric(0)
          ) } else {
            visit_summaries_nas_all <-
              visit_summaries_nas %>%
              add_row(
                missing_flag = 'visit_na',
                total_ct = as.numeric(0)
              )
          }
      visit_summaries_nas_all

    }  else {visit_summaries_nas_all <- visit_summaries_nas }

    visit_summaries_nas_all <-
      visit_summaries_nas_all %>%
      pivot_wider(
        names_from = 'missing_flag',
        values_from = 'total_ct'
      )

    all_tbl <- tibble(
      measure = check_visit_list[[i]]$check_description,
      total_ct = as.numeric(total_rows$total_ct),
      total_visits = as.numeric(total_visit_ids$total_visits),
      missing_visits_total = as.numeric(visit_summaries$missing_visits_total),
      missing_visits_distinct = as.numeric(visit_summaries$missing_visits_distinct),
      visit_na = as.numeric(visit_summaries_nas_all$visit_na),
      total_id = as.numeric(visit_summaries_nas_all$visit_id)
    ) %>% distinct()

    tbl_visits[[i]] = all_tbl %>% add_meta(check_lib = check_string) %>%
      mutate(check_name=paste0(check_string, '_', check_visit_list[[i]]$check_id),
             domain = check_visit_list[[i]]$table)

  }

  tbl_visits_red <- purrr::reduce(.x = tbl_visits,
                                  .f = dplyr::union)

  return(tbl_visits_red)

}


#' Missing Field: Visit ID -- Processing
#'
#' Intakes the output of `check_mf_visitid` in order to apply additional processing.
#' This includes computing proportions of missing visits and computing overall
#' totals across all sites included in the input.
#'
#' @param mf_visitid_results *tabular input* || **required**
#'
#'  The tabular output of `check_mf_visitid`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `mf_visitid_results` table.
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
#'  This function will return the `mf_visitid_results` table with the addition
#'  of a column that reflects proportion of missing visits
#'
#' @export
#'
#' @examples
#' # This function should be run after check_mf_visitid has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = my_mf_visitid_rslts,
#'                                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = 'my_mf_visitid_rslts',
#'                                             rslt_source = 'csv',
#'                                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_mf_visitid_process <- process_mf_visitid(mf_visitid_results = 'my_mf_visitid_rslts',
#'                                             rslt_source = 'remote')
#' }
#'
process_mf_visitid <- function(mf_visitid_results,
                               rslt_source = 'remote',
                               csv_rslt_path = NULL) {

  if(tolower(rslt_source) == 'remote'){
    mf_int <- results_tbl(mf_visitid_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    mf_int <- readr::read_csv(paste0(csv_rslt_path, mf_visitid_results))
  }else if(tolower(rslt_source) == 'local'){
    mf_int <- mf_visitid_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  # compute overall counts for mf_visitid check
  test_mf_overall <- mf_int %>%
    group_by(domain, measure, check_type, database_version, check_name) %>%
    summarise(total_visits=sum(total_visits),
              missing_visits_total=sum(missing_visits_total),
              missing_visits_distinct=sum(missing_visits_distinct),
              visit_na=sum(visit_na),
              total_id=sum(total_id),
              total_ct=sum(total_ct)) %>%
    ungroup()%>%
    mutate(site = 'total') %>% collect()

  # compute proportions
  mf_int %>%
    dplyr::union_all(test_mf_overall)%>%
    filter(total_ct!=0)%>%
    mutate(prop_total_visits = round(total_visits/total_ct, 2),
           prop_missing_visits_total = round(missing_visits_total/total_ct,2))%>%
    mutate(check_name_app=paste0(check_name, "_rows"))

}
