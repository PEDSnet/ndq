
#' Clinical Fact Documentation
#'
#' This function will identify visits that do not link to the user-specified
#' facts. It will also compute the counts of patients who have at least one
#' visit that does not link to the specified fact type.
#'
#' @param cfd_tbl a table with information about the fact tables that should be
#'               evaluated against the visit_tbl; see `?cfd_input omop` or `?cfd_input_pcornet`
#' @param visit_type_filter a string or vector of strings label to identify the
#'                          visit type(s) for which you are executing the check
#'                          (i.e. inpatient, c(inpatient, outpatient));
#'
#'                          if `all` is included as a visit type, all available
#'                          visit types will be pulled from the visit_tbl
#'                          (not just the visit_type_tbl) to capture the full array of visits.
#' @param visit_type_tbl a table with mappings from the visit type string label to the
#'                       visit_concept_ids / enc_types that represent it. Multiple rows
#'                       should be included for multiple mappings.
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param visit_tbl the CDM table with visit information, that will be filtered to the visit
#'                  type of interest based on the visit_type_tbl + visit_type_filter
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `cfd`
#'
#'
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::cfd_input_omop
#' ndq::cfd_input_pcornet
#'
#' # Next define the visit types to be examined
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::cfd_visit_types_omop
#' ndq::cfd_visit_types_pcornet
#'
#' # Use this as your input to the CFD function
#' \dontrun{
#' my_cfd_rslt <- check_cfd(cfd_tbl = ndq::cfd_input_omop,
#'                          visit_type_tbl = ndq::cfd_visit_types_omop,
#'                          visit_type_filter = c('inpatient', 'outpatient'),
#'                          omop_or_pcornet = 'omop',
#'                          visit_tbl = cdm_tbl('visit_occurrence'),
#'                          check_string = 'cfd')
#' }
#'
check_cfd <- function(cfd_tbl,
                      visit_type_filter,
                      visit_type_tbl,
                      omop_or_pcornet = 'omop',
                      visit_tbl=cdm_tbl('visit_occurrence'),
                      check_string='cfd') {

  site_nm <- config('qry_site')

  if(tolower(omop_or_pcornet) == 'omop'){
    pt_col <- 'person_id'
    visit_col <- 'visit_occurrence_id'
    visit_type_col <- colnames(visit_type_tbl)[2]
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    pt_col <- 'patid'
    visit_col <- 'encounterid'
    visit_type_col <- colnames(visit_type_tbl)[2]
  }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

  for(k in visit_type_filter){

    if(k != 'all'){
      cids <- visit_type_tbl %>%
        filter(visit_type == k) %>%
        pull(!!sym(visit_type_col))
    }else{
      cids <- visit_tbl %>%
        distinct(!!sym(visit_type_col)) %>%
        collect() %>% pull(!!sym(visit_type_col))
    }

    visit_tbl_filt <- visit_tbl %>%
      add_site() %>%
      filter(site == site_nm,
             !!sym(visit_type_col) %in% cids)

    visit_tbl_all <-
      visit_tbl_filt %>%
      summarise(
        total_visits = n(),
        total_pts = n_distinct(!!sym(pt_col))
      ) %>% ungroup() %>% collect()

    fact_tbls <- split(cfd_tbl, seq(nrow(cfd_tbl)))

    all_tbls <- list()

    for (i in 1:length(fact_tbls)) {

      if(!is.na(fact_tbls[[i]]$filter_logic)){
        tbl_use <- pick_schema(schema = fact_tbls[[i]]$schema,
                               table = fact_tbls[[i]]$table,
                               db = config('db_src')) %>%
          filter(!! rlang::parse_expr(fact_tbls[[i]]$filter_logic))
      }else{
        tbl_use <- pick_schema(schema = fact_tbls[[i]]$schema,
                               table = fact_tbls[[i]]$table,
                               db = config('db_src'))
      }

      check_description_name <- fact_tbls[[i]]$check_description

      chk_nm <- fact_tbls[[i]]$check_id

      cli::cli_inform(paste0('Starting ',check_description_name))

      visit_tbl_all_name <-
        visit_tbl_all %>%
        mutate(check_desc = check_description_name)

      missed_visits <-
        visit_tbl_filt %>%
        select(person_id,
               !!sym(visit_col)) %>%
        anti_join(tbl_use,
                  by=visit_col) %>%
        mutate(check_desc = check_description_name) %>%
        group_by(
          check_desc
        ) %>%
        summarise(
          no_fact_visits = n(),
          no_fact_pts = n_distinct(!!sym(pt_col))
        ) %>% ungroup() %>% collect()

      # missed_pts <-
      #   visit_tbl %>% select(!!sym(pt_col)) %>%
      #   anti_join(tbl_use,
      #             by=pt_col) %>%
      #   mutate(check_description = check_description_name) %>%
      #   group_by(
      #     check_description
      #   ) %>%
      #   summarise(
      #     no_fact_pts = n_distinct(!!sym(pt_col))
      #   ) %>% ungroup() %>% collect()

      cts_combined <-
        visit_tbl_all_name %>%
        left_join(missed_visits) %>%
        # left_join(missed_pts) %>%
        mutate(
          no_fact_visits_prop = ifelse(total_visits == 0, NA,
                                       round(no_fact_visits / total_visits, 2)),
          no_fact_pts_prop = ifelse(total_pts == 0, NA,
                                    round(no_fact_pts / total_pts, 2))
        )  %>%
        add_meta(check_lib=check_string) %>%
        mutate_if(., is.numeric, ~replace(., is.na(.), 0)) %>%
        mutate(
          fact_visits = total_visits - no_fact_visits,
          fact_pts = total_pts - no_fact_pts,
          fact_visits_prop = ifelse(total_visits == 0, NA, round(1.00 - no_fact_visits_prop, 2)),
          fact_pts_prop = ifelse(total_pts == 0, NA, round(1.00 - no_fact_pts_prop, 2))
        ) %>% mutate(visit_type = k,
                     check_name=paste0(check_string, '_', chk_nm, '_', visit_type))

      all_tbls[[chk_nm]] <- cts_combined
    }
  }


  all_tbls_red <- purrr::reduce(.x = all_tbls,
                                .f = dplyr::union)

}


#' Clinical Fact Documentation -- Processing
#'
#' Intakes the output of check_cfd in order to apply additional processing. This
#' includes computing overall counts/proportions across all sites included in the input and tidying
#' some of the descriptive metadata.
#'
#' @param cfd_results table output by check_cfd
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return cfd_output tbl with additional total counts
#'
#' @importFrom stringr str_remove
#'
#' @export
#'
#' @examples
#' # This function should be run after check_cfd has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' ## When results are kept locally:
#' \dontrun{
#' my_cfd_process <- process_cfd(cfd_results = my_cfd_rslts,
#'                               rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_cfd_process <- process_cfd(cfd_results = 'my_cfd_rslts',
#'                               rslt_source = 'csv',
#'                               csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_cfd_process <- process_cfd(cfd_results = 'my_cfd_rslts',
#'                               rslt_source = 'remote')
#' }
#'
#'
process_cfd <- function(cfd_results,
                        rslt_source = 'remote',
                        csv_rslt_path = NULL) {

  if(tolower(rslt_source) == 'remote'){
    cfd_int <- results_tbl(cfd_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    cfd_int <- readr::read_csv(paste0(csv_rslt_path, cfd_results))
  }else if(tolower(rslt_source) == 'local'){
    cfd_int <- cfd_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  db_version<-config('current_version')

  cfd_totals <- cfd_int %>%
    group_by(check_desc, check_name, visit_type) %>%
    summarise(no_fact_visits=sum(no_fact_visits),
              no_fact_pts=sum(no_fact_pts),
              total_visits=sum(total_visits),
              total_pts=sum(total_pts),
              fact_visits=sum(fact_visits),
              fact_pts=sum(fact_pts)) %>%
    ungroup()%>%
    mutate(site='total',
           check_type='cfd',
           database_version=db_version)%>%
    mutate(no_fact_visits_prop=round(no_fact_visits/total_visits,2),
           no_fact_pts_prop=round(no_fact_pts/total_pts,2),
           fact_visits_prop=1-no_fact_visits_prop,
           fact_pts_prop=1-no_fact_pts_prop) %>%
    collect()

  # have to collect to bind rows since total columns may be missing site-specific things (e.g. thresholds)
  bind_rows(cfd_int, cfd_totals)%>%
    mutate(check_name_app=paste0(check_name, "_visits"),
           check_desc_neat=str_remove(check_desc, "visits_with_|_visits"))

}
