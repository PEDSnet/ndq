
#' Patient Facts (NDQ)
#'
#' Function to find visit_occurrences without facts in another set of tables
#'
#' @param pf_tbl a table with information about the fact tables that should be
#'               evaluated against the visit_tbl
#' @param visit_type_string a string label to identify the visit type for which
#'.                         you are executing the check (i.e. inpatient)
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param visit_tbl the CDM table with visit information, filtered to the visit
#'                  type of interest
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `pf`
#'
#'
#' @return tbl with visit_occurrences, and all columns in the original visit_occurrence table,
#'        for which there are no facts in the `fact_tbls`
#'
#' @export
#'
check_pf <- function(pf_tbl,
                     visit_type_string,
                     omop_or_pcornet = 'omop',
                     visit_tbl=cdm_tbl('visit_occurrence'),
                     check_string='pf') {

  site_nm <- config('qry_site')

  if(tolower(omop_or_pcornet) == 'omop'){
    pt_col <- 'person_id'
    visit_col <- 'visit_occurrence_id'
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    pt_col <- 'patid'
    visit_col <- 'encounterid'
  }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

  visit_tbl_all <-
    visit_tbl %>%
    add_site() %>% filter(site == site_nm) %>%
    summarise(
      total_visits = n(),
      total_pts = n_distinct(!!sym(pt_col))
    ) %>% ungroup() %>% collect()

  fact_tbls <- split(pf_tbl, seq(nrow(pf_tbl)))

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
      mutate(check_description = check_description_name)

    missed_visits <-
      visit_tbl %>%
      add_site() %>% filter(site == site_nm) %>%
      select(person_id,
             !!sym(visit_col)) %>%
      anti_join(tbl_use,
                by=visit_col) %>%
      mutate(check_description = check_description_name) %>%
      group_by(
        check_description
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
        no_fact_visits_prop = round(
          no_fact_visits / total_visits, 2
        ),
        no_fact_pts_prop = round(
          no_fact_pts / total_pts, 2
        )
      )  %>%
      add_meta(check_lib=check_string) %>%
      mutate_if(., is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(
        fact_visits = total_visits - no_fact_visits,
        fact_pts = total_pts - no_fact_pts,
        fact_visits_prop = round(1.00 - no_fact_visits_prop, 2),
        fact_pts_prop = round(1.00 - no_fact_pts_prop, 2)
      ) %>% mutate(check_name=paste0(check_string, '_', chk_nm),
                   visit_type = visit_type_string)

    all_tbls[[chk_nm]] <- cts_combined
  }


  all_tbls_red <- purrr::reduce(.x = all_tbls,
                                .f = dplyr::union)

}


#' Patient Facts -- Processing
#'
#' function to add total counts by check_description
#'
#' @param pf_results table output by check_pf
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return pf_output tbl with additional total counts
#'
#' @importFrom stringr str_remove
#'
#' @export
#'
process_pf <- function(pf_results,
                       rslt_source = 'remote',
                       csv_rslt_path = NULL) {

  if(tolower(rslt_source) == 'remote'){
    pf_int <- results_tbl(pf_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    pf_int <- readr::read_csv(paste0(csv_rslt_path, pf_results))
  }else if(tolower(rslt_source) == 'local'){
    pf_int <- pf_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  db_version<-config('current_version')

  pf_totals <- pf_int %>%
    group_by(check_description, check_name) %>%
    summarise(no_fact_visits=sum(no_fact_visits),
              no_fact_pts=sum(no_fact_pts),
              total_visits=sum(total_visits),
              total_pts=sum(total_pts),
              fact_visits=sum(fact_visits),
              fact_pts=sum(fact_pts)) %>%
    ungroup()%>%
    mutate(site='total',
           check_type='pf',
           database_version=db_version)%>%
    mutate(no_fact_visits_prop=round(no_fact_visits/total_visits,2),
           no_fact_pts_prop=round(no_fact_pts/total_pts,2),
           fact_visits_prop=1-no_fact_visits_prop,
           fact_pts_prop=1-no_fact_pts_prop) %>%
    collect()

  # have to collect to bind rows since total columns may be missing site-specific things (e.g. thresholds)
  bind_rows(pf_int, pf_totals)%>%
    mutate(check_name_app=paste0(check_name, "_visits"),
           check_desc_neat=str_remove(check_description, "visits_with_|_visits"))

}
