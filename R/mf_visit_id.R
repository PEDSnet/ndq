
#' Missing Field: Visit ID
#'
#' This function will check to see if the visit_occurrence_id/encounterid in a given fact table
#' also exists in the visit_occurrence/encoubter table and identify cases where the visit_occurrence_id/encounterid
#' is missing entirely (NULL). There may be cases where this is expected
#' (for example, immunizations imported from an external registry) but generally the
#' visit_occurrence_id/encounterid should be populated and exist as a primary key in the
#' visit_occurrence/encounter table.
#'
#' @param mf_tbl a table with information about the tables that should be cross
#'               checked against the visit_tbl to ensure the visit_occurrence_id / encounterid
#'               exists as a primary key
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param visit_tbl the CDM visit_occurrence / encounter table that contains the visit ID primary key
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `mf_visitid`
#'
#' @return a table summarizing the total number of visits in the fact table, the number of NULL visits,
#'         and the number of visits that cannot link back to the visit table. has columns: measure,
#'         total_visits, missing_visits_total, missing_visits_distinct, visit_na, total_id, check_name,
#'         database_version,site
#'
#' @export
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
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      visit_col <- 'encounterid'
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
      add_site() %>% filter(site == site_nm) %>%
      summarise(total_ct = n()) %>%
      collect()

    total_visit_ids <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      summarise(
        total_visits=n_distinct(!!sym(visit_col))
      ) %>% collect()

    tbl_visit_ids <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
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
        missing_visits_total = n(),
        missing_visits_distinct = n_distinct(!!sym(visit_col))
      ) %>% collect()

    visit_summaries_nas <-
      tbl_visit_flags %>%
      group_by(
        missing_flag
      ) %>% summarise(
        total_ct = n()
      ) %>% collect()

    if(dim(visit_summaries_nas)[1] == 0) {

      visit_summaries_nas_all <-
        visit_summaries_nas %>%
        add_row(
          missing_flag = 'visit_na',
          total_ct = 0
        ) %>%
        add_row(
          missing_flag = 'visit_id',
          total_ct = 0
        )

    } else if(dim(visit_summaries_nas)[1] == 1) {

      if(visit_summaries_nas$missing_flag == 'visit_na') {
        visit_summaries_nas_all <-
          visit_summaries_nas %>%
          add_row(
            missing_flag = 'visit_id',
            total_ct = 0
          ) } else {
            visit_summaries_nas_all <-
              visit_summaries_nas %>%
              add_row(
                missing_flag = 'visit_na',
                total_ct = 0
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
      total_ct = total_rows$total_ct,
      total_visits = total_visit_ids$total_visits,
      missing_visits_total = visit_summaries$missing_visits_total,
      missing_visits_distinct = visit_summaries$missing_visits_distinct,
      visit_na = visit_summaries_nas_all$visit_na,
      total_id = visit_summaries_nas_all$visit_id
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
#' Intakes the output of check_mf_visitid in order to apply additional processing.
#' This includes computing proportions of missing visits and computing overall
#' totals across all sites included in the input.
#'
#' @param mf_visitid_results output of check_mf_visitid
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return mf_visitid tbl with additional domain, total_ct, and proportion
#' column
#'
#' @export
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
