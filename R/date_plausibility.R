
#' Date Plausibility
#'
#' This function will iterate through the provided input table to identify
#' the proportion of rows in each fact type that have an implausible date.
#' Implausibility is defined as a date that falls before the associated visit
#' start date, after the associated visit end date, or before the patient's
#' birth date.
#'
#' @param dp_tbl table with information about the fact types that should
#'               be evaluated in comparison to visit & birth dates
#'
#'               see ?ndq::dp_input_omop for more details
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param visit_tbl the CDM table with visit identifiers and dates; defaults to `cdm_tbl(visit_occurrence)`
#' @param dob_tbl the CDM table with patient identifiers and birth dates; defaults to `cdm_tbl(person)`
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `dp`
#'
#' @returns a table with the count & proportion of rows with an implausible date value,
#'          meaning that it fell before the associated visit start date, after the associated
#'          visit end date, or before the patient birth date
#'
#' @export
#'
check_dp <- function(dp_tbl,
                     omop_or_pcornet = 'omop',
                     visit_tbl = cdm_tbl('visit_occurrence'),
                     dob_tbl = cdm_tbl('person'),
                     check_string = 'dp'){

  site_nm <- config('qry_site')

  if(tolower(omop_or_pcornet) == 'omop'){
    person_col <- 'person_id'
    visit_col <- 'visit_occurrence_id'
    visit_start <- 'visit_start_date'
    visit_end <- 'visit_end_date'
    dob <- 'birth_datetime'
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    person_col <- 'patid'
    visit_col <- 'encounterid'
    visit_start <- 'admit_date'
    visit_end <- 'discharge_date'
    dob <- 'birth_date'
  }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

  date_tbls <- split(dp_tbl, seq(nrow(dp_tbl)))

  date_rslt <- list()

  for(i in 1:length(date_tbls)){

    message(paste0('Starting ', date_tbls[[i]]$check_description))

    if(!is.na(date_tbls[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = date_tbls[[i]]$schema,
                             table = date_tbls[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(date_tbls[[i]]$filter_logic)) %>%
        add_site() %>% filter(site == site_nm)
    }else{
      tbl_use <- pick_schema(schema = date_tbls[[i]]$schema,
                             table = date_tbls[[i]]$table,
                             db = config('db_src')) %>%
        add_site() %>% filter(site == site_nm)
    }

    ## total records
    total_records <- tbl_use %>%
      group_by(site) %>%
      summarise(total_rows = n()) %>%
      collect()

    ## Event is prior to visit start date
    visit_start_measure <- visit_tbl %>%
      select(!!sym(visit_col), !!sym(visit_start)) %>%
      left_join(tbl_use %>% select(site, !!sym(visit_col), !!sym(date_tbls[[i]]$date_field))) %>%
      filter(!!sym(date_tbls[[i]]$date_field) < !!sym(visit_start)) %>%
      group_by(site) %>%
      summarise(before_visit_start = n()) %>% collect()

    if(nrow(visit_start_measure) < 1){
      visit_start_measure <- tibble('site' = site_nm,
                                    'before_visit_start' = 0)
    }

    ## Event is after visit end date
    visit_end_measure <- visit_tbl %>%
      select(!!sym(visit_col), !!sym(visit_end)) %>%
      left_join(tbl_use %>% select(site, !!sym(visit_col), !!sym(date_tbls[[i]]$date_field))) %>%
      filter(!!sym(date_tbls[[i]]$date_field) > !!sym(visit_end)) %>%
      group_by(site) %>%
      summarise(after_visit_end = n()) %>% collect()

    if(nrow(visit_end_measure) < 1){
      visit_end_measure <- tibble('site' = site_nm,
                                   'after_visit_end' = 0)
    }

    ## Event is prior to birth date
    dob_tbl_use <- dob_tbl %>%
      select(!!sym(person_col), !!sym(dob)) %>%
      mutate(dob_use = as.Date(!!sym(dob)))

    birth_date_measure <- tbl_use %>%
      select(site, !!sym(person_col), !!sym(date_tbls[[i]]$date_field)) %>%
      left_join(dob_tbl_use) %>%
      filter(dob_use > !!sym(date_tbls[[i]]$date_field)) %>%
      group_by(site) %>%
      summarise(before_birth_date = n()) %>% collect()

    if(nrow(birth_date_measure) < 1){
      birth_date_measure <- tibble('site' = site_nm,
                                   'before_birth_date' = 0)
    }

    ## build table
    combo_tbl <- total_records %>%
      left_join(visit_start_measure) %>%
      left_join(visit_end_measure) %>%
      left_join(birth_date_measure) %>%
      pivot_longer(cols = !c('site', 'total_rows'),
                   names_to = 'implausible_type',
                   values_to = 'implausible_row') %>%
      mutate(prop_implausible = as.numeric(implausible_row) / as.numeric(total_rows),
             prop_implausible = round(prop_implausible, 4),
             implausible_row = ifelse(is.na(implausible_row), 0, implausible_row),
             prop_implausible = ifelse(is.na(prop_implausible), 0, prop_implausible),
             check_name = paste0(check_string, '_', date_tbls[[i]]$check_id),
             check_description = date_tbls[[i]]$check_description)

    date_rslt[[i]] <- combo_tbl

  }

  date_red <- purrr::reduce(.x = date_rslt,
                            .f = dplyr::union)

  return(date_red)
}


#' Date Plausibility -- Processing
#'
#' Intakes the output of check_dp in order to apply additional processing. This
#' includes creating a new check_name_app column to specify that the check
#' was computed at the row level.
#'
#' @param dp_results table output by check_dp
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @returns same input table with additional check_name_app column to indicate application level
#'
#' @export
#'
process_dp <- function(dp_results,
                       rslt_source = 'remote',
                       csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    dp_int <- results_tbl(dp_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    dp_int <- readr::read_csv(paste0(csv_rslt_path, dp_results))
  }else if(tolower(rslt_source) == 'local'){
    dp_int <- dp_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  dp_final <- dp_int %>%
    mutate(check_name_app = paste0(check_name, '_rows'))

  return(dp_final)
}
