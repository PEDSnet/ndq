
#' Date Plausibility
#'
#' This function will iterate through the provided input table to identify
#' the proportion of rows in each fact type that have an implausible date.
#' Implausibility is defined as a date that falls before the associated visit
#' start date, after the associated visit end date, before the patient's
#' birth date, or after the patient's death date + the defined buffer period.
#'
#' @param dp_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the clinical fact
#'  types that should be evaluated for date plausibility.
#'  see `?dp_input_omop` or `?dp_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param visit_tbl *tabular input* || defaults to `cdm_tbl(visit_occurrence)`
#'
#'  A table with visit information, including identifiers that can link back to facts
#'  and start & end dates. This argument will most likely be the `visit_occurrence` table for OMOP or
#'  the `encounter` table for PCORnet. Custom tables with specific visit subsets
#'  (i.e. a table with only nephrology specialist visits) can also be provided.
#'
#' @param dob_tbl *tabular input* || defaults to `cdm_tbl('person')`
#'
#'  A table with patient identifiers that can link back to facts and birth dates. This argument
#'  will most likely be the `person` table for OMOP or the `demographic` table for PCORnet.
#'
#' @param death_tbl *tabular input* || defaults to `cdm_tbl('death')`
#'
#'  A table with patient identifiers that can link back to facts and death dates. This argument
#'  will most likely be the `death` table for OMOP or the `death` table for PCORnet.
#'
#' @param post_death_buffer *integer* || defaults to `30`
#'
#'  An integer reflecting the number of days after death where the function should begin
#'  counting facts as implausible. This allows the user to capture autopsy results
#'  or other expected post-death facts to occur within the plausible window.
#'
#' @param check_string *string* || defaults to `dp`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @returns
#'
#'  This function will return a table with the count & proportion of rows with an
#'  implausible date value, with one column for each of the 3 implausibility definitions.
#'  These will indicate the proportion of facts falling before the associated visit start date,
#'  after the associated visit end date, or before the patient birth date
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::dp_input_omop
#' ndq::dp_input_pcornet
#'
#' # Use this as your input to the DP function
#' \dontrun{
#' my_dp_rslt <- check_dp(dp_tbl = ndq::dp_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        visit_tbl = cdm_tbl('visit_occurrence'), ## table with visit/encounter dates
#'                        dob_tbl = cdm_tbl('person'), ## table with dates of birth
#'                        check_string = 'dp')
#' }
#'
check_dp <- function(dp_tbl,
                     omop_or_pcornet = 'omop',
                     visit_tbl = cdm_tbl('visit_occurrence'),
                     dob_tbl = cdm_tbl('person'),
                     death_tbl = cdm_tbl('death'),
                     post_death_buffer = 30L,
                     check_string = 'dp'){

  site_nm <- config('qry_site')

  if(tolower(omop_or_pcornet) == 'omop'){
    person_col <- 'person_id'
    pt_tbl <- 'person'
    visit_col <- 'visit_occurrence_id'
    visit_start <- 'visit_start_date'
    visit_end <- 'visit_end_date'
    dob <- 'birth_datetime'
    death <- 'death_date'
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    person_col <- 'patid'
    pt_tbl <- 'demographic'
    visit_col <- 'encounterid'
    visit_start <- 'admit_date'
    visit_end <- 'discharge_date'
    dob <- 'birth_date'
    death <- 'death_date'
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
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = person_col) %>% filter(site == site_nm)
    }else{
      tbl_use <- pick_schema(schema = date_tbls[[i]]$schema,
                             table = date_tbls[[i]]$table,
                             db = config('db_src')) %>%
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = person_col) %>% filter(site == site_nm)
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
      summarise(previs = n()) %>% collect()

    if(nrow(visit_start_measure) < 1){
      visit_start_measure <- tibble('site' = site_nm,
                                    'previs' = 0)
    }

    ## Event is after visit end date
    visit_end_measure <- visit_tbl %>%
      select(!!sym(visit_col), !!sym(visit_end)) %>%
      left_join(tbl_use %>% select(site, !!sym(visit_col), !!sym(date_tbls[[i]]$date_field))) %>%
      filter(!!sym(date_tbls[[i]]$date_field) > !!sym(visit_end)) %>%
      group_by(site) %>%
      summarise(postvis = n()) %>% collect()

    if(nrow(visit_end_measure) < 1){
      visit_end_measure <- tibble('site' = site_nm,
                                  'postvis' = 0)
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
      summarise(prebirth = n()) %>% collect()

    if(nrow(birth_date_measure) < 1){
      birth_date_measure <- tibble('site' = site_nm,
                                   'prebirth' = 0)
    }

    ## Event is (post_death_buffer) days after death
    death_measure <- death_tbl %>%
      select(!!sym(person_col), !!sym(death)) %>%
      left_join(tbl_use %>% select(site, !!sym(person_col), !!sym(date_tbls[[i]]$date_field))) %>%
      filter(!!sym(date_tbls[[i]]$date_field) > !!sym(death)) %>%
      mutate(time_bw_events = sql(calc_days_between_dates(date_tbls[[i]]$date_field, death))) %>%
      filter(abs(time_bw_events) > post_death_buffer) %>%
      group_by(site) %>%
      summarise(postdeath = n()) %>% collect()

    if(nrow(death_measure) < 1){
      death_measure <- tibble('site' = site_nm,
                              'postdeath' = 0)
    }

    ## build table
    combo_tbl <- total_records %>%
      left_join(visit_start_measure) %>%
      left_join(visit_end_measure) %>%
      left_join(birth_date_measure) %>%
      left_join(death_measure) %>%
      pivot_longer(cols = !c('site', 'total_rows'),
                   names_to = 'implausible_type',
                   values_to = 'implausible_row') %>%
      mutate(prop_implausible = as.numeric(implausible_row) / as.numeric(total_rows),
             prop_implausible = round(prop_implausible, 4),
             implausible_row = ifelse(is.na(implausible_row), 0, implausible_row),
             prop_implausible = ifelse(is.na(prop_implausible), 0, prop_implausible),
             check_name = paste0(check_string, '_', date_tbls[[i]]$check_id, '-', implausible_type),
             check_description = date_tbls[[i]]$check_description,
             implausible_type = case_when(implausible_type == 'previs' ~ 'Before Visit Start',
                                          implausible_type == 'postvis' ~ 'After Visit End',
                                          implausible_type == 'prebirth' ~ 'Before Patient Birth',
                                          implausible_type == 'postdeath' ~ paste0('After Patient Death \n(With ',
                                                                                   post_death_buffer, ' Day Buffer)')))

    date_rslt[[i]] <- combo_tbl

  }

  date_red <- purrr::reduce(.x = date_rslt,
                            .f = dplyr::union)

  return(date_red)
}


#' Date Plausibility -- Processing
#'
#' Intakes the output of `check_dp` in order to apply additional processing. This
#' includes creating a new `check_name_app` column to specify that the check
#' was computed at the row level.
#'
#' @param dp_results *tabular input* || **required**
#'
#'  The tabular output of `check_dp`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `dp_results` table.
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
#'  This function will return the `dp_results` table with additional
#'  `check_name_app` column to indicate application level
#'
#' @export
#'
#' @examples
#' # This function should be run after check_dp has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' ## When results are kept locally:
#' \dontrun{
#' my_dp_process <- process_dp(dp_results = my_dp_rslts,
#'                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_dp_process <- process_dp(dp_results = 'my_dp_rslts',
#'                             rslt_source = 'csv',
#'                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_dp_process <- process_dp(dp_results = 'my_dp_rslts',
#'                             rslt_source = 'remote')
#' }
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

  # compute overall proportion
  dp_overall <- dp_int %>%
    group_by(implausible_type, check_name, check_description) %>%
    summarise(total_rows=sum(total_rows),
              implausible_row=sum(implausible_row)) %>%
    ungroup()%>%
    mutate(site = 'total',
           prop_implausible = implausible_row/total_rows) %>% collect()

  # bring together total and sites
  dp_final <- dp_int %>%
    dplyr::bind_rows(dp_overall) %>%
    mutate(check_name_app=paste0(check_name, "_rows"),
           check_type='dp')

  return(dp_final)
}
