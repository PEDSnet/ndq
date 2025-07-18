
#' Domain Concordance
#'
#' Given the details of a pair of clinical events, this function
#' will determine the count of patients OR visits that meet criteria for
#' the first event, the second event, and both events. Users can optionally
#' define a time limitation for the combined cohort, so patients/visits will only
#' count towards that cohort when the two events occur within a specified number of days
#' of each other.
#'
#' @param dcon_tbl table describing each cohort pair that should be examined;
#'                 there should be 2 rows per check, one for each cohort, with the same check_id
#'                 see `?dcon_input_omop` or `?dcon_input_pcornet` for details
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param compute_level string indicating the level at which the computation should be executed
#'                      accepted values are `patient` or `visit`
#' @param check_string the abbreviated name of the check; defaults to `dcon`
#'
#' @return a list with two dataframes:
#'         1. counts for the patients/visits in the first cohort, the patients/visits in the second cohort, and the patients/visits in both
#'         2. metadata information about each of the cohorts for which counts were computed
#'
#' @export
#'
check_dcon<- function(dcon_tbl,
                      compute_level = 'patient',
                      omop_or_pcornet = 'omop',
                      check_string='dcon'){

  site_nm <- config('qry_site')

  conc_tbls <- dplyr::group_split(dcon_tbl %>% collect() %>% group_by(check_id))

  final <- list()
  meta <- list()

  for(k in 1:length(conc_tbls)) {

    cli::cli_inform(paste0('Starting ', conc_tbls[[k]]$check_id %>% unique()))

    c1_date <- conc_tbls[[k]]$date_field[1]
    c2_date <- conc_tbls[[k]]$date_field[2]

    ## Cohort 1 Table
    if(!is.na(conc_tbls[[k]]$filter_logic[1])){
      tbl_use_c1 <- pick_schema(schema = conc_tbls[[k]]$schema[1],
                             table = conc_tbls[[k]]$table[1],
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(conc_tbls[[k]]$filter_logic[1]))
    }else{
      tbl_use_c1 <- pick_schema(schema = conc_tbls[[k]]$schema[1],
                             table = conc_tbls[[k]]$table[1],
                             db = config('db_src'))
    }

    if(!is.na(conc_tbls[[k]]$conceptset_name[1])){
      if(omop_or_pcornet == 'omop'){
        join_cols <- set_names('concept_id', conc_tbls[[k]]$concept_field[1])
      }else{
        join_cols <- set_names('concept_code', conc_tbls[[k]]$concept_field[1])
        join_cols2 <- set_names('vocabulary_id', conc_tbls[[k]]$vocabulary_field[1])
        join_cols <- join_cols %>% append(join_cols2)
      }

      tbl_use_c1 <- tbl_use_c1 %>%
        inner_join(load_codeset(conc_tbls[[k]]$conceptset_name[1]), by = join_cols)
    }

    ## Cohort 2 Table
    if(!is.na(conc_tbls[[k]]$filter_logic[2])){
      tbl_use_c2 <- pick_schema(schema = conc_tbls[[k]]$schema[2],
                             table = conc_tbls[[k]]$table[2],
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(conc_tbls[[k]]$filter_logic[2]))
    }else{
      tbl_use_c2 <- pick_schema(schema = conc_tbls[[k]]$schema[2],
                             table = conc_tbls[[k]]$table[2],
                             db = config('db_src'))
    }

    if(!is.na(conc_tbls[[k]]$conceptset_name[2])){
      if(omop_or_pcornet == 'omop'){
        join_cols <- set_names('concept_id', conc_tbls[[k]]$concept_field[2])
      }else{
        join_cols <- set_names('concept_code', conc_tbls[[k]]$concept_field[2])
        join_cols2 <- set_names('vocabulary_id', conc_tbls[[k]]$vocabulary_field[2])
        join_cols <- join_cols %>% append(join_cols2)
      }

      tbl_use_c2 <- tbl_use_c2 %>%
        inner_join(load_codeset(conc_tbls[[k]]$conceptset_name[2]), by = join_cols)
    }

    b1 <- conc_tbls[[k]]$cohort_id[1]
    b2 <- conc_tbls[[k]]$cohort_id[2]

    cohort_1 <- tbl_use_c1 %>% mutate(date1 = !!sym(c1_date),
                                      cohort_label = b1) %>%
      add_site() %>% filter(site == site_nm) %>% compute_new(name = 'temp_c1',
                                                             overwrite = TRUE)
    cohort_2 <- tbl_use_c2 %>% mutate(date2 = !!sym(c2_date),
                                      cohort_label = b2) %>%
      add_site() %>% filter(site == site_nm) %>% compute_new(name = 'temp_c2',
                                                             overwrite = TRUE)

    ## Combined cohort
    if(omop_or_pcornet == 'omop'){
      if(compute_level=='visit'){
        col_nm <- sym('visit_occurrence_id')
      } else{col_nm <- sym('person_id')}
    }else if(omop_or_pcornet == 'pcornet'){
      if(compute_level=='visit'){
        col_nm <- sym('encounterid')
      } else{col_nm <- sym('patid')}
    }

    time_bw_events <- conc_tbls[[k]]$time_between_events %>% unique()
    if(length(time_bw_events) > 1){cli::cli_abort('The time between events for each cohort in a given check must match.')}

    if(!is.na(time_bw_events)){

      days_diff_integer <- time_bw_events

      combined <-
        cohort_1 %>% select(site, all_of(col_nm), date1) %>%
        inner_join(
          select(cohort_2, site, all_of(col_nm), date2)
        ) %>%
        mutate(date_diff = sql(calc_days_between_dates(date_col_1 = 'date2',
                                                       date_col_2 = 'date1')),
               date_diff = abs(as.numeric(date_diff))) %>%
        filter(date_diff <= days_diff_integer) %>%
        mutate(cohort_label = 'combined')

    }else{

      combined <-
        cohort_1 %>% select(all_of(col_nm)) %>%
        inner_join(
          select(cohort_2, all_of(col_nm))
        ) %>% mutate(cohort_label = 'combined')

    }

    cohort_list <- list('cohort_1' = cohort_1,
                        'cohort_2' = cohort_2,
                        'combined' = combined)

    cohort_map <- tibble('cohort' = c('cohort_1', 'cohort_2', 'combined'),
                         'cohort_label' = c(conc_tbls[[k]]$cohort_id[1],
                                            conc_tbls[[k]]$cohort_id[2],
                                            'combined'))


    cohort_list_cts <- list()

    for(i in 1:length(cohort_list)) {

      string_nm <- names(cohort_list[i])
      cohort_nm <- cohort_map %>% filter(cohort %in% string_nm) %>%
        distinct(cohort_label) %>% pull()

      final_cts <- cohort_list[[i]] %>%
        summarise(value=n_distinct(col_nm)) %>%
        collect() %>%
        mutate(cohort = string_nm,
               cohort_label = cohort_nm)

      cohort_list_cts[[i]] <- final_cts

    }

    nm <- conc_tbls[[k]]$check_id %>% unique()
    d <- paste0(conc_tbls[[k]]$cohort_description[1], ' and ', conc_tbls[[k]]$cohort_description[2])

    final_tbls <-
      reduce(.x=cohort_list_cts,
             .f=dplyr::union) %>%
      #mutate(yr=9999) %>%
      add_meta(check_lib = 'dcon') %>%
      mutate(check_name=paste0(check_string, '_', nm),
             check_desc=d) %>% collect()

    meta_tbl <- tibble(check_type = c(compute_level, compute_level),
                       check_name = c(final_tbls$check_name[1], final_tbls$check_name[2]),
                       cohort = c(final_tbls$cohort[1], final_tbls$cohort[2]),
                       cohort_description = c(conc_tbls[[k]]$cohort_description[1], conc_tbls[[k]]$cohort_description[2]))

    final[[k]] <- final_tbls
    meta[[k]] <- meta_tbl

  }

  final_red <- purrr::reduce(.x = final,
                             .f = dplyr::union)
  meta_red <- purrr::reduce(.x = meta,
                            .f = dplyr::union)

  opt <- list('dcon_output' = final_red,
              'dcon_meta' = meta_red)

  return(opt)

}


#' Domain Concordance -- Processing
#'
#' Intakes the output of check_dcon in order to apply additional processing. This
#' includes computing the following cohort overlap counts and proportions:
#' - cohort_1_only: overall, patients in just cohort 1
#' - cohort_2_only: overall, patients in just cohort 2
#' - combined: overall, patients in both 1 and 2
#' - cohort_1_denom: patients in cohort 1 not cohort 2
#' - cohort_2_denom: patients in cohort 2 not cohort 1
#' - cohort_1_in_2: patients in cohort 2 who are also in 1 (use cohort 2 as denominator)
#' - cohort_2_in_1: patients in cohort 1 who are also in 2 (use cohort 1 as denominator)
#'
#' Note that cohort_1_in_2 and cohort_2_in_1 will have the same raw count, but different
#' proportions since the denominator is different
#'
#' @param dcon_results table output by check_dcon
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return a dataframe with one row for cohort overlap computation type listed in the description, with
#'         the associated raw count (of patients or visits) and the associated proportion
#'
#' @export
#'
process_dcon <- function(dcon_results,
                         rslt_source = 'remote',
                         csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    dcon_int <- results_tbl(dcon_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    dcon_int <- readr::read_csv(paste0(csv_rslt_path, dcon_results))
  }else if(tolower(rslt_source) == 'local'){
    dcon_int <- dcon_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  dcon_overall <- dcon_int %>%
    group_by(check_type, database_version, check_name, check_desc, cohort) %>%
    summarise(value=sum(value,na.rm=TRUE))%>%
    ungroup()%>%
    mutate(site='total')

  dcon_tbl_pp<-dcon_int %>%
    select(-cohort_label) %>%
    bind_rows(dcon_overall) %>%
    pivot_wider(values_from = value,
                names_from=cohort)%>%
    mutate(tot_pats=cohort_1+cohort_2-combined,
           cohort_1_only=cohort_1-combined,
           cohort_2_only=cohort_2-combined,
           cohort_1_in_2=combined,
           cohort_2_in_1=combined,
           # duplicative, but keeping around for proportion of 2 in 1 and vice versa
           cohort_1_denom=cohort_1-combined,
           cohort_2_denom=cohort_2-combined)%>%
    pivot_longer(cols=c(cohort_1_only, cohort_2_only, combined, cohort_2_in_1, cohort_1_in_2, cohort_1_denom, cohort_2_denom),
                 names_to="cohort",
                 values_to="value")%>%
    # cohort_1_only: overall, patients in just cohort 1
    # cohort_2_only: overall, patients in just cohort 2
    # combined: overall, patients in both 1 and 2
    # cohort_1_denom: patients in cohort 1 not cohort 2
    # cohort_2_denom: patients in cohort 2 not cohort 1
    # cohort_1_in_2: patients in cohort 2 who are also in 1 (use cohort 2 in denom)
    # cohort_2_in_1: patients in cohort 1 who are also in 2 (use cohort 1 in denom)
    # note that cohort_1_in_2 and cohort_2_in_1 will have the same raw number, but different proportions since the denominator is different
    mutate(prop=case_when(cohort%in%c('cohort_1_only', 'cohort_2_only', 'combined')~value/tot_pats,
                          cohort=='cohort_1_in_2'~value/cohort_2,
                          cohort=='cohort_2_in_1'~value/cohort_1,
                          cohort=='cohort_1_denom'~value/cohort_1,
                          cohort=='cohort_2_denom'~value/cohort_2))%>%
    # in case one of the cohort denominators is 0
    mutate(prop=case_when(is.na(prop)~0,
                          TRUE~prop))

  cohort_maps <- dcon_int %>% distinct(check_type, check_name, cohort, cohort_label)

  opt <- list('dcon_props' = dcon_tbl_pp %>% mutate(check_name_app=paste0(check_name,"_concordance")),
              'dcon_cohort_map' = cohort_maps)

  return(opt)
}

