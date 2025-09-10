
#' Facts Over Time
#'
#' This function will compute the number of rows, patients, and (optionally) visits associated with
#' the fact of interest within a specified time period. The user will supply the end points of the time span
#' (i.e. January 2009 - January 2024) and the time period they wish to divide it by (i.e. month, year).
#'
#' @param fot_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the clinical fact
#'  types that should be evaluated across the user-specified time span per each time period.
#'  see `?fot_input_omop` or `?fot_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param compute_method *string* || defaults to `loop`
#'
#'  A string, either `loop` or `group`, that controls whether the
#'  check is executed by looping through each time period or grouping by a date field to
#'  obtain counts. `group` is recommended for high performing database systems.
#'
#' @param time_span *list (length 2)* || defaults to `list('2009-01-01', today())`
#'
#'  A list that contains the start date and end date of the total time period of interest
#'
#' @param time_period *string* || defaults to `month`
#'
#'  A string indicating the length of each time period that should be examined, like months or years
#'
#' @param lookback_interval *integer* || defaults to `1`
#'
#'  An integer indicating the number of time periods (defined in `time_period`) to look back in each interval.
#'  For example, a `time_period` of `month` and a `lookback_interval` of `3` would produce quarterly counts.
#'
#' @param check_string *string* || defaults to `fot`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @param visits_only *boolean* || defaults to FALSE
#'
#'  If set to `TRUE`, then this function will only return visit counts and will not return patient
#'  or row counts. By default, visit, row, and patient counts are returned.
#'
#' @param distinct_visits *boolean* || defaults to TRUE
#'
#'  If set to `FALSE`, then this function will only return patient and row counts and will not return
#'  visit counts. By default, visit, row, and patient counts are returned.
#'
#' @return
#'
#'  This function will return a dataframe with one row for each time period within the
#'  specified time span for each check. Additionally:
#' - If `visits_only = TRUE`, only visit counts will be returned for the check + time period
#' - If `visits_only = FALSE` and `distinct_visits = TRUE`, patient, row, and visit counts will be returned for the check + time period
#' - If `visits_only = FALSE` and `distinct_visits = FALSE`, only patient and row counts will be returned for the check + time period
#'
#' @importFrom purrr reduce
#' @import lubridate
#' @importFrom rlang :=
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::fot_input_omop
#' ndq::fot_input_pcornet
#'
#' # Use this as your input to the FOT function
#' ## This check can be executed for different time period lengths, like...
#' ### Yearly
#' \dontrun{
#' my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
#'                          omop_or_pcornet = 'omop',
#'                          compute_method = 'loop', # use 'group' for high performant DBMSs
#'                          time_span = list('2015-01-01', '2025-01-01'),
#'                          time_period = 'year',
#'                          lookback_interval = 1,
#'                          check_string = 'fot')
#' }
#'
#' ### Monthly
#' \dontrun{
#' my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
#'                          omop_or_pcornet = 'omop',
#'                          compute_method = 'loop', # use 'group' for high performant DBMSs
#'                          time_span = list('2015-01-01', '2025-01-01'),
#'                          time_period = 'month',
#'                          lookback_interval = 1,
#'                          check_string = 'fot')
#' }
#'
#' ### Quarterly
#' \dontrun{
#' my_fot_rslt <- check_fot(fot_tbl = ndq::fot_input_omop,
#'                          omop_or_pcornet = 'omop',
#'                          compute_method = 'loop', # use 'group' for high performant DBMSs
#'                          time_span = list('2015-01-01', '2025-01-01'),
#'                          time_period = 'month',
#'                          lookback_interval = 3,
#'                          check_string = 'fot')
#' }
#'
check_fot <- function(fot_tbl,
                      omop_or_pcornet = 'omop',
                      compute_method = 'loop',
                      time_span = list('2009-01-01', today()),
                      time_period = 'month',
                      lookback_interval = 1,
                      check_string = 'fot',
                      visits_only = FALSE,
                      distinct_visits = TRUE) {

  if(tolower(time_period) == 'month'){
    num_mnths <- (interval(time_span[[1]], time_span[[2]]) %/% months(1))

    time_span_list_output <-
      as.character(seq(as.Date(time_span[[1]]), length = num_mnths,
                       by=paste0(lookback_interval, ' months')))

    time_frame <- tibble(time_span_list_output) %>%
      rename('time_end' = 'time_span_list_output') %>%
      mutate(time_end = ceiling_date(ymd(time_end), 'month') - 1,
             time_start = as.Date(time_end) %m-% months(lookback_interval),
             time_start = ceiling_date(time_start, 'month') - 1) %>%
      filter(time_start <= time_span[[2]])
  }else if(tolower(time_period) == 'year'){
    num_yrs <- (interval(time_span[[1]], time_span[[2]]) %/% years(1))

    time_span_list_output <-
      as.character(seq(as.Date(time_span[[1]]), length = num_yrs,
                       by= paste0(lookback_interval, ' years')))

    time_frame <- tibble(time_span_list_output) %>%
      rename('time_end' = 'time_span_list_output') %>%
      mutate(time_end = ceiling_date(ymd(time_end), 'year') - 1,
             time_start = as.Date(time_end) %m-% years(lookback_interval)) %>%
      filter(time_start <= time_span[[2]])
  }


  if(tolower(compute_method) == 'group'){

    fot_rslt <- check_fot_group(fot_tbl = fot_tbl,
                                omop_or_pcornet = omop_or_pcornet,
                                time_frame = time_frame,
                                lookback_interval = lookback_interval,
                                check_string = check_string,
                                visits_only = visits_only,
                                distinct_visits = distinct_visits)

    # cli::cli_abort('`group` method still under development')

  }else if(tolower(compute_method) == 'loop'){

    fot_rslt <- check_fot_loop(fot_tbl = fot_tbl,
                               omop_or_pcornet = omop_or_pcornet,
                               time_frame = time_frame,
                               lookback_interval = lookback_interval,
                               check_string = check_string,
                               visits_only = visits_only,
                               distinct_visits = distinct_visits)

  }else{cli::cli_abort('Invalid compute_method. Please select either `group` or `loop`')}

  return(fot_rslt)

}

#' Facts Over Time (Original Postgres Implementation)
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined;
#'                see `?fot_input_omop` or `?fot_input_pcornet` for details
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param time_frame a table of dates that should be iterated through to retrieve the facts for each time period;
#'                   has columns: time_start, time_end
#' @param lookback_interval the number of time periods (defined in check_fot) to look back;
#'                          defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients or rows
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return a dataframe with one row for each time period within the specified time span for each check;
#' - if visits_only = TRUE, will produce only counts of visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = TRUE, will produce counts of patients, rows, and visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = FALSE, will produce counts of patients and rows for the check + time period
#'
#' @keywords internal
#'
check_fot_loop <- function(fot_tbl,
                           time_frame,
                           omop_or_pcornet = 'omop',
                           lookback_interval = 1,
                           check_string = 'fot',
                           visits_only = FALSE,
                           distinct_visits = TRUE) {

  site_nm <- config('qry_site')

  time_tbls <- split(fot_tbl, seq(nrow(fot_tbl)))
  time_frame <- split(time_frame, seq(nrow(time_frame)))

  final_results <- list()

  for(i in 1:length(time_tbls)) {

    cli::cli_inform(paste0('Starting ', time_tbls[[i]]$check_description))

    temp_results <- list()

    for(k in 1:length(time_frame)) {

      message(paste0('Starting ',time_frame[[k]]$time_end))

      baseline_start_date <- time_frame[[k]]$time_start
      baseline_end_date <- time_frame[[k]]$time_end

      # target <- ymd(k)

      # baseline_end_date <- target
      # if(lookback_weeks == 0) {
      #   baseline_start_date <- target %m-% months(lookback_months)
      # } else {baseline_start_date <- target - weeks(x=lookback_weeks)}

      if(tolower(omop_or_pcornet) == 'omop'){
        visit_col <- 'visit_occurrence_id'
        pt_col <- 'person_id'
        pt_tbl <- 'person'
      }else if(tolower(omop_or_pcornet) == 'pcornet'){
        visit_col <- 'encounterid'
        pt_col <- 'patid'
        pt_tbl <- 'demographic'
      }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

      date_cols <- time_tbls[[i]]$date_field

      # order_cols <- ncol(date_cols)
      #
      # date_cols_unmapped <-
      #   date_cols %>%
      #   select(all_of(order_cols))

      colname_string <- date_cols

      if(!is.na(time_tbls[[i]]$filter_logic)){
        tbl_use <- pick_schema(schema = time_tbls[[i]]$schema,
                               table = time_tbls[[i]]$table,
                               db = config('db_src')) %>%
          filter(!! rlang::parse_expr(time_tbls[[i]]$filter_logic))
      }else{
        tbl_use <- pick_schema(schema = time_tbls[[i]]$schema,
                               table = time_tbls[[i]]$table,
                               db = config('db_src'))
      }

      visits_narrowed <-
        tbl_use %>%
        add_site(site_tbl = cdm_tbl(pt_tbl),
                 id_col = pt_col) %>% filter(site == site_nm) %>%
        filter(!! sym(colname_string) <= baseline_end_date &
                 !! sym(colname_string) > baseline_start_date)

      n <- paste0(check_string, '_', time_tbls[[i]]$check_id)
      d <- time_tbls[[i]]$check_description
      t <- time_tbls[[i]]$table

      if(visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_visits = n_distinct(!!sym(visit_col))) %>%
          collect() %>%
          ungroup()  %>%
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>%
          mutate(check_desc = d,
                 domain = t)
      } else if(distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_visits = n_distinct(!!sym(visit_col)),
                    row_pts = n_distinct(!!sym(pt_col))) %>%
          collect() %>%
          ungroup()  %>%
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>%
          mutate(check_desc = d,
                 domain = t)
      } else if(!distinct_visits & !visits_only) {
        visit_cts <-
          visits_narrowed %>%
          summarise(row_cts = n(),
                    row_pts = n_distinct(!!sym(pt_col))) %>%
          collect() %>%
          ungroup()  %>%
          add_meta(check_lib=check_string) %>%
          mutate(check_name = n) %>%
          mutate(check_desc = d,
                 domain = t)
      }


      this_round <- visit_cts %>%
        mutate(time_end = baseline_end_date,
               time_start = ymd(baseline_start_date) + 1)

      temp_results[[k]] <- this_round

    }

    final_results[[paste0(n)]] = purrr::reduce(.x=temp_results, .f=union)
  }

  final_results_red <- purrr::reduce(.x = final_results,
                                     .f = dplyr::union)

  return(final_results_red)

}


#' Facts Over Time (Trino/Snowflake Implementation)
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined;
#'                see `?fot_input_omop` or `?fot_input_pcornet` for details
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param time_frame a table of dates that should be iterated through to retrieve the facts for each time period;
#'                   has columns: time_start, time_end
#' @param lookback_interval the number of time periods (defined in check_fot) to look back;
#'                          defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients or rows
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return a dataframe with one row for each time period within the specified time span for each check;
#' - if visits_only = TRUE, will produce only counts of visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = TRUE, will produce counts of patients, rows, and visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = FALSE, will produce counts of patients and rows for the check + time period
#'
#' @importFrom tidyr replace_na
#'
#' @keywords internal
#'
check_fot_group <- function(fot_tbl,
                            omop_or_pcornet = 'omop',
                            time_frame,
                            lookback_interval = 1,
                            check_string = 'fot',
                            visits_only = TRUE,
                            distinct_visits = TRUE) {

  site_nm <- config('qry_site')

  time_tbls <- split(fot_tbl, seq(nrow(fot_tbl)))

  final_results <- list()

  for(i in 1:length(time_tbls)) {

    cli::cli_inform(paste0('Starting ',time_tbls[[i]]$check_description))

    start_date <- time_frame %>% filter(time_start == min(time_start)) %>% pull(time_start)
    end_date <- time_frame %>% filter(time_end == max(time_end)) %>% pull(time_end)

    if(tolower(omop_or_pcornet) == 'omop'){
      visit_col <- 'visit_occurrence_id'
      pt_col <- 'person_id'
      pt_tbl <- 'person'
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      visit_col <- 'encounterid'
      pt_col <- 'patid'
      pt_tbl <- 'demographic'
    }else{cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')}

    date_cols <- time_tbls[[i]]$date_field

    # order_cols <- ncol(date_cols)
    #
    # date_cols_unmapped <-
    #   date_cols %>%
    #   select(all_of(order_cols))

    colname_string <- as.character(date_cols)

    if(!is.na(time_tbls[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = time_tbls[[i]]$schema,
                             table = time_tbls[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(time_tbls[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = time_tbls[[i]]$schema,
                             table = time_tbls[[i]]$table,
                             db = config('db_src'))
    }

    visits_narrowed <-
      tbl_use %>%
      add_site(site_tbl = cdm_tbl(pt_tbl),
               id_col = pt_col) %>% filter(site == site_nm) %>%
      filter(!! sym(colname_string) <= end_date &
               !! sym(colname_string) >= start_date)

    n <- paste0(check_string, '_', time_tbls[[i]]$check_id)
    d <- time_tbls[[i]]$check_description
    t <- time_tbls[[i]]$table

    if(visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(!!sym(colname_string)) %>%
        summarise(row_visits = n_distinct(!!sym(visit_col))) %>%
        collect() %>%
        ungroup()  %>%
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>%
        mutate(check_desc = d,
               domain = t)

      visit_cts_filter <- visit_cts %>%
        cross_join(time_frame) %>%
        filter(!!sym(colname_string) > time_start,
               !!sym(colname_string) <= time_end) %>%
        mutate(time_start = ymd(time_start) + 1) %>%
        group_by(check_type, database_version, site, check_name, check_desc,
                 domain, time_end, time_start) %>%
        summarise(row_visits = sum(row_visits))

    } else if(distinct_visits & !visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(!!sym(colname_string)) %>%
        summarise(row_cts = n(),
                  row_visits = n_distinct(!!sym(visit_col)),
                  row_pts = n_distinct(!!sym(pt_col))) %>%
        collect() %>%
        ungroup()  %>%
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>%
        mutate(check_desc = d,
               domain = t)

      visit_cts_filter <- visit_cts %>%
        cross_join(time_frame) %>%
        filter(!!sym(colname_string) > time_start,
               !!sym(colname_string) <= time_end) %>%
        mutate(time_start = ymd(time_start) + 1) %>%
        group_by(check_type, database_version, site, check_name, check_desc,
                 domain, time_end, time_start) %>%
        summarise(row_cts = sum(row_cts),
                  row_pts = sum(row_pts),
                  row_visits = sum(row_visits))

    } else if(!distinct_visits & !visits_only) {
      visit_cts <-
        visits_narrowed %>%
        group_by(!!sym(colname_string)) %>%
        summarise(row_cts = n(),
                  row_pts = n_distinct(!!sym(pt_col))) %>%
        collect() %>%
        ungroup()  %>%
        add_meta(check_lib=check_string) %>%
        mutate(check_name = n) %>%
        mutate(check_desc = d,
               domain = t)

      visit_cts_filter <- visit_cts %>%
        cross_join(time_frame) %>%
        filter(!!sym(colname_string) > time_start,
               !!sym(colname_string) <= time_end) %>%
        mutate(time_start = ymd(time_start) + 1) %>%
        group_by(check_type, database_version, site, check_name, check_desc,
                 domain, time_end, time_start) %>%
        summarise(row_cts = sum(row_cts),
                  row_pts = sum(row_pts))
    }

    time_cj <- visit_cts_filter %>%
      ungroup() %>%
      distinct(time_end, time_start) %>%
      union(time_frame %>% mutate(time_start = time_start + 1))

    fill_blanks <- visit_cts_filter %>%
      ungroup() %>%
      distinct(check_type, database_version, site, check_name, check_desc,
               domain) %>%
      cross_join(time_cj)

    final_results[[paste0(n)]] = visit_cts_filter %>%
      ungroup() %>%
      right_join(fill_blanks) %>%
      mutate(across(where(is.numeric), .fns = ~replace_na(.,0)))

  }

  final_results_red <- purrr::reduce(.x = final_results,
                                     .f = dplyr::union)

  return(final_results_red)

}



# This function calculates the heuristic used in the FOT dq check
# The heuristic is:
# month / ((month-1)*.25 +
#          (month+1)*.25 +
#          (month-12)*.5)
# In plain words, its the current month divided by the weighted average of the
# previous month, the next month, and the value from the current month in the
# previous year
fot_check_calc <- function(tblx,
                           site_col,
                           time_col,
                           target_col) {
  tblx %>%
    collect() %>%
    # window_order(!!sym(site_col),!!sym(time_col)) %>%
    arrange(!!sym(site_col),!!sym(time_col)) %>%
    mutate(
      lag_1 = lag(!!sym(target_col)),
      lag_1_plus = lead(!!sym(target_col),1),
      lag_12 = lag(!!sym(target_col),12),
      check_denom = (lag_1*.25 +
                     lag_1_plus*.25 +
                     lag_12*.5)) %>%
    filter(check_denom!=0) %>%
    mutate(check = !!sym(target_col)/check_denom-1)
}


# Compute heuristic
fot_check <- function(target_col,
                      tblx,
                      check_col='check_name',
                      check_desc='check_desc',
                      site_col='site',
                      time_col='time_end') {

  cols_to_keep <- c('domain',eval(site_col),eval(check_col),eval(check_desc),eval(time_col),'check')

  rv <- FALSE
  rv_agg <- FALSE
  #base tbl to make a network wide version of the check
  agg_check <- tblx %>% group_by(domain, !!sym(time_col),!!sym(check_col), !!sym(check_desc)) %>%
    summarise(!! target_col := sum(!!sym(target_col))) %>%
    ungroup() %>%
    mutate(!! site_col :='all')

  for (target_check in tblx %>% select(!!sym(check_col)) %>% distinct() %>% pull()) {
    for (target_site in tblx %>% select(!!sym(site_col)) %>% distinct() %>% pull()) {
      foo <- fot_check_calc(tblx %>%
                              filter(check_name==target_check & site==target_site),
                            site_col='site',
                            time_col,
                            target_col) %>% collect()
      if(!is.logical(rv)){
        rv <- union(rv, foo)
      } else {
        rv <- foo
      }
    }
    bar <- fot_check_calc(agg_check %>% filter(check_name==target_check),
                          site_col,time_col,target_col) %>%
      select(cols_to_keep) %>% collect()
    if(!is.logical(rv_agg)){
      rv_agg <- union(rv_agg, bar)
    } else {
      rv_agg <- bar
    }
  }

  #summarise the checks across sites
  rv_summary <- rv %>% group_by(domain, !!sym(check_col), !!sym(site_col)) %>%
    summarise(std_dev = sd(check,na.rm=TRUE),
              pct_25 = quantile(check,.25),
              pct_75 = quantile(check,.75),
              med = median(check),
              m = mean(check)) %>% ungroup() %>% collect()

  rv_summary_allsites <- rv_agg %>%
    filter(site=='all') %>% group_by(domain, !!sym(check_col), !!sym(site_col)) %>%
    summarise(std_dev = sd(check,na.rm=TRUE),
              pct_25 = quantile(check,.25),
              pct_75 = quantile(check,.75),
              med = median(check),
              m = mean(check)) %>% ungroup() %>% collect() %>%
    mutate(site='all')


  return(list(fot_heuristic_pp= dplyr::union(rv %>% select(cols_to_keep),
                                             rv_agg),
              fot_heuristic_summary_pp=dplyr::union(rv_summary,
                                                    rv_summary_allsites)))
}


add_fot_ratios <- function(fot_lib_output,
                           denom_mult){

  fot_input_tbl<-fot_lib_output %>%
    mutate(row_ratio=case_when(row_pts==0|total_pt==0~0,
                               TRUE~row_pts/(total_pt)*denom_mult))%>%collect()

  fot_input_tbl_allsite_med<-fot_input_tbl%>%
    group_by(check_type, check_name, check_desc, database_version, time_end) %>%
    summarise(row_ratio=median(row_ratio, na.rm=TRUE))%>%
    ungroup()%>%
    mutate(site='allsite_median')

  fot_input_tbl_allsite_mean<-fot_input_tbl%>%
    group_by(check_type, check_name, check_desc, database_version, time_end) %>%
    summarise(row_ratio=mean(row_ratio, na.rm=TRUE))%>%
    ungroup()%>%
    mutate(site='allsite_mean')

  bind_rows(fot_input_tbl,
            fot_input_tbl_allsite_med)%>%
    bind_rows(fot_input_tbl_allsite_mean)

}




#' Facts Over Time -- Processing
#'
#' Intakes the output of check_fot in order to apply additional processing. This
#' includes applying a heuristic meant to compare the fact count in a given time period
#' to other time periods around it. For a monthly computation, for example, the heuristic
#' would be `month / ((month-1 * .25) + (month+1 * .25) + (month-12 * .5))` which is
#' the value for a given month divided by the weighted average of the value in the previous
#' month, the next month, and the same month in the previous year. The function can also
#' optionally compute a ratio given a `total_pt` column (added by the user) to be used as a
#' denominator and a `ratio_mult` value to be used as a multiplier.
#'
#' @param fot_results *tabular input* || **required**
#'
#'  The tabular output of `check_fot`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param target_col *string* || defaults to `row_cts`
#'
#'  A string indicating the column in `fot_results` that should be used to
#'  compute the primary heuristic. For the ratio computation, `row_pts` will
#'  *always* be used regardless of what is specified in this parameter.
#'
#' @param add_ratios *boolean* || defaults to `FALSE`
#'
#'  A boolean indicating whether incidence ratios / rates should be computed.
#'  if TRUE, the user should edit the `fot_results` table to include an additional `total_pt`
#'  column with counts for the denominator of the user's choosing. We recommend including
#'  the desired denominator population as a check passed into the `check_fot` function
#'  to facilitate the computation of patient counts for each time period.
#'
#' @param ratio_mult *integer* || defaults to `10,000`
#'
#'  If `add_ratios` is set to  `TRUE`, this parameter should represent the numerical
#'  multiplier that should be used to compute the incidence ratio. This will result in
#'  a ratio that represents the fact rate per (`ratio_mult`) patients.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `fot_results` table.
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
#'  This function will return a list of up to three dataframes:
#'  - `fot_heuristic`: a table with the summary heuristic  for each fact type
#'  (month / ((month-1) * .25 +
#'  (month+1) * .25 +
#'  (month-12)*.5))
#'  - `fot_heuristic_summary`: a table with summary values (mean, med, sd, q1, q3) based on
#'  the heuristic
#'  - `fot_ratios`: the `fot_results` table with an additional column with the
#'  incidence ratio (only returned if `row_ratio = TRUE`)
#'
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @export
#'
#' @examples
#' # This function should be run after check_fot has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_fot_process <- process_fot(fot_results = my_fot_rslts,
#'                               target_col = 'row_cts',
#'                               rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
#'                               target_col = 'row_cts',
#'                               rslt_source = 'csv',
#'                               csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
#'                               target_col = 'row_cts',
#'                               rslt_source = 'remote')
#' }
#'
#' # You can also optionally compute patient incidence ratios. This computation will use
#' # the patient count, regardless of what is specified in the `target_col` argument.
#' # A column with the total patient count for that time period will need to be added to
#' # the results output. It can reflect any patient cohort, so long as the column is called
#' # `total_pt`. We recommend computing the denominator as part of the initial check_fot execution
#' # and extracting the counts from the resulting output.
#'
#' \dontrun{
#' my_fot_process <- process_fot(fot_results = 'my_fot_rslts',
#'                               target_col = 'row_cts',
#'                               add_ratios = TRUE,
#'                               ratio_mult = 10000,
#'                               rslt_source = 'remote')
#' }
#'
process_fot <- function(fot_results,
                        target_col = 'row_cts',
                        add_ratios = FALSE,
                        ratio_mult = 10000,
                        rslt_source = 'remote',
                        csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    fot_int <- results_tbl(fot_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    fot_int <- readr::read_csv(paste0(csv_rslt_path, fot_results))
  }else if(tolower(rslt_source) == 'local'){
    fot_int <- fot_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}



  fot_list <- fot_check(target_col = target_col,
                        tblx = fot_int,
                        check_col='check_name',
                        check_desc='check_desc',
                        site_col='site',
                        time_col='time_end')

  if(add_ratios){

    fot_ratios <- add_fot_ratios(fot_lib_output = fot_int,
                                 denom_mult = ratio_mult)

    fot_list[['fot_ratios']] <- fot_ratios

  }

  return(fot_list)

}
