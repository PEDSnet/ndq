
#' Facts Over Time
#'
#' This function will compute the number of rows, patients, and (optionally) visits associated with
#' the fact of interest within a specified time period. The user will supply the end points of the time span
#' (i.e. January 2009 - January 2024) and the time period they wish to divide it by (i.e. month, year).
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined;
#'                see `?fot_input_omop` or `?fot_input_pcornet` for details
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param compute_method a string input of either `loop` or `group`; controls whether the
#'                       check is executed by looping through each time period or grouping by a date field
#' @param time_span a list that contains the start date and end date of the time span (i.e. list(2009-01-01, 2015-01-01))
#' @param time_period string indicating the length of time that the time span should be divided into (i.e. months, years)
#' @param lookback_interval the number of time periods (defined in `time_period`) to look back in each interval
#'                          (i.e. 1 year, 3 months); defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients or rows
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return a dataframe with one row for each time period within the specified time span for each check;
#' - if visits_only = TRUE, will produce only counts of visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = TRUE, will produce counts of patients, rows, and visits for the check + time period
#' - if visits_only = FALSE and distinct_visits = FALSE, will produce counts of patients and rows for the check + time period
#'
#' @importFrom purrr reduce
#' @import lubridate
#' @importFrom rlang :=
#'
#' @export
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
check_fot_loop <- function(fot_tbl,
                           time_frame,
                           omop_or_pcornet = 'omop',
                           lookback_interval = 1,
                           check_string = 'fot',
                           visits_only = FALSE,
                           distinct_visits = TRUE) {

  site_nm <- config('qry_site')

  time_tbls <- split(fot_tbl, seq(nrow(fot_tbl)))

  final_results <- list()

  for(i in 1:length(time_tbls)) {

    cli::cli_inform(paste0('Starting ', time_tbls[[i]]$check_description))

    temp_results <- list()

    time_frame <- split(time_frame, seq(nrow(time_frame)))

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
      }else if(tolower(omop_or_pcornet) == 'pcornet'){
        visit_col <- 'encounterid'
        pt_col <- 'patid'
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
        add_site() %>% filter(site == site_nm) %>%
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
    }else if(tolower(omop_or_pcornet) == 'pcornet'){
      visit_col <- 'encounterid'
      pt_col <- 'patid'
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
      add_site() %>% filter(site == site_nm) %>%
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
      full_join(time_frame)

    fill_blanks <- visit_cts_filter %>%
      ungroup() %>%
      distinct(check_type, database_version, site, check_name, check_desc,
               domain) %>%
      cross_join(time_cj)

    final_results[[paste0(n)]] = visit_cts_filter %>%
      full_join(fill_blanks) %>%
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
#' to other time periods around it. For a montly computation, for example, the heuristic
#' would be `month / ((month-1 * .25) + (month+1 * .25) + (month-12 * .5))` which is
#' the value for a given month divided by the weighted average of the value in the previous
#' month, the next month, and the same month in the previous year. The function can also
#' optionally compute a ratio given a `total_pt` column (added by the user) to be used as a
#' denominator and a `ratio_mult` value to be used as a multiplier.
#'
#'
#' @param fot_results the table output by check_fot
#' @param target_col the numerical column in fot_results that should be used to
#'                   compute the heuristic; options are row_cts, row_pts, or row_visits
#' @param add_ratios boolean to indicate whether ratios / rates should be computed
#'                   if TRUE, the fot_results table should have an additional `total_pt`
#'                   column with counts for the denominator of the user's choosing
#'
#'                   for example, the count of patients in that time period that
#'                   had a visit
#' @param ratio_mult if add_ratios = TRUE, the numerical multiplier that should
#'                   be used to compute the rate
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @returns a list with up to three dataframes:
#'          `fot_heuristic`: the summary heuristic (month / ((month-1)*.25 +
#'                                                           (month+1)*.25 +
#'                                                           (month-12)*.5))
#'          `fot_heuristic_summary`: summary values (mean, med, sd, q1, q3) based on
#'                                   the heuristic
#'          and if add_ratios = TRUE
#'          `fot_ratios`: the fot_results table with an additional column with the
#'                        incidence ratio applied (as row_ratio)
#'
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @export
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
