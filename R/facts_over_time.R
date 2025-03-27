
#' Facts Over Time
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param compute_method either loop or group; controls whether the check is executed by looping through each time period
#'                       or grouping by a date field
#' @param time_span a list that contains the start date and end date of the time span
#' @param time_period string indicating the length of time that the time span should be divided into; i.e. months, years
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer.
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
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
                      lookback_weeks=0,
                      lookback_months=1,
                      check_string = 'fot',
                      visits_only = FALSE,
                      distinct_visits = TRUE) {

  if(tolower(time_period) == 'month'){
    num_mnths <- (interval(time_span[[1]], time_span[[2]]) %/% months(1))

    time_span_list_output <-
      as.character(seq(as.Date(time_span[[1]]), length = num_mnths, by='months'))

    time_frame <- c(time_span_list_output)
  }else if(tolower(time_period) == 'year'){
    num_yrs <- (interval(time_span[[1]], time_span[[2]]) %/% years(1))

    time_span_list_output <-
      as.character(seq(as.Date(time_span[[1]]), length = num_yrs, by='years'))

    time_frame <- c(time_span_list_output)
  }


  if(tolower(compute_method) == 'group'){

    # fot_rslt <- check_fot_group(fot_tbl = fot_tbl,
    #                             omop_or_pcornet = omop_or_pcornet,
    #                             time_frame = time_frame,
    #                             lookback_weeks = lookback_weeks,
    #                             lookback_months = lookback_months,
    #                             check_string = check_string,
    #                             visits_only = visits_only,
    #                             distinct_visits = distinct_visits)

    cli::cli_abort('`group` method still under development')

  }else if(tolower(compute_method) == 'loop'){

    fot_rslt <- check_fot_loop(fot_tbl = fot_tbl,
                               omop_or_pcornet = omop_or_pcornet,
                               time_frame = time_frame,
                               lookback_weeks = lookback_weeks,
                               lookback_months = lookback_months,
                               check_string = check_string,
                               visits_only = visits_only,
                               distinct_visits = distinct_visits)

  }else{cli::cli_abort('Invalid compute_method. Please select either `group` or `loop`')}

  return(fot_rslt)

}

#' Facts Over Time (Original Postgres Implementation)
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param time_frame a list that contains the end date of every month to iterate through
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer.
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#'
check_fot_loop <- function(fot_tbl,
                           time_frame = time_span,
                           omop_or_pcornet = 'omop',
                           lookback_weeks=0,
                           lookback_months=1,
                           check_string = 'fot',
                           visits_only = FALSE,
                           distinct_visits = TRUE) {

  site_nm <- config('qry_site')

  time_tbls <- split(fot_tbl, seq(nrow(fot_tbl)))

  final_results <- list()

  for(i in 1:length(time_tbls)) {

    cli::cli_inform(paste0('Starting ', time_tbls[[i]]$check_description))

    temp_results <- list()

    for(k in time_frame) {

      message(paste0('Starting ',k))

      target <- ymd(k)

      baseline_end_date <- target
      if(lookback_weeks == 0) {
        baseline_start_date <- target %m-% months(lookback_months)
      } else {baseline_start_date <- target - weeks(x=lookback_weeks)}

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
        filter(!! sym(colname_string) < baseline_end_date &
                 !! sym(colname_string) >= baseline_start_date)

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


      if(! lookback_weeks) {
        this_round <- visit_cts %>%
          mutate(month_end = lubridate::date(k) - 1,
                 month_start = baseline_start_date)
      } else {this_round <- visit_cts %>%
        mutate(week_end = lubridate::date(k) - 1,
               week_start = baseline_start_date) }

      temp_results[[k]] <- this_round

    }

    final_results[[paste0(n)]] = purrr::reduce(.x=temp_results, .f=union)
  }

  final_results_red <- purrr::reduce(.x = final_results,
                                     .f = dplyr::union)

  return(final_results_red)

}



#' Facts Over Time (Optimized for Trino)
#'
#' @param fot_tbl a table with information describing the fact tables that should be examined
#' @param time_frame a list that contains at least end date of the first and last month of the desired time span
#' @param lookback_weeks if lookback is in weeks instead of months, this should be set to a non-zero integer.
#' Defaults to 0, for lookback to be in months.
#' @param lookback_months the number of months to look back; defaults to 1
#' @param check_string the abbreviated name of the check; defaults to `fot`
#' @param visits_only if TRUE, counts ONLY distinct visits and not patients
#' @param distinct_visits if TRUE, counts distinct visits as well as total counts and total patients
#'
#' @return table with the following rows (if `distinct_visits` = `TRUE`:
#'  `month_end` | `check_name` | `database_version` | `site` | `time_desc` | `row_cts` | `row_pts` | `row_visits`
#'
#'  ** if `time_tbls` contains fields that are grouped, the output will contain the grouped variables
#'
# check_fot_group <- function(fot_tbl,
#                             time_frame = time_span,
#                             lookback_weeks=0,
#                             lookback_months=1,
#                             check_string = 'fot',
#                             visits_only = TRUE,
#                             distinct_visits = TRUE) {
#
#   site_nm <- config('qry_site')
#
#   time_tbls <- split(fot_tbl, seq(nrow(fot_tbl)))
#
#   final_results <- list()
#
#   for(i in 1:length(time_tbls)) {
#
#     cli::cli_inform(paste0('Starting ',time_tbls[[i]]$check_description))
#
#     start_date <- as.Date(time_span[1]) - months(lookback_months)
#     end_int <- length(time_span)
#     end_date <- time_span[end_int]
#
#     date_cols <- time_tbls[[i]]$date_field
#
#     order_cols <- ncol(date_cols)
#
#     date_cols_unmapped <-
#       date_cols %>%
#       select(all_of(order_cols))
#
#     colname_string <- as.character(colnames(date_cols_unmapped)[1])
#
#     # visits_narrowed <-
#     #   time_tbls[[i]][[1]] %>%
#     #   filter(!! sym(colname_string) < as.Date(end_date) &
#     #            !! sym(colname_string) >= as.Date(start_date)) %>%
#     #   mutate(month_end = last_day_of_month(!!sym(colname_string)),
#     #          month_start = first_day_of_month(!!sym(colname_string)))
#
#     n <- names(time_tbls[i])
#     d <- time_tbls[[i]][[2]]
#     t <- time_tbls[[i]]$table
#
#     if(visits_only) {
#       visit_cts <-
#         visits_narrowed %>%
#         group_by(month_end) %>%
#         summarise(row_visits = n_distinct(visit_occurrence_id)) %>%
#         collect() %>%
#         ungroup()  %>%
#         add_meta(check_lib=check_string) %>%
#         mutate(check_name = n) %>%
#         mutate(check_desc = d,
#                domain = t)
#     } else if(distinct_visits & !visits_only) {
#       visit_cts <-
#         visits_narrowed %>%
#         group_by(month_end) %>%
#         summarise(row_cts = n(),
#                   row_visits = n_distinct(visit_occurrence_id),
#                   row_pts = n_distinct(person_id)) %>%
#         collect() %>%
#         ungroup()  %>%
#         add_meta(check_lib=check_string) %>%
#         mutate(check_name = n) %>%
#         mutate(check_desc = d,
#                domain = t)
#     } else if(!distinct_visits & !visits_only) {
#       visit_cts <-
#         visits_narrowed %>%
#         group_by(month_end) %>%
#         summarise(row_cts = n(),
#                   row_pts = n_distinct(person_id)) %>%
#         collect() %>%
#         ungroup()  %>%
#         add_meta(check_lib=check_string) %>%
#         mutate(check_name = n) %>%
#         mutate(check_desc = d,
#                domain = t)
#     }
#
#     final_results[[paste0(n)]] = visit_cts
#
#   }
#
#   final_results
#
# }



# This function calculates the heuristic used in the FOT dq check
# The heuristic is:
# month / ((month-1)*.25 +
#          (month+1)*.25 +
#          (month-12)*.5)
# In plain words, its the current month divided by the weighted average of the
# previous month, the next month, and the value from the current month in the
# previous year
fot_check_calc <- function(tblx, site_col,time_col, target_col) {
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
                      time_col='month_end') {

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
    group_by(check_type, check_name, check_desc, database_version, month_end) %>%
    summarise(row_ratio=median(row_ratio, na.rm=TRUE))%>%
    ungroup()%>%
    mutate(site='allsite_median')

  fot_input_tbl_allsite_mean<-fot_input_tbl%>%
    group_by(check_type, check_name, check_desc, database_version, month_end) %>%
    summarise(row_ratio=mean(row_ratio, na.rm=TRUE))%>%
    ungroup()%>%
    mutate(site='allsite_mean')

  bind_rows(fot_input_tbl,
            fot_input_tbl_allsite_med)%>%
    bind_rows(fot_input_tbl_allsite_mean)

}




#' Facts Over Time -- Process
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
#' @returns a list with two dataframes:
#'          one with the summary heuristic (month / ((month-1)*.25 +
#'                                                  (month+1)*.25 +
#'                                                  (month-12)*.5))
#'          and another with summary values (mean, med, sd, q1, q3) based on
#'          the heuristic
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
                        tblx = fot_int)

  if(add_ratios){

    fot_ratios <- add_fot_ratios(fot_lib_output = fot_int,
                                 denom_mult = ratio_mult)

    fot_list[[3]] <- fot_ratios

  }

  return(fot_list)

}
