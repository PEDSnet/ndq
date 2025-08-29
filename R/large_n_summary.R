
#' Add summary columns for a large N of sites
#'
#' For analyses that have a high volume of sites in the output, the standard, site-level
#' results may be difficult to visualize on a plot. This function will compute high level summary
#' statistics across all sites to provide another option for visualization. It will also
#' allow for users to compare the site level output to the overall / "network-level" statistics.
#'
#' @param dq_output *tabular output* || **required**
#'
#'  Any of the output tables that has been created by one of the `check_*` functions
#'  and has also passed through the associated `process_*` function
#'
#' @param num_col *string* || **required**
#'
#'  A string indicating the name of the numeric column in the `dq_output` table
#'  that should be the basis for summary statistic computation
#'
#' @param grp_vars *string / vector* || **required**
#'
#'  A string or vector listing the variables in `dq_output` that should be used
#'  for grouping when comparing summary statistics.
#'
#' @param check_string *string* || **required**
#'
#'  A string that matches the check string originally input into the analysis
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'  A boolean indicating whether the check is time dependent. This should only
#'  apply for Facts Over Time output or Unmapped Concepts output in cases when it
#'  was computed longitudinally
#'
#' @param shape *string* || defaults to `wide`
#'
#'  A string, either `long` or `wide`, indicating whether summary statistics should be
#'  separate columns in the table (wide) or assigned to separate rows with the
#'  name of the summary statistic in the site column (long)
#'
#' @return
#'
#'  This function will return the original `dq_output` table with the addition of
#'  summary statistics based on the numeric column provided. These will include:
#'
#'  - If `time = TRUE`, the mean and median are returned.
#'  - If `time = FALSE`, the max, min, q1, q3, mean, and median are returned
#'
#' @export
#'
summarize_large_n <- function(dq_output,
                              num_col,
                              grp_vars,
                              check_string,
                              time = FALSE,
                              shape = 'wide'){

  if(!time){

    # summary values already computed for anomaly detection thresholds
    if('max_val'%in%colnames(dq_output)){dq_output<-dq_output%>%select(-c(mean_val, median_val,max_val, min_val))}
    if(check_string%in%c('vc','vs')){
      denoms <- dq_output %>% distinct(check_name, site, check_type,
                                       table_application,measurement_column,
                                       total_denom_ct, site_anon, sitenum) %>%
        collect()

      total_viol<-dq_output%>%filter(!accepted_value)%>%
        group_by(check_name, site, check_type,
                 table_application,measurement_column, accepted_value) %>%
        summarise(tot_viol_ct = as.integer(sum(tot_ct)))%>%
        ungroup() %>% collect()

      dq_output<-denoms%>%
        left_join(total_viol)%>%
        mutate(tot_viol_ct=case_when(is.na(tot_viol_ct)~0L,
                                     TRUE~tot_viol_ct),
               prop_viol=tot_viol_ct/total_denom_ct,
               accepted_value=case_when(is.na(accepted_value)~TRUE,
                                        TRUE~FALSE))
    }
    summ_dat <- dq_output %>%
      collect() %>%
      group_by(!!!syms(grp_vars)) %>%
      summarise(max_val = max(!!sym(num_col)),
                min_val = min(!!sym(num_col)),
                q1 = quantile(!!sym(num_col), 0.25),
                q3 = quantile(!!sym(num_col), 0.75),
                mean_val = mean(!!sym(num_col)),
                median_val = median(!!sym(num_col)))
    if(shape=='long'){
      summ_dat<-summ_dat%>%
        pivot_longer(cols = c(max_val, min_val, q1, q3, mean_val, median_val),
                     names_to = 'site',
                     values_to = num_col)

      final_dat <- dq_output %>%
        collect() %>%
        bind_rows(summ_dat)
    }else if(shape=='wide'){
      final_dat <- dq_output %>%
        collect() %>%
        left_join(summ_dat)
    }

  }else{

    summ_dat <- dq_output %>%
      group_by(!!!syms(grp_vars)) %>%
      summarise(mean_val = mean(!!sym(num_col)),
                median_val = median(!!sym(num_col))) %>%
      pivot_longer(cols = c(mean_val, median_val),
                   names_to = 'site',
                   values_to = num_col) %>% collect()

    final_dat <- dq_output %>%
      collect() %>%
      bind_rows(summ_dat)
  }

  return(final_dat)

}
