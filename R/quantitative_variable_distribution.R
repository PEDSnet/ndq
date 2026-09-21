
#' Quantitative Variable Distribution
#'
#' This function will, for a given cohort, compute summary statistics that
#' describe the numerical distribution of a given quantitative variable. If
#' compute_trimmed is set to TRUE, the function will additionally compute
#' a summary for a distribution that has the top and bottom 2.5% removed,
#' to help visualize the results without the most extreme outliers.
#'
#' @param qvd_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the variables
#'  to be examined by the function. It should define each variable and point the
#'  system to the appropriate column with quantitative information.
#'
#'  see `?qvd_input_omop` or `?qvd_input_pcornet` for examples of the input structure
#'
#' @param qvd_cohort *tabular input* || **required**
#'
#'  A table defining the specific subset of patients for which variable information
#'  should be summarised. A more specific cohort of patients is recommended to
#'  make it easier to assess whether certain values are plausible
#'  (i.e. height or lab results)
#'
#' @param compute_trimmed *boolean* || defaults to `TRUE`
#'
#'  A boolean indicating whether the variable of interest should also be evaluated
#'  using a trimmed distribution, which removes the top and bottom 2.5% to lessen
#'  the impact of the most extreme outliers on the summary statistic. These results
#'  will be treated as an additional variable with its own row of results, rather than
#'  adding new columns to the output.
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param check_string *string* || defaults to `dp`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @returns
#'
#'  This function will return a dataframe with summary statistics that describe
#'  the distribution for each user-defined variable. This will include mean,
#'  median, standard deviation, Q1, Q3, min, and max. Two measures of skew
#'  will also be returned: skewness and Pearson's skew.
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::qvd_input_omop
#' ndq::qvd_input_pcornet
#'
#' # Use this as your input to the QVD function
#' \dontrun{
#' my_qvd_rslt <- check_qvd(qvd_tbl = ndq::dp_input_omop,
#'                          qvd_cohort = my_cohort,
#'                          compute_trimmed = TRUE,
#'                          omop_or_pcornet = 'omop',
#'                          check_string = 'qvd')
#' }
#'
check_qvd <- function(qvd_tbl,
                      qvd_cohort,
                      compute_trimmed = TRUE,
                      omop_or_pcornet = 'omop',
                      check_string = 'qvd'){

  site_nm <- config('qry_site')

  if(compute_trimmed){
    qvd_w_trim <- qvd_tbl %>%
      union(qvd_tbl %>% mutate(check_id = paste0(check_id, '_trim'),
                               check_description = paste0(check_description, ' - Trimmed')))
    qvd_list <- split(qvd_w_trim, seq(nrow(qvd_w_trim)))
  }else{
    qvd_list <- split(qvd_tbl, seq(nrow(qvd_tbl)))
  }

  stat_rslt <- list()

  for(i in 1:length(qvd_list)){

    message(paste0('Starting ', qvd_list[[i]]$check_description))

    if(omop_or_pcornet == 'omop'){
      join_cols <- purrr::set_names('concept_id', qvd_list[[i]]$concept_field)
      person_col <- 'person_id'
    }else{
      join_cols <- purrr::set_names('concept_code', qvd_list[[i]]$concept_field)

      if(!is.na(qvd_list[[i]]$vocabulary_field)){
        join_cols2 <- set_names('vocabulary_id', qvd_list[[i]]$vocabulary_field)
        join_cols <- join_cols %>% append(join_cols2)
      }

      person_col <- 'patid'
    }

    ## Build table
    if(!is.na(qvd_list[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = qvd_list[[i]]$schema,
                             table = qvd_list[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(qvd_list[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = qvd_list[[i]]$schema,
                             table = qvd_list[[i]]$table,
                             db = config('db_src'))
    }

    if(!is.na(qvd_list[[i]]$codeset_name)){
      tbl_use <- tbl_use %>%
        inner_join(load_codeset(qvd_list[[i]]$codeset_name), by = join_cols)
    }else{tbl_use <- tbl_use}

    ## Frequencies by patient count or value count
    get_values <- tbl_use %>%
      inner_join(qvd_cohort) %>%
      filter(!is.na(!!sym(qvd_list[[i]]$value_field)),
             site == site_nm) %>%
      group_by(!!sym(qvd_list[[i]]$value_field), .add = TRUE) %>%
      summarise(value_freq = n()) %>%
      collect() %>%
      rename('value_col' := !!sym(qvd_list[[i]]$value_field)) %>%
      mutate(check_description = qvd_list[[i]]$check_description,
             value_col = as.numeric(value_col))

    ## trim dist if necessary
    if(grepl('_trim$', qvd_list[[i]]$check_id)){
      get_values <- get_values %>%
        tidyr::uncount(value_freq) %>%
        mutate(top_lv = quantile(value_col, 0.975),
               low_lv = quantile(value_col, 0.025)) %>%
        filter(value_col <= top_lv & value_col >= low_lv) %>%
        select(-c(top_lv, low_lv))
    }else{
      get_values <- get_values %>%
        tidyr::uncount(value_freq)
    }

    ## Summarise numerical distribution
    stat_sum <- get_values %>%
      group_by(check_description, .add = TRUE) %>%
      summarise(n_obs = n(),
                mean_val = mean(as.numeric(value_col), na.rm = TRUE),
                median_val = median(as.numeric(value_col), na.rm = TRUE),
                sd_val = sd(as.numeric(value_col), na.rm = TRUE),
                q1_val = quantile(as.numeric(value_col), 0.25, na.rm = TRUE),
                q3_val = quantile(as.numeric(value_col), 0.75, na.rm = TRUE),
                min_val = min(as.numeric(value_col), na.rm = TRUE),
                max_val = max(as.numeric(value_col), na.rm = TRUE),
                skewness = compute_skewness(as.numeric(value_col))) %>%
      mutate(across(where(is.numeric), ~replace_na(., NA))) %>%
      mutate(pearsons_skew = (3 * (mean_val - median_val)) / sd_val) %>%
      add_meta(check_lib = check_string)

    low_whisk <- get_values %>%
      group_by(check_description, .add = TRUE) %>%
      left_join(stat_sum %>% distinct(check_description, q1_val, q3_val)) %>%
      filter(value_col <= (q1_val - (1.5 * (q3_val - q1_val)))) %>%
      summarise(low_whisk = as.numeric(max(value_col)))

    upper_whisk <- get_values %>%
      group_by(check_description, .add = TRUE) %>%
      left_join(stat_sum %>% distinct(check_description, q1_val, q3_val)) %>%
      filter(value_col >= (q3_val + (1.5 * (q3_val - q1_val)))) %>%
      summarise(upper_whisk = as.numeric(min(value_col)))

    stat_rslt[[i]] <- stat_sum %>%
      left_join(low_whisk) %>%
      left_join(upper_whisk) %>%
      mutate(check_description = qvd_list[[i]]$check_description,
             check_id = paste0(check_string, '_', qvd_list[[i]]$check_id))

    rm(tbl_use)
  }

  stat_red <- purrr::reduce(.x = stat_rslt,
                            .f = dplyr::union)

  return(stat_red)

}


#' Quantitative Variable Distribution -- Processing
#'
#' Intakes the output of `check_qvd` in order to apply additional processing. This
#' includes creating a new `check_name_app` column to specify that the check
#' was computed at the person level.
#'
#' @param qvd_results *tabular input* || **required**
#'
#'  The tabular output of `check_qvd`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `qvd_results` table.
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
#'  This function will return the `qvd_results` table with and additional
#'  `check_name_app` column to indicate application level
#'
#' @export
#'
#' @examples
#' # This function should be run after check_qvd has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_qvd_process <- process_qvd(qvd_results = my_qvd_rslts,
#'                               rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_qvd_process <- process_qvd(qvd_results = 'my_qvd_rslts',
#'                               rslt_source = 'csv',
#'                               csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_qvd_process <- process_qvd(qvd_results = 'my_qvd_rslts',
#'                               rslt_source = 'remote')
#' }
#'
process_qvd <- function(qvd_results,
                        rslt_source = 'remote',
                        csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    qvd_int <- results_tbl(qvd_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    qvd_int <- readr::read_csv(paste0(csv_rslt_path, qvd_results))
  }else if(tolower(rslt_source) == 'local'){
    qvd_int <- qvd_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  qvd_final <- qvd_int %>%
    mutate(check_name_app = paste0(check_name, '_person'))

  return(qvd_final)
}
