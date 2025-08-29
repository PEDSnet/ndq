
#' Data Cycle Changes
#'
#' This function will compute row & patient counts in the specified tables for both
#' the current data model version and a previous data model version in order to
#' assess changes across data extractions. If you have previously executed this function,
#' you have the option to point to a previous result set instead of recomputing the
#' counts from the CDM.
#'
#' @param dc_tbl *tabular input* || **required**
#'
#'  The primary input table that contains descriptive information about the checks
#'  to be executed by the function. It should include definitions for the CDM elements
#'  that should be compared between the current and previous data model versions.
#'  see `?dc_input_omop` or `?dc_input_pcornet` for examples of the input structure
#'
#' @param omop_or_pcornet *string* || defaults to `omop`
#'
#'  A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @param prev_db_string *string* || defaults to `v1`
#'
#'  A string label indicating the previous CDM version
#'
#'  If `prev_ct_src` is set to `result`, make sure this matches the appropriate database
#'  label in the last set of results
#'
#' @param current_db_string *string* || defaults to `v2`
#'
#'  A string label indicating the current CDM version
#'
#' @param prev_ct_src *string* || defaults to `cdm`
#'
#'  A string indicating the source from which  the counts from the previous data model
#'  should be extracted:
#'  - `cdm` will compute the counts based on the previous CDM tables
#'  - `result` will pull existing counts from a previous instance of `check_dc` output
#'
#' @param prev_rslt_tbl *string* || defaults to `dc_output`
#'
#'  If `prev_ct_src` is set to `result`, this string should reflect name of
#'  the table where previous results are stored on the database
#'
#' @param prev_rslt_schema *string* || defaults to `config('results_schema')`
#'
#'  If `prev_ct_src` is set to `result`, this string should reflect name of
#'  the schema where previous results are stored on the database
#'
#' @param prev_db *database connection* || defaults to `config('db_src')`
#'
#'  A database connection object that will connect the function to the
#'  previous CDM instance or the defined result table
#'
#' @param check_string *string* || defaults to `dc`
#'
#'  An abbreviated identifier that will be used to label all output from this module
#'
#' @returns
#'  This function will return a list with two dataframes:
#'  - `dc_cts`: A table containing the row and (where applicable) person counts for each CDM
#'  element specified by the user
#'  - `dc_meta`: A table containing the metadata associated with each check that appears in `dc_cts`
#'
#' @importFrom cli cli_abort
#' @importFrom stringr str_detect
#' @importFrom rlang parse_expr
#' @import dplyr
#' @import argos
#'
#' @export
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::dc_input_omop
#' ndq::dc_input_pcornet
#'
#' # Use this as your input to the DC function
#'
#' ## If you would like to retrieve counts from a previous CDM instance,
#' ## configure the function like so:
#' \dontrun{
#' my_dc_rslt <- check_dc(dc_tbl = ndq::dc_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        prev_db_string = 'my_previous_data',
#'                        current_db_string = 'my_current_data',
#'                        prev_ct_src = 'cdm', ## looking in previous CDM instance
#'                        prev_db = my_prev_db_connection,
#'                        check_string = 'dc')
#' }
#'
#' ## If you would like to reference previously executed counts from a prior
#' ## run of check_dc, configure the function like so:
#' \dontrun{
#' my_dc_rslt <- check_dc(dc_tbl = ndq::dc_input_omop,
#'                        omop_or_pcornet = 'omop',
#'                        prev_db_string = 'my_previous_data',
#'                        current_db_string = 'my_current_data',
#'                        prev_ct_src = 'result', ## looking in previous results
#'                        prev_db = my_prev_db_connection,
#'                        prev_rslt_tbl = 'my_dc_rslt',
#'                        prev_rslt_schema = 'my_previous_schema',
#'                        check_string = 'dc')
#' }
#'
check_dc <- function(dc_tbl,
                     omop_or_pcornet = 'omop',
                     prev_db_string = 'v1',
                     current_db_string = 'v2',
                     prev_ct_src = 'cdm',
                     prev_db = config('db_src'),
                     prev_rslt_tbl = 'dc_output',
                     prev_rslt_schema = config('results_schema'),
                     check_string = 'dc'){

  site_nm <- config('qry_site')

  if(tolower(omop_or_pcornet) == 'omop'){
    pt_col <- 'person_id'
  }else if(tolower(omop_or_pcornet) == 'pcornet'){
    pt_col <- 'patid'
  }else{
    cli::cli_abort('Invalid value for omop_or_pcornet. Please choose `omop` or `pcornet` as the CDM')
  }

  cts <- list()
  meta <- list()

  dc_tbl <- split(dc_tbl, seq(nrow(dc_tbl)))

  for(i in 1:length(dc_tbl)){

    cli::cli_inform(paste0('Computing ', dc_tbl[[i]]$check_description))

    tbl_current <- pick_schema(schema = dc_tbl[[i]]$schema_current,
                               table = dc_tbl[[i]]$table_current,
                               db = config('db_src')) %>%
      add_site() %>%
      filter(site == site_nm)

    pid_check <- any(str_detect(colnames(tbl_current),pt_col))

    ## Current Tables
    if(!is.na(dc_tbl[[i]]$filter_logic)){
      this_round_current <- tbl_current %>%
        filter(!! rlang::parse_expr(dc_tbl[[i]]$filter_logic))
    }else{
      this_round_current <- tbl_current
    }

    if(pid_check){
      this_round_current <- this_round_current %>%
        summarise(total_ct=n(),
                  total_pt_ct=as.numeric(n_distinct(!!sym(pt_col)))) %>%
        collect() %>%
        mutate(database_version=current_db_string)
    }else{
      this_round_current <- this_round_current %>%
        summarise(total_ct=n(),
                  total_pt_ct=as.numeric(0)) %>%
        collect() %>%
        mutate(database_version=current_db_string)
    }

    ## Previous Tables
    if(prev_ct_src == 'cdm'){

      if(!is.na(dc_tbl[[i]]$filter_logic)){
        this_round_prev <- pick_schema(schema = dc_tbl[[i]]$schema_prev,
                                       table = dc_tbl[[i]]$table_prev,
                                       db = prev_db) %>%
          filter(!! rlang::parse_expr(dc_tbl[[i]]$filter_logic)) %>%
          add_site() %>%
          filter(site == site_nm)
      }else{
        this_round_prev <- pick_schema(schema = dc_tbl[[i]]$schema_prev,
                                       table = dc_tbl[[i]]$table_prev,
                                       db = prev_db) %>%
          add_site() %>%
          filter(site == site_nm)
      }

      if(pid_check){
        this_round_prev <- this_round_prev %>%
          summarise(total_ct=n(),
                    total_pt_ct=as.numeric(n_distinct(!!sym(pt_col)))) %>%
          collect() %>%
          mutate(database_version=prev_db_string)
      }else{
        this_round_prev <- this_round_prev %>%
          summarise(total_ct=n(),
                    total_pt_ct=as.numeric(0)) %>%
          collect() %>%
          mutate(database_version=prev_db_string)
      }

    }else{
      q <- paste0(check_string, '_', dc_tbl[[i]]$check_id)

      this_round_prev <-
        pick_schema(table = prev_rslt_tbl,
                    schema = prev_rslt_schema,
                    db = prev_db) %>%
        filter(site == site_nm,
               database_version == prev_db_string,
               check_name == q) %>%
        collect()
    }

    q <- dc_tbl[[i]]$check_id
    t <- dc_tbl[[i]]$check_domain

    if(prev_ct_src == 'cdm'){
      cts[[dc_tbl[[i]]$check_id]] <-
        this_round_current %>%
        union(this_round_prev) %>%
        mutate(site = site_nm) %>%
        mutate(domain=t) %>%
        mutate(check_name=paste0(check_string,'_',q)) %>%
        relocate(site, .before=total_ct)  %>% mutate(check_type=check_string)
    }else{
      cts[[dc_tbl[[i]]$check_id]] <-
        this_round_current %>%
        mutate(site = site_nm) %>%
        mutate(domain=t) %>%
        mutate(check_name=paste0(check_string,'_',q)) %>%
        relocate(site, .before=total_ct)  %>% mutate(check_type=check_string) %>%
        union(this_round_prev)
      }

    cts

    meta[[dc_tbl[[i]]$check_id]] <- tibble(check_name = paste0(check_string, '_', dc_tbl[[i]]$check_id),
                                           check_description = dc_tbl[[i]]$check_description,
                                           check_type = check_string,
                                           version_previous = prev_db_string,
                                           version_current = current_db_string)

  }

  collapse_cts <- purrr::reduce(.x = cts,
                                .f = dplyr::union)

  collapse_meta <- purrr::reduce(.x = meta,
                                 .f = dplyr::union)

  final_list <- list('dc_cts' = collapse_cts,
                     'dc_meta' = collapse_meta)

  return(final_list)
}


#' Data Cycle Changes -- Processing
#'
#' Intakes the output of `check_dc` in order to apply additional processing. This
#' includes computing percent change across data model versions and computing an
#' overall set of counts/percent changes across all sites included in the input.
#'
#' @param dc_ct_results *tabular input* || **required**
#'
#'  The `dc_cts` output of `check_dc`. This table should include results for all
#'  institutions that should be included in the computation of overall / "network level"
#'  statistics.
#'
#' @param dc_meta_results *tabular input* || **required**
#'
#'  The `dc_meta` output of `check_dc`
#'
#' @param rslt_source *string* || defaults to `remote`
#'
#'  A string that identifies the location of the `dc_results` table.
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
#' @return
#'  This function will return the `dc_ct_results` table with overall / "network-wide" counts
#'  and the proportion change between the two data models
#'
#'  The `dc_meta_results` table remains unchanged, so the original version returned by `check_dc`
#'  can be used if needed
#'
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
#' @examples
#' # This function should be run after check_dc has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' ## When results are kept locally:
#' \dontrun{
#' my_dc_process <- process_dc(dc_ct_results = my_dc_cts,
#'                             dc_meta_results = my_dc_meta,
#'                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_dc_process <- process_dc(dc_ct_results = 'my_dc_cts',
#'                             dc_meta_results = 'my_dc_meta',
#'                             rslt_source = 'csv',
#'                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_dc_process <- process_dc(dc_ct_results = 'my_dc_cts',
#'                             dc_meta_results = 'my_dc_meta',
#'                             rslt_source = 'remote')
#' }
#'
#'
process_dc <- function(dc_ct_results,
                       dc_meta_results,
                       rslt_source = 'remote',
                       csv_rslt_path = NULL) {

  if(tolower(rslt_source) == 'remote'){
    dc_wider <- results_tbl(dc_ct_results) %>%
      pivot_wider(names_from=database_version,
                  values_from=c(total_ct, total_pt_ct))%>%
      collect()
    dc_meta <- results_tbl(dc_meta_results)
  }else if(tolower(rslt_source) == 'csv'){
    dc_wider <- readr::read_csv(paste0(csv_rslt_path, dc_ct_results)) %>%
      pivot_wider(names_from=database_version,
                  values_from=c(total_ct, total_pt_ct))%>%
      collect()
    dc_meta <- readr::read_csv(paste0(csv_rslt_path, dc_meta_results))
  }else if(tolower(rslt_source) == 'local'){
    dc_wider <- dc_ct_results %>%
      pivot_wider(names_from=database_version,
                  values_from=c(total_ct, total_pt_ct))%>%
      collect()
    dc_meta <- dc_meta_results
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  ## get current and previous versions
  prev_v_str <- dc_meta %>% distinct(version_previous) %>% pull()
  curr_v_str <- dc_meta %>% distinct(version_current) %>% pull()

  prev_v <- paste0('total_ct_',prev_v_str)
  current_v <- paste0('total_ct_',curr_v_str)

  prev_v_pt <- paste0('total_pt_ct_',prev_v_str)
  current_v_pt <- paste0('total_pt_ct_',curr_v_str)

  ## compute proportions & summaries
  dc_totals_person <- dc_wider %>%
    group_by(domain, check_name, check_type) %>%
    summarise({{prev_v}} := sum(.data[[prev_v_pt]]),
              {{current_v}} := sum(.data[[current_v_pt]])) %>%
    mutate(site='total', application='person')%>%
    ungroup()

  dc_totals_rows <- dc_wider %>%
    group_by(domain, check_name, check_type) %>%
    summarise({{prev_v}} := sum(.data[[prev_v]]),
              {{current_v}} := sum(.data[[current_v]])) %>%
    mutate(site='total', application='rows')%>%
    ungroup()

  dc_site_pats <- dc_wider %>% select(-c({{prev_v}}, {{current_v}})) %>%
    rename({{prev_v}}:=all_of(prev_v_pt),
           {{current_v}}:=all_of(current_v_pt))%>%
    mutate(application='person')

  dc_site_rows <- dc_wider %>% select(-c({{prev_v_pt}}, {{current_v_pt}})) %>%
    mutate(application='rows')

  ## combine row and patient summaries
  all_dat<-bind_rows(dc_totals_person, dc_totals_rows) %>%
    bind_rows(., dc_site_pats) %>%
    bind_rows(., dc_site_rows) %>%
    mutate(prop_total_change=
             case_when(!!sym(prev_v) == 0 ~ NA_real_,
                       TRUE ~ round((.data[[current_v]]-.data[[prev_v]])/.data[[prev_v]],2))) %>%
    mutate(check_name_app=paste0(check_name, "_", application))

  # adding in a scaled proportion
  max_val<-all_dat%>%summarise(m=max(abs(prop_total_change), na.rm=TRUE))%>%pull()

  all_dat %>%
    mutate(prop_total_change=case_when(is.na(prop_total_change)~0,
                                       TRUE~prop_total_change))

}
