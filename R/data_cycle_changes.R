
#' Data Cycle Changes
#'
#' @param dc_tbl table with details of the CDM elements that should be tested in
#'               both the current and previous data model versions
#' @param omop_or_pcornet string indicating the CDM format of the data; defaults to `omop`
#' @param prev_db_string string label indicating the previous CDM version; if
#'                       prev_ct_src == 'result', ensure this matches the appropriate database
#'                       label in the last set of results
#' @param current_db_string string label indicating the current CDM version
#' @param prev_ct_src a string indicating where the counts from the previous data model
#'                    should be extracted: either `cdm` (to pull from the previous CDM instance)
#'                    or `result` (to pull from a previous instance of check_dc output)
#' @param prev_rslt_tbl if prev_ct_src = 'result', the name of the table where previous results
#'                      are stored
#' @param prev_rslt_schema if prev_ct_src = 'result', the name of the schema where previous results
#'                         are stored. defaults to `config('results_schema')`
#' @param prev_db the database connection to be used to access the previous CDM or results;
#'                defaults to `config('db_src')`
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `dc`
#'
#' @returns a list with two dataframes:
#'          - `dc_cts`: dataframe containing the row and (where applicable) person counts for each table
#'          - `dc_meta`: the metadata associated with each input table that appears in `dc_cts`
#'
#' @importFrom cli cli_abort
#' @importFrom stringr str_detect
#' @importFrom rlang parse_expr
#' @import dplyr
#' @import argos
#'
#' @export
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
                  total_pt_ct=n_distinct(!!sym(pt_col))) %>%
        collect() %>%
        mutate(database_version=current_db_string)
    }else{
      this_round_current <- this_round_current %>%
        summarise(total_ct=n(),
                  total_pt_ct=0) %>%
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
                    total_pt_ct=n_distinct(!!sym(pt_col))) %>%
          collect() %>%
          mutate(database_version=prev_db_string)
      }else{
        this_round_prev <- this_round_prev %>%
          summarise(total_ct=n(),
                    total_pt_ct=0) %>%
          collect() %>%
          mutate(database_version=prev_db_string)
      }

    }else{
      t <- dc_tbl[[i]]$check_domain

      this_round_prev <-
        pick_schema(table = prev_rslt_tbl,
                    schema = prev_rslt_schema,
                    db = prev_db) %>%
        filter(site == site_nm,
               database_version == prev_db_string,
               domain == t) %>%
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

    meta[[dc_tbl[[i]]$check_id]] <- tibble(check_name = cts[[i]]$check_name,
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
#' function to add proportions,thresholds, and totals to the output
#' of check_dc
#'
#'
#' @param dc_ct_results the table output by check_dc labelled `dc_cts` that contains
#'                      the previous and current data cycle counts
#' @param dc_meta_results the table output by check_dc labelled `dc_meta` that contains
#'                        metadata associated with each executed check
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return dc_ct_results tbl with totals and additional change in proportion
#'         and threshold columns; the dc_meta_results table remains unchanged, so the original
#'         version should be used where needed
#'
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_wider
#'
#' @export
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
