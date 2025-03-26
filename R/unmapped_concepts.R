
#' Function to check for value set conformance to a defined set
#' @param uc_tbl dataframe with metadata describing the tables/columns for which
#'               unmapped concepts should be identified
#' @param by_year boolean indicating whether the analysis should be conducted longitudinally by year
#'                or not; note that a mapped list will NOT be produced for the longitudinal analysis
#' @param unmapped_values concepts / other values that indicate an unmapped value
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `uc`
#' @param produce_mapped_list if `TRUE` then will produce a table with the source values
#'                            associated with the top unmapped values; this table will
#'                            be iteratively output to the database backend of choice and will
#'                            NOT be stored locally. Only source values with > 10 occurrences
#'                            are included.
#'
#' @return
#'   tbl with the following columns:
#'   - site
#'   - measure: from element name describing measuer
#'   - total_rows: the denominator
#'   - unmapped_rows: numerator
#'   - check_name: from `check_string`
#'   - database_version
#'   - unmapped_prop
#'
#' if `produce_mapped_list` is `TRUE`, then the following table will be automatically output
#' to the database with the table name `st_conf_concepts_grpd`:
#'   - site
#'   - unmapped_description
#'   - database_version
#'   - check_name
#'   - src_value
#'   - src_value_name
#'   - src_value_ct
#'
#'
#' @export
#'
#'
check_uc <- function(uc_tbl,
                     by_year = FALSE,
                     produce_mapped_list=TRUE,
                     unmapped_values = c(44814650L,0L,
                                         44814653L, 44814649L),
                     check_string = 'uc') {

  if(by_year){
    check_concepts <- check_uc_by_year(uc_tbl = uc_tbl,
                                       unmapped_values = unmapped_values,
                                       check_string = check_string)
  }else{

    site_nm <- config('qry_site')

    concept_list <- split(uc_tbl, seq(nrow(uc_tbl)))

    check_concepts <- list()

    for(i in 1:length(concept_list)) {

      cli::cli_inform(paste0('Starting ', concept_list[[i]]$check_description))

      if(!is.na(concept_list[[i]]$filter_logic)){
        tbl_use <- pick_schema(schema = concept_list[[i]]$schema,
                               table = concept_list[[i]]$table,
                               db = config('db_src')) %>%
          filter(!! rlang::parse_expr(concept_list[[i]]$filter_logic))
      }else{
        tbl_use <- pick_schema(schema = concept_list[[i]]$schema,
                               table = concept_list[[i]]$table,
                               db = config('db_src'))
      }

      total_rows <-
        tbl_use %>%
        add_site() %>% filter(site == site_nm) %>%
        summarise(
          total_rows = n()
        ) %>% collect()


      colname <- concept_list[[i]]$concept_field

      unmapped_vals <-
        tbl_use %>%
        add_site() %>% filter(site == site_nm) %>%
        filter(.data[[colname]]  %in% unmapped_values)

      if(produce_mapped_list) {

        meta_desc = concept_list[[i]]$check_description

        unmapped_db <-
          unmapped_vals %>%
          group_by(!! sym(concept_list[[i]]$source_value_field)) %>%
          summarise(src_value_ct = n()) %>%
          ungroup() %>% filter(src_value_ct > 10) %>% collect() %>%
          pivot_longer(cols=!!sym(concept_list[[i]]$source_value_field),
                       names_to = 'src_value_name',
                       values_to = 'src_value') %>%
          add_meta(check_lib = check_string) %>%
          mutate(
            unmapped_description = meta_desc
          ) %>% collect()

        output_tbl_append(data=unmapped_db,
                          name=paste0(check_string, '_grpd'))

      }

      total_unmapped <-
        unmapped_vals %>%
        summarise(
          unmapped_rows = n()
        ) %>% collect()

      unmapped_cts <-
        bind_cols(total_rows, total_unmapped) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', concept_list[[i]]$check_id)) %>%
        relocate(site, .before = total_rows) %>%
        mutate(measure = concept_list[[i]]$check_description) %>%
        relocate(measure, .after = site) %>%
        mutate(
          unmapped_prop = round(unmapped_rows / total_rows, 2)
        )

      check_concepts[[concept_list[[i]]$check_id]] <- unmapped_cts

    }
  }

  check_concepts_red <- purrr::reduce(.x = check_concepts,
                                      .f = dplyr::union)

  return(check_concepts_red)

}

#' Unmapped Concepts by Year
#'
#' Function that produces output that contains unmapped values by year
#'
#' @param uc_tbl dataframe with metadata describing the tables/columns for which
#'               unmapped concepts should be identified
#' @param unmapped_values concepts / other values that indicate an unmapped value
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `uc`
#'
#' @return a dataframe with the following columns:
#' - site
#' - year_date
#' - total_unmapped_row_ct
#' - total_row_ct
#' - unmapped_description
#' - check_name
#' - database_version
#'
#'
check_uc_by_year <- function(uc_tbl,
                             unmapped_values = c(44814650L,0L,
                                                 44814653L, 44814649L),
                             check_string = 'uc') {

  site_nm <- config('qry_site')

  concept_list <- split(uc_tbl, seq(nrow(uc_tbl)))

  check_concepts <- list()

  for(i in 1:length(concept_list)) {

    cli::cli_inform(paste0('Starting ', concept_list[[i]]$check_description))

    colname <- concept_list[[i]]$concept_field

    if(!is.na(concept_list[[i]]$filter_logic)){
      tbl_use <- pick_schema(schema = concept_list[[i]]$schema,
                             table = concept_list[[i]]$table,
                             db = config('db_src')) %>%
        filter(!! rlang::parse_expr(concept_list[[i]]$filter_logic))
    }else{
      tbl_use <- pick_schema(schema = concept_list[[i]]$schema,
                             table = concept_list[[i]]$table,
                             db = config('db_src'))
    }

    unmapped_vals <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      filter(.data[[colname]]  %in% unmapped_values)

    date_cols <-
      unmapped_vals %>%
      select(!!sym(concept_list[[i]]$date_field))
      # select(ends_with('_date')) %>% select(- contains('end'))

    order_cols <- ncol(date_cols)

    date_cols_unmapped <-
      date_cols %>%
      select(order_cols)

    date_col_final <-
      colnames(date_cols_unmapped)

    if(class(config('db_src')) %in% c('PostgreSQLConnection', 'PqConnection', 'BigQueryConnection')){
      sql_string <- paste0("extract(year from ", date_col_final, ")")
    }else if(class(config('db_src')) %in% c('SQLiteConnection')){
      sql_string <- paste0("date(", date_col_final, ')')
    }else{
      sql_string <- paste0('YEAR(', date_col_final, ')')
    }

    total_rows <-
      tbl_use %>%
      add_site() %>% filter(site == site_nm) %>%
      mutate(year_date = as.integer(sql(sql_string))) %>%
      group_by(
        year_date
      ) %>% summarise(
        total_row_ct = n()
      )%>% collect()

    date_col_grpd <-
      date_cols_unmapped %>%
      mutate(year_date = as.integer(sql(sql_string))) %>%
      group_by(
        year_date
      ) %>% summarise(
        total_unmapped_row_ct = n()
      ) %>% collect() %>%
      inner_join(total_rows) %>%
      mutate(unmapped_description=concept_list[[i]]$check_description) %>%
      add_meta(check_lib = check_string) %>%
      relocate(
        site, .before = year_date
      ) %>% mutate(check_name = paste0(check_string, '_', concept_list[[i]]$check_id))

    check_concepts[[concept_list[[i]]$check_id]] <- date_col_grpd

  }

  check_concepts

}


#' Unmapped Concepts -- Processing
#'
#' Function to add post-processed columns to the unmapped concepts dqa_library output
#'             and to add a `total` row with for each of the check applications
#'             for the overall number and proportion of unmapped rows
#'
#' @param uc_results the output of check_uc
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return table with additional columns/etc needed for pp output
#'
#' @export
#'
process_uc <- function(uc_results,
                       rslt_source = 'remote',
                       csv_rslt_path = NULL){

  if(tolower(rslt_source) == 'remote'){
    uc_int <- results_tbl(uc_results) %>%
      collect()
  }else if(tolower(rslt_source) == 'csv'){
    uc_int <- readr::read_csv(paste0(csv_rslt_path, uc_results))
  }else if(tolower(rslt_source) == 'local'){
    uc_int <- uc_results %>% collect()
  }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

  year_yn <- any(grepl('year_date', colnames(uc_int)))

  if(year_yn){

    uc_final <- uc_int %>%
      mutate(prop_total=total_unmapped_row_ct/total_row_ct)

  }else{
    total_uc <- uc_int %>%
      group_by(measure, check_type, database_version, check_name) %>%
      summarise(total_rows=sum(total_rows, na.rm = TRUE),
                unmapped_rows=sum(unmapped_rows, na.rm = TRUE))%>%
      ungroup() %>%
      mutate(site='total',
             unmapped_prop=unmapped_rows/total_rows,
             unmapped_prop=ifelse(is.na(unmapped_prop), 0, unmapped_prop))

    uc_final <- total_uc %>%
      dplyr::union_all(uc_int)%>%
      mutate(check_name_app=paste0(check_name,"_rows"))
  }

  return(uc_final)

}
