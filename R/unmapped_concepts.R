
#' Unmapped Concepts
#'
#' This function will evaluate the count and proportion of unmapped concepts associated
#' with the fact type of interest. If produce_mapped_list is set to TRUE, a summary of
#' the source values associated with unmapped concepts will also be produced to help
#' identify areas where mappings could potentially be improved.
#'
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
#' @return if `by_year` is `FALSE`: a dataframe with the total row count, the unmapped row count, the proportion
#'         of unmapped values, and some additional descriptive metadata for each check
#'
#'         if `by_year` is `TRUE`: a dataframe with the total row count, the unmapped row count, the proportion
#'         of unmapped values, and some additional descriptive metadata for each check stratified by each year
#'         present in the fact table
#'
#'         if `produce_mapped_list` is `TRUE`, then a table with name `uc_grpd` that includes
#'         the source values (with > 10 appearances) and counts of those values associated
#'         with unmapped concepts
#'
#' @export
#'
#' @examples
#' # First create input file with desired checks to be executed
#' # You can access examples for both OMOP & PCORnet here:
#' ndq::uc_input_omop
#' ndq::uc_input_pcornet
#'
#' # Use this as your input to the UC function
#' ## Overall
#' \dontrun{
#' my_uc_rslt <- check_uc(uc_tbl = ndq::uc_input_omop,
#'                        by_year = FALSE,
#'                        produce_mapped_list = TRUE, # list of unmapped source values
#'                        unmapped_values = c(44814650L,0L,
#'                                            44814653L, 44814649L) # will also check for NA by default
#'                        omop_or_pcornet = 'omop',
#'                        check_string = 'uc')
#' }
#'
#' ## By Year
#' \dontrun{
#' my_uc_rslt <- check_uc(uc_tbl = ndq::uc_input_omop,
#'                        by_year = TRUE,
#'                        produce_mapped_list = FALSE, # not functionality for by year analysis
#'                        unmapped_values = c(44814650L,0L,
#'                                            44814653L, 44814649L) # will also check for NA by default
#'                        omop_or_pcornet = 'omop',
#'                        check_string = 'uc')
#' }
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
          total_rows = as.numeric(n())
        ) %>% collect()


      colname <- concept_list[[i]]$concept_field

      unmapped_vals <-
        tbl_use %>%
        add_site() %>% filter(site == site_nm) %>%
        filter(.data[[colname]]  %in% unmapped_values | is.na(.data[[colname]]))

      if(produce_mapped_list) {

        meta_desc = concept_list[[i]]$check_description

        unmapped_db <-
          unmapped_vals %>%
          group_by(!! sym(concept_list[[i]]$source_value_field)) %>%
          summarise(src_value_ct = as.numeric(n())) %>%
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
          unmapped_rows = as.numeric(n())
        ) %>% collect()

      unmapped_cts <-
        bind_cols(total_rows, total_unmapped) %>%
        add_meta(check_lib = check_string) %>%
        mutate(check_name = paste0(check_string, '_', concept_list[[i]]$check_id)) %>%
        relocate(site, .before = total_rows) %>%
        mutate(measure = concept_list[[i]]$check_description) %>%
        relocate(measure, .after = site) %>%
        mutate(
          unmapped_prop = round(as.numeric(unmapped_rows) / as.numeric(total_rows), 2),
          unmapped_prop = ifelse(is.na(unmapped_prop), 0, unmapped_prop)
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
#' This function will evaluate the count and proportion of unmapped concepts associated
#' with the fact type of interest, stratified by year.
#'
#' @param uc_tbl dataframe with metadata describing the tables/columns for which
#'               unmapped concepts should be identified
#' @param unmapped_values concepts / other values that indicate an unmapped value
#' @param check_string an abbreviated identifier to identify all output from this module
#'                     defaults to `uc`
#'
#' @return a dataframe with the total row count, the unmapped row count, the proportion
#'         of unmapped values, and some additional descriptive metadata for each check stratified by each year
#'         present in the fact table
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
      filter(.data[[colname]]  %in% unmapped_values | is.na(.data[[colname]]))

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
        total_row_ct = as.numeric(n())
      )%>% collect()

    date_col_grpd <-
      date_cols_unmapped %>%
      mutate(year_date = as.integer(sql(sql_string))) %>%
      group_by(
        year_date
      ) %>% summarise(
        total_unmapped_row_ct = as.numeric(n())
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
#' Intakes the output of check_uc in order to apply additional processing. This
#' includes either adding proportions (for by_year output) or computing overall
#' totals across all sites included in the input (for not by_year output)
#'
#' @param uc_results the output of check_uc
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @return a table with either an additional column with unmapped proportions (by_year) or
#'         with additional rows that include total unmapped counts/proportions across all
#'         sites (not by_year)
#'
#' @export
#'
#' @examples
#' # This function should be run after check_uc has been executed for all
#' # network institutions and results have been combined into a common table
#'
#' # Once the labels have been applied, the function can be executed
#' ## When results are kept locally:
#' \dontrun{
#' my_uc_process <- process_uc(uc_results = my_uc_rslts,
#'                             rslt_source = 'local')
#' }
#'
#' ## When results are kept in CSV files:
#' \dontrun{
#' my_uc_process <- process_uc(uc_results = 'my_uc_rslts',
#'                             rslt_source = 'csv',
#'                             csv_rslt_path = 'path/to/my/results')
#' }
#'
#' ## When results are kept on a remote database:
#' \dontrun{
#' my_uc_process <- process_uc(uc_results = 'my_uc_rslts',
#'                             rslt_source = 'remote')
#' }
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
