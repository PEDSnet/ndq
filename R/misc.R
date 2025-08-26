
pick_schema <- function(schema,
                        table,
                        db){

  if(grepl('cdm', schema)){
    scm <- config('cdm_schema')
  }else if(grepl('result', schema)){
    scm <- config('results_schema')
  }else{
    scm <- schema
  }

  tbl <- get_argos_default()$qual_tbl(name = table,
                                      schema_tag = scm,
                                      db = db)

  tbl_case_corrector <- function(tbl) {
    if (any(colnames(tbl) != tolower(colnames(tbl)))) {
      return(tbl %>% rename_all( ~ tolower(.)))
    } else {
      return(tbl)
    }
  }

  tbl_corrected <- tbl_case_corrector(tbl)

  return(tbl_corrected)

}


#' Additional processing for VS & VC checks
#'
#' @param tbl_list a list that contains all the vc or vs violations
#' @param check_string a string that contains the table name for the check output
#'
#' @return a pivoted version of the input table with dummy rows added for checks
#'         that did not return any violations
#'
#' @keywords internal
#'
create_vc_vs_output <- function(tbl_list,
                                check_string='vc') {

  meta_tbl <- list()

  for(i in 1:length(tbl_list)) {

    pivot_col <- sym(colnames(tbl_list[[i]][,1]))
    current_tbl <- tbl_list[[i]]

    final <-pivot_longer(current_tbl,
                         cols=all_of(pivot_col),
                         names_to='measurement_column',
                         values_to='concepts') %>%
      relocate(c(measurement_column,concepts), .before = total_viol_ct) %>%
      relocate(table_application, .before = measurement_column)

    meta_tbl[[i]] <- final
  }

  meta_tbl
}


#' add check name, db version, and site name to a given table
#'
#' @param tbl_meta the table to add meta information to
#' @param check_lib the name of the check
#' @param version the version of the database; defaults to
#' `config('current_version')`;
#' @param site_nm the name of the site; defaults to
#' `config('site')`
#'
#' @keywords internal
#'
add_meta <- function(tbl_meta,
                     check_lib,
                     version=config('current_version'),
                     site_nm=config('qry_site')) {

  tbl_meta %>%
    mutate(check_type = check_lib,
           database_version=version,
           site=site_nm)


}


#' Calculate Date Differences in Multiple SQL Backends
#'
#' Function to get sql code for number of days between date1 and date2.
#' Adapted for sql dialects for Postgres and MS SQL.
#'
#' Should always be wrapped by sql()
#' @param date_col_1 Date col 1
#' @param date_col_2 Date col 2
#' @param db connection type object. Defaulted to config('db_src') for standard framework
#' Functionality added for Postgres, MS SQL and Snowflake
#'
#' @return an integer representing the difference (in days) between the two provided
#' dates
#'
#' @keywords internal
#'
calc_days_between_dates <-
  function(date_col_1, date_col_2, db = config("db_src")) {
    if (class(db) %in% "Microsoft SQL Server") {
      sql_code <-
        paste0("DATEDIFF(day, ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db) %in% 'BigQueryConnection'){
      sql_code <-
        paste0('DATE_DIFF(', date_col_2, ', ', date_col_1, ', ', 'DAY)')
    } else if (class(db) %in% "PqConnection") {
      sql_code <-
        paste0(date_col_2, " - ", date_col_1)
    } else if (class(db) %in% "Snowflake") {
      sql_code <-
        paste0(
          "DATEDIFF(day, ",
          '"',
          date_col_1,
          '"',
          ",",
          '"',
          date_col_2,
          '"',
          ")"
        )
    }else if(class(db) %in% 'SQLiteConnection'){
      sql_code <-
        paste0("julianday(", date_col_2, ") - julianday(", date_col_1, ")")
    }else if(class(db) %in% 'PrestoConnection'){
      sql_code <-
        paste0("date_diff('day', ", date_col_1, ", ", date_col_2, ")")
    }
    return(sql_code)
  }


#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#'
#' @param data the data to output
#' @param name the name of the table to output
#' @param local description
#' @param file blah
#' @param db blah
#' @param results_tag blah
#' @param ... other arguments
#'
#' Parameters are the same as `output_tbl`
#'
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#'
#' @keywords internal
#'
output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = base::ifelse(config('results_target') == 'file',
                                                  TRUE, FALSE),
                              db = if (! file) config('results_target') else NA,
                              results_tag = TRUE, ...) {

  if (is.na(name)) name <- quo_name(enquo(data))

  if(db_exists_table(config('db_src'), intermed_name(name,temporary=FALSE))) {

    tmp <- results_tbl(name) %>% collect_new()
    new_tbl <-
      dplyr::union(tmp,
                   data)
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = results_tag, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = results_tag, ...)
  }


}


#' Create informational metadata file
#'
#' @param check_tbls a list of table names from which the list of executed
#'                   checks should be extracted
#' @param metadata_file if one exists, a previously generated metadata file
#'                      to append to. the function will also highlight new
#'                      checks that will need to be described
#' @param rslt_source the location of the results. acceptable values are `local` (stored as a dataframe in the R environment),
#'                    `csv` (stored as CSV files), or `remote` (stored on a remote DBMS); defaults to remote
#' @param csv_rslt_path if the results have been stored as CSV files, the path to the location
#'                      of these files. If the results are local or remote, leave NULL
#'
#' @returns a dataframe with all executed checks from the input tables
#'
#'          if a metadate file is provided, it will include all information from this
#'          file in addition to newly added checks that need additional metadata added
#'
#'          if a metadata file is NOT provided, it will include all checks with some
#'          metadata pulled from the tables themselves and other fields left blank for the
#'          user to fill in
#'
#' @export
#'
create_check_metadata <- function(check_tbls,
                                  metadata_file = NULL,
                                  rslt_source = 'remote',
                                  csv_rslt_path = NULL){

  check_list <- list()

  for(i in 1:length(check_tbls)){

    if(tolower(rslt_source) == 'remote'){
      tbl_load <- results_tbl(check_tbls[[i]]) %>%
        collect()
    }else if(tolower(rslt_source) == 'csv'){
      tbl_load <- readr::read_csv(paste0(csv_rslt_path, check_tbls[[i]]))
      collect()
    }else if(tolower(rslt_source) == 'local'){
      tbl_load <- check_tbls[[i]] %>%
        collect()
    }else{cli::cli_abort('Incorrect input for rslt_source. Please set the rslt_source to either local, csv, or remote')}

    if(!is.null(metadata_file)){

      cts <- tbl_load %>% distinct(check_type) %>% pull()

      tbl_cks <- tbl_load %>%
        distinct(check_type, check_name)

      meta_final <- metadata_file %>%
        filter(check_type == cts) %>%
        full_join(tbl_cks)

      check_list[[i]] <- meta_final

    }else{

      cts <- tbl_load %>% distinct(check_type) %>% pull()

      if(cts == 'dc'){sc <- c('check_type', 'domain')}else if(cts %in% c('dcon', 'bmc')){sc <- c('check_type', 'check_desc')
      }else if(cts == 'ecp'){sc <- c('check_type', 'check_type')}else if(cts == 'fot'){sc <- c('domain', 'check_desc')
      }else if(cts == 'mf'){sc <- c('domain', 'measure')}else if(cts == 'pf'){sc <- c('visit_type', 'check_desc')
      }else if(cts == 'uc'){sc <- c('check_type', 'measure')}else if(cts %in% c('vc', 'vs')){sc <- c('table_application', 'measurement_column')}

      meta_final <- tbl_load %>%
        mutate(check_domain = !!sym(sc[1]),
               check_domain = ifelse(check_domain == check_type, NA, check_domain),
               check_application = !!sym(sc[2]),
               check_application = ifelse(check_application == check_type, NA, check_application),
               full_description = NA) %>%
        distinct(check_type, check_name, check_domain, check_application, full_description)

      check_list[[i]] <- meta_final

    }

  }

  meta_reduce <- purrr::reduce(.x = check_list,
                               .f = dplyr::union)

  if(!is.null(metadata_file)){
    meta_message <- meta_reduce %>%
      filter(is.na(full_description)) %>%
      distinct(check_name)

    meta_string <- meta_message %>% pull(check_name)
    meta_string <- paste(meta_string, collapse = ', ')

    meta_n <- meta_message %>% summarise(n = n()) %>% pull(n)

    cli::cli_inform(cli::col_cyan(paste0('There are ', meta_n, ' new checks to be described: ', meta_string)))
  }else{
    meta_n <- meta_reduce %>% summarise(n = n()) %>% pull(n)

    cli::cli_inform(cli::col_cyan(paste0("A new metadata reference file has been created with ", meta_n, " checks ready for describing.")))
  }

  return(meta_reduce)

}
