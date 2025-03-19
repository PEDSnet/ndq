
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

  return(tbl)

}


#' Additional processing for VS & VC checks
#'
#' @param tbl_list a list that contains all the vc or vs violations
#' @param check_string a string that contains the table name for the check output
#'
#' @return a pivoted version of the input table with dummy rows added for checks
#'         that did not return any violations
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
add_meta <- function(tbl_meta,
                     check_lib,
                     version=config('current_version'),
                     site_nm=config('site')) {

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
        paste0("date_diff(day, ", date_col_1, ", ", date_col_2, ")")
    }
    return(sql_code)
  }


#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#'
#' @param data the data to output
#' @param name the name of the table to output
#'
#' Parameters are the same as `output_tbl`
#'
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
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
