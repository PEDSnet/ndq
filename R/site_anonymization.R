
#' Add an anonymized site identifier to all tables
#'
#' This function will iterate through the provided list of result tables
#' to apply consistent anonymization of site names across all output.
#'
#' @param all_sites_tbl *tabular input* || **required**
#'
#'  A reference table that contains a column called `site` with all of
#'  the sites that should be masked in each of the provided result tables
#'
#' @param tbls_to_anon *named list* || **required**
#'
#'  A **NAMED** list of all of the tables that should be anonymized
#'
#' @return
#'
#'  This function will return each of the original tables in `tbls_to_anon` with
#'  all the original columns plus a column called `site_anon` with a masked identifier
#'  that is consistent across all of the tables in `tbls_to_anon`
#'
#' @export
#'
attach_anon_id <- function(all_sites_tbl,
                           tbls_to_anon){

  # generate map for site anonymization
  distinct_sites <- all_sites_tbl %>%
    distinct(site) %>% collect()
  site_nums <- distinct_sites[sample(1:nrow(distinct_sites)),]%>%
    mutate(sitenum=row_number(),
           site_anon=paste0("site ", sitenum))

  # join all tables to the site map
  tbls_all <- list()
  for(i in 1:length(tbls_to_anon)){
    rslt_name <- tbls_to_anon[[i]] %>% collect()

    if("site_anon" %in% colnames(rslt_name)){
      anoned_tbl_pre <- rslt_name %>%
        select(-c(site_anon, sitenum))
    }else{
      anoned_tbl_pre <- rslt_name
    }

    anoned_tbl <- anoned_tbl_pre %>%
      left_join(site_nums,
                by='site',
                copy=TRUE)%>%
      mutate(site_anon=coalesce(site_anon,site)) # bring in "all" or "total" rows

    tbls_all[[names(tbls_to_anon[i])]] <- anoned_tbl
  }
  return(tbls_all)
}
