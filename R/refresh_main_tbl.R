#' Refresh project table
#'
#' Refreshes tbl in the master app control center
#'
#' @author Michael B. Garcia
#' @param project_name name of parent project
#' @param project_id id of parent project
#' @param owner_name name of project owner
#' @param owner_id id of project owner
#' @param hardware_tier_id hardware tier selected for application
#' @param hrs_expire hours to keep application open
#' @param api_key Domino Data Labs user api key
#' @param host Domino Data Labs host
#' 
#' @import purrr httr dplyr
#' @importFrom dplyr filter ungroup rowwise bind_rows tibble mutate
#' 
#' @rawNamespace import(jsonlite, except = flatten)
#' @return success or failure messages
#' 
#' @export
refresh_main_tbl = function(project_name, api_key, host) {
  my_tbl = projects_get(api_key, host)$content %>% {
      tibble(
        id = map(., pluck("id")) %>% flatten_chr,
        name = map(., pluck("name")) %>% flatten_chr,
        app_status = as.character(NA),
        app_url= as.character(NA),
        time_left = as.integer(NA)
      )
    } %>%
    bind_rows() %>%
    filter(grepl(paste(project_name,"childproject",sep = "_"),name)) %>%
    {
      if(nrow(.) > 0) {
        rowwise(.) %>%
          mutate(app_status = app_get_details(id,api_key, host)$status,
                 app_url = app_get_url(app_id = app_get_details(id,api_key, host)$id, host),
                 time_left = 
                   projects_get_details(id, api_key, host)$content %>%
                   pluck("description") %>%
                   gsub(pattern = "Project expires: ", replacement = "", x = .) %>%
                   as.POSIXct(.) %>%
                   difftime(.,Sys.time(), units="secs") %>%
                   .POSIXct(xx = .) %>%
                   format(., "%H:%M:%S")
          ) %>%
          ungroup()
      } else {
        .
      }
    }
  
  return(my_tbl)
}