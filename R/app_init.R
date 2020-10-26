#' Create application instance
#' 
#' 
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
#' @import purrr httr
#' @rawNamespace import(jsonlite, except = flatten)

#' @export
app_init = function(project_name, project_id, 
                    owner_name, owner_id, 
                    hardware_tier_id, hrs_expire,
                    api_key, host) {
  
  # Hash for project & app
  hash = as.integer(Sys.time())
  
  # Create new project name
  project_name_new = project_create_name(project_name = project_name, hash = hash)
  
  # Copy Project
  project_id_new = project_copy(owner_id = owner_id,
                                project_id = project_id,
                                project_name_new = project_name_new,
                                api_key = api_key, host = host) %>%
    content() %>% 
    pluck("id")
  
  # #Upload app.sh
  project_file_upload_shell(owner_name, project_name_new, api_key, host)
  
  # Create new app name
  app_name_new = app_create_name(project_name_new = project_name_new)
  
  # Create app
  app_id_new = app_create(app_name = app_name_new,
                          project_id = project_id_new,
                          api_key = api_key, host = host) %>%
    content() %>% 
    pluck("id")
  
  # Start app
  app_start(app_id = app_id_new, hardware_tier_id = hardware_tier_id, api_key = api_key, host = host)
  
  # Schedule project deletion
  project_file_upload_delete(owner_name, project_name_new, api_key, host)
  schedulerun_create(owner_name, project_name_new, project_id_new, hrs_expire, api_key, host)
}