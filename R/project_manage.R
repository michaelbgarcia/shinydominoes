#' Functions to manage Domino projects
#'
#' @author Michael B. Garcia
#' @param project_name name of project
#' @param project_id id of project
#' @param owner_name name of owner
#' @param owner_id id of owner
#' @param hrs_expire hours to keep application open
#' @param hash hashed id to be used for project name
#' @param api_key Domino Data Labs user api key
#' @param host Domino Data Labs host
#'
#' @import purrr httr
#' @rawNamespace import(jsonlite, except = flatten)

# Get project list ----
#' @export
projects_get = function(api_key, host) {
  GET(
    url = paste0(host,ep_project_list()),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}

# Get project summary
#' @export
projects_get_details = function(project_id, api_key, host) {
  GET(
    url = paste0(host,ep_project_summary(project_id)),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}

# Get project hardware tiers
#' @export
projects_get_hardware = function(project_id, api_key, host) {
  GET(
    url = paste0(host,ep_project_hardware(project_id)),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}

#' @export
projects_get_hardware_tbl = function(project_id, api_key, host) {
  projects_get_hardware(project_id, api_key, host) %>% content() %>%
    {
      tibble(
        id = map(., pluck("hardwareTier")) %>% map(., pluck("id")) %>% flatten_chr(),
        name = map(., pluck("hardwareTier")) %>% map(., pluck("name")) %>% flatten_chr()
      )
    }
}


# Schedule runs ----
#' @export
schedulerun_create = function(owner_name, project_name, project_id, hrs_expire, api_key, host) {
  
  if(missing(hrs_expire)) hrs_expire = 4
  
  body_list = list(
    title =  'self_destruct',
    commandToRun =  'domino_project_delete.R',
    overrideHardwareTierId =  'small-k8s',
    overrideEnvironmentId =  '5df81277c552390008eab913',
    datasetConfigName =  '',
    `schedule-run` =  'custom',
    clientTimezone =  'America/New_York',
    customCronString = paste0('0 0 ',hrs_expire,'/',hrs_expire,' * * ?'),
    allowConcurrentRuns =  'false',
    notificationList =  '',
    publishModelId =  ''
  )
  
  POST(
    url = paste0(host,ep_scheduledrun_create(owner_name, project_name)),
    add_headers(`X-Domino-Api-Key` = api_key),
    body = toJSON(body_list, auto_unbox = TRUE),
    content_type("application/json"),
    verbose(),
    config(http_version=2)
  )
  
  # Update description of project with time expire
  start_time = Sys.time()
  stop_time = start_time + hrs_expire * 3600 #(sec / hr)
  
  body_list_patch = list(
    description = paste0("Project expires: ",stop_time)
  )
  
  PATCH(
    url = paste0(host,ep_project_summary(project_id)),
    add_headers(`X-Domino-Api-Key` = api_key),
    body = toJSON(body_list_patch, auto_unbox = TRUE),
    content_type("application/json"),
    verbose(),
    config(http_version=2)
  )
  
}

# app.sh File upload ----
#' @export
project_file_upload_shell = function(owner_name, project_name, api_key, host) {
  
  dir = Sys.getenv("DOMINO_WORKING_DIR")
  shiny_app_config = paste0(dir,"/.shinydominoes_config")
  
  app_content = try(readLines(shiny_app_config))
  if("try-error" %in% class(app_content)) stop("app config file not found. Did you run 'shinydominoes::init()?'")
  if(length(app_content) == 0) stop("app config file not found. Did you run 'shinydominoes::init()?'")
  
  shiny_app = readLines(".shinydominoes_config")[[1]]
  file = paste0("R -e 'shiny::runApp(\"",shiny_app,"\", port=8888, host=\"0.0.0.0\")'")
  PUT(
    url = paste0(host,ep_file_upload(owner_name = owner_name, project_name = project_name, file_path = "app.sh")),
    body = file,
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}

# app.sh File upload ----
#' @export
project_file_upload_delete = function(owner_name, project_name, api_key, host) {
  appDir = system.file("project_delete.R", package = "shinydominoes")
  if (appDir == "") {
    stop("Could not find the project delete file. Try re-installing `shinydominoes`.", call. = FALSE)
  }
  
  PUT(
    url = paste0(host,ep_file_upload(owner_name = owner_name, project_name = project_name, file_path = "domino_project_delete.R")),
    body = upload_file(appDir),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}

# Information ----
# Create children app projects
#' @export
project_create_name = function(project_name, hash) {
  child_name = paste(project_name,"childproject",hash, sep = "_")
  return(child_name)
}

#' @export
project_copy = function(owner_id, project_id, project_name_new, api_key, host) {
  cat(paste("Creating",project_name_new,"\n"))
  
  body_list = list(name = project_name_new
                   # description = project_description,
                   # visibility = project_visibility,
                   # ownerId = owner_id,
                   # collaborators = list(),
                   # tags = list(
                   #   tagNames = list()
                   # )
  )
  
  url = paste0(host,ep_project_copy(project_id))
  
  try({
    POST(
      url = url,
      add_headers(`X-Domino-Api-Key` = api_key),
      body = toJSON(body_list, auto_unbox = TRUE),
      content_type("application/json"),
      verbose(),
      config(http_version=2)
    )
  })
  
  # result_text = paste0("Status: ", response$success,"\n",
  #                      "Details: ", response$message,"\n")
  # cat(result_text)
  
}

# Delete project (be careful, need to have check for child projects only)
#' @export
project_delete = function(owner_name, project_name, api_key, host) {
  response = DELETE(
    url = paste0(host,ep_project_delete(owner_name = owner_name, project_name = project_name)),
    add_headers(`X-Domino-Api-Key` = api_key)
  )
  
  return(response)
}

#' @export
get_project_url = function(owner_name, project_name, host) {
  url = paste(host, url_project(owner_name, project_name), sep = "/")
  return(url)
}

# Get owner id----
#' @export
get_owner_id = function(api_key, host) {
  owner_id = GET(
    url = paste0(host,ep_self()),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  ) %>% content() %>% pluck("id")
  return(owner_id)
}

#' @export
get_project_id = function(owner_name, project_name, api_key, host) {
  #browser()
  project_id = GET(
    url = paste0(host,ep_project_id(owner_name = owner_name, project_name = project_name)),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  ) %>% content() %>% pluck("id")
  
  return(project_id)
}