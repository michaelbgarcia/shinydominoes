#' Functions to manage shiny applications
#'
#' @author Michael B. Garcia
#' @param project_name name of parent project
#' @param owner_name name of owner
#' @param api_key Domino Data Labs user api key
#' @param host Domino Data Labs host
#'
#' @import purrr httr
#' @rawNamespace import(jsonlite, except = flatten)


#' @export
app_get_url = function(app_id, host) {
  url = paste0(host,ep_app_url(app_id = app_id))
  return(url)
}
#' @export
app_create = function(app_name, project_id, api_key, host) {
  
  body_list = list(
    modelProductType = "APP",
    projectId = project_id,
    name = app_name,
    description = "App produced by shinyDomino (github link TBD)",
    created = as.numeric(Sys.time()),
    lastUpdated = as.numeric(Sys.time()),
    status = "",
    media = list(),
    openUrl = "",
    tags = list(),
    stats = list(usageCount = 0),
    appExtension = list(appType = ""),
    id = "000000000000000000000000",
    permissionsData = list(
      visibility = "PUBLIC",
      accessRequestStatuses = {},
      pendingInvitations = list(),
      discoverable = FALSE,
      appAccessStatus = "ALLOWED"
    )
  )
  
  ep = ep_app_create()
  resp = POST(
    url = paste0(host,ep),
    add_headers(`X-Domino-Api-Key` = api_key),
    body = toJSON(body_list, auto_unbox = TRUE),
    content_type("application/json"),
    verbose(),
    config(http_version=2)
  )
  
  parsed = content(resp)
  
  structure(
    list(
      content = parsed,
      path = ep,
      response = resp
    ),
    class = "domino_api"
  )
  
}
#' @export
app_start = function(app_id, hardware_tier_id, api_key, host) {
  if(missing(hardware_tier_id)) hardware_tier_id = "medium-k8s"
  
  ep = ep_app_start(app_id)
  url = paste0(host,ep)
  
  resp = POST(url = url,
       add_headers(`X-Domino-Api-Key` = api_key),
       body = toJSON(list(hardwareTierId = hardware_tier_id), auto_unbox = TRUE),
       content_type("application/json")
  )
  parsed = content(resp)
  
  structure(
    list(
      content = parsed,
      path = ep,
      response = resp
    ),
    class = "domino_api"
  )
}

#' @export
app_create_name = function(project_name_new, hash) {
  app_name = paste(project_name_new,"app", sep = "_")
  return(app_name)
}

#' @export
app_get_details = function(project_id, api_key, host) {
  
  # GET(
  #   url = paste0(host,ep_app_list(project_id = project_id)),
  #   add_headers(`X-Domino-Api-Key` = api_key),
  #   content_type("application/json")
  # ) %>% content() %>% pluck(1)
  
  ep = ep_app_list(project_id = project_id)
  resp = GET(
    url = paste0(host,ep),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
  
  parsed = content(resp) %>% pluck(1)
  
  structure(
    list(
      content = parsed,
      path = ep,
      response = resp
    ),
    class = "domino_api"
  )
}

#' @export
get_app_id = function(project_name, owner_name, api_key, host) {
  # app_id = GET(
  #   url = paste(host,ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host)), sep = "/"),
  #   add_headers(`X-Domino-Api-Key` = api_key),
  #   content_type("application/json")
  # ) %>% content() %>% pluck(1, "id")
  
  ep = ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host))
  resp = GET(
    url = paste0(host,ep),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
  
  parsed = content(resp) %>% pluck(1, "id")
  
  structure(
    list(
      content = parsed,
      path = ep,
      response = resp
    ),
    class = "domino_api"
  )
  
  # return(app_id)
}

#' @export
get_app_status = function(project_name, owner_name, api_key, host) {
  # app_status = GET(
  #   url = paste(host,ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host)), sep = "/"),
  #   add_headers(`X-Domino-Api-Key` = api_key),
  #   content_type("application/json")
  # ) %>% content() %>% pluck(1, "status")
  
  ep = ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host))
  
  resp = GET(
    url = paste0(host,ep),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
  
  parsed = content(resp) %>% pluck(1, "status")
  
  structure(
    list(
      content = parsed,
      path = ep,
      response = resp
    ),
    class = "domino_api"
  )
  
  # return(app_status)
}


# Write control panel app to main file
#' @export
write_master_app = function() {
  appDir <- system.file("app_master.R", package = "shinydominoes")
  if (appDir == "") {
    stop("Could not find app master file. Try re-installing `shinydominoes`.", call. = FALSE)
  }
  file.copy(from = appDir, to = getwd(), overwrite = TRUE)
  writeLines(text = "R -e 'shiny::runApp(\"app_master.R\", port=8888, host=\"0.0.0.0\")'",
             "app.sh")
}

# Write main analytics app - config file
#' @export
write_shinydominoes_appdir = function(shiny_app) {
  
  if(missing(shiny_app)) {
    stop("You must supply the path of your Shiny app")
  } 
  
  # shiny_app_dir = gsub(x = shiny_app, pattern = "[^/]+$",replacement = "")
  # 
  # writeLines(shiny_app, paste0(shiny_app_dir,".shinydominoes_config"))
  dir = Sys.getenv("DOMINO_WORKING_DIR")
  file_dir = paste0(dir,"/.shinydominoes_config")
  
  writeLines(shiny_app, file_dir)
  
  cat(paste0("config file written to ",file_dir,"\n"))
  
}
