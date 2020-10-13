#' Functions
#'
#' These functions will be broken out
#'
#' @author Michael B. Garcia
#' @param project_name name of parent project
#' @param owner_name name of owner
#' @param api_key Domino Data Labs user api key
#' @param host Domino Data Labs host
#'
#' @import purrr httr jsonlite magrittr
#' @return confirmation message of success or failure

# Endpoints ----

ep_self = function() {paste0("/v4/users/self")}
ep_app_list = function(project_id) {paste0("/v4/modelProducts?projectId=",project_id)}
ep_app_create = function() {paste0("/v4/modelProducts")}
ep_app_start = function(app_id) {paste0("/v4/modelProducts/",app_id,"/start")}
ep_app_stop = function(app_id) {paste0("/v4/modelProducts/",app_id,"/stop")}
ep_file_upload = function(owner_name, project_name, file_path) {paste0("/v1/projects/",owner_name,"/", project_name,"/", file_path)}
ep_project_create = function() {paste0("/v4/projects")}
ep_project_delete = function(owner_name, project_name) {paste0("/v1/projects/",owner_name,"/",project_name)}
ep_project_copy = function(project_id) {paste0("/v4/projects/",project_id,"/copy")}
ep_project_id = function(owner_name, project_name) {
  ep = paste0("/v4/gateway/projects/findProjectByOwnerAndName?ownerName=",owner_name,"&projectName=",project_name)
  return(ep)
}
ep_project_list = function() {paste0("/v4/gateway/projects?relationship=Owned&showCompleted=false")}
ep_scheduledrun_create = function(owner_name, project_name) {paste0("/u/mgarc135/",project_name,"/scheduledruns")}
#ep_scheduledrun_delete = function(owner_name, run_id) {paste0("/u/mgarc135/PatientMapping_Backup/scheduledruns/unschedule")}

#' @export
url_view_app = function(app_id) {paste0("/modelproducts/",app_id)}
#' @export
url_project = function(owner_name, project_name) {paste("u",owner_name,project_name, sep = "/")}



# Get project list ----
#' @export
projects_get = function(api_key, host) {
  GET(
    url = paste0(host,ep_project_list()),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  )
}
# Schedule runs ----
#' @export
schedulerun_create = function(owner_name, project_name, api_key, host) {

  body_list = list(
    title =  'self_destruct',
    commandToRun =  'domino_project_delete.R',
    overrideHardwareTierId =  'small-k8s',
    overrideEnvironmentId =  '5e8523164faead0008631b88',
    datasetConfigName =  '',
    `schedule-run` =  'custom',
    clientTimezone =  'America/New_York',
    customCronString = '0 0 0/4 * * ?',
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

}

# app.sh File upload ----
#' @export
project_file_upload_shell = function(owner_name, project_name, api_key, host) {
  file = "R -e 'shiny::runApp(\"app_child.R\", port=8888, host=\"0.0.0.0\")'"
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

# App management ----
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


  POST(
    url = paste0(host,ep_app_create()),
    add_headers(`X-Domino-Api-Key` = api_key),
    body = toJSON(body_list, auto_unbox = TRUE),
    content_type("application/json"),
    verbose(),
    config(http_version=2)
  )

}
#' @export
app_start = function(app_id, hardware_tier_id, api_key, host) {
  if(missing(hardware_tier_id)) hardware_tier_id = "medium-k8s"

  url = paste0(host,ep_app_start(app_id))

  POST(url = url,
       add_headers(`X-Domino-Api-Key` = api_key),
       body = toJSON(list(hardwareTierId = hardware_tier_id), auto_unbox = TRUE),
       content_type("application/json")
  )
}
#' @export
app_create_name = function(project_name_new, hash) {
  app_name = paste(project_name_new,"app", sep = "_")
  return(app_name)
}
#' @export
app_get_details = function(project_id, api_key, host) {

  GET(
    url = paste0(host,ep_app_list(project_id = project_id)),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  ) %>% content() %>% pluck(1)
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

#' @export
get_app_id = function(project_name, owner_name, api_key, host) {
  app_id = GET(
    url = paste(host,ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host)), sep = "/"),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  ) %>% content() %>% pluck(1, "id")

  return(app_id)
}

#' @export
get_app_status = function(project_name, owner_name, api_key, host) {
  app_status = GET(
    url = paste(host,ep_app_list(project_id = get_project_id(owner_name, project_name, api_key, host)), sep = "/"),
    add_headers(`X-Domino-Api-Key` = api_key),
    content_type("application/json")
  ) %>% content() %>% pluck(1, "status")

  return(app_status)
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


# Write main analytics app
#' @export
write_child_app = function(shiny_app) {

  if(missing(shiny_app)) {
    stop("You must supply the path of your Shiny app")
  } else {
    # app_path = paste(app_dir, shiny_app, sep = "/")
    app_content = try(readLines(shiny_app))
    if("try-error" %in% class(app_content)) stop("App file does not exist or is corrupted")
    if(length(app_content) == 0) stop("App file is empty")

    shiny_app_dir = gsub(x = shiny_app, pattern = "[^/]+$",replacement = "")

    file.copy(from = shiny_app, to = paste0(shiny_app_dir,"app_child.R"))
  }
}

#Initiate shinydominoes
#' @export
shinydominoes_init = function(shiny_app) {
  cat("Initializing shinydominoes...\n")
  if(missing(shiny_app)) {
    stop("You must supply the path of your Shiny app")
  } else {
    # app_path = paste(app_dir, shiny_app, sep = "/")
    app_content = try(readLines(shiny_app))
    if("try-error" %in% class(app_content)) stop("App file does not exist or is corrupted")
    if(length(app_content) == 0) stop("App file is empty")
  }

  cat("Shiny app found\n")
  cat("Creating master app\n")
  write_master_app()
  cat("app_master.R created\n")
  cat("Creating child app\n")
  write_child_app(shiny_app)
  cat("app_child.R created\n")
}

