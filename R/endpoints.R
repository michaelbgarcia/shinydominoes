#' Domino Data Labs API Endpoints
#'
#' These endpoints are used by functions within this package
#'
#' @author Michael B. Garcia
#' @param project_id project id of a Domino project
#' @param app_id app id of a published Shiny app
#' @param project_name name of parent project
#' @param owner_name name of owner
#' @param file_path destination path used for file upload
#' 
#' @return url endpoints of Domino Data Labs API

ep_self = function() {paste0("/v4/users/self")}
ep_app_list = function(project_id) {paste0("/v4/modelProducts?projectId=",project_id)}
ep_app_create = function() {paste0("/v4/modelProducts")}
ep_app_start = function(app_id) {paste0("/v4/modelProducts/",app_id,"/start")}
ep_app_stop = function(app_id) {paste0("/v4/modelProducts/",app_id,"/stop")}
ep_file_upload = function(owner_name, project_name, file_path) {paste0("/v1/projects/",owner_name,"/", project_name,"/", file_path)}
ep_project_create = function() {paste0("/v4/projects")}
ep_project_delete = function(owner_name, project_name) {paste0("/v1/projects/",owner_name,"/",project_name)}
ep_project_copy = function(project_id) {paste0("/v4/projects/",project_id,"/copy")}
ep_project_hardware = function(project_id) {paste0("/v4/projects/",project_id,"/hardwareTiers")}
ep_project_id = function(owner_name, project_name) {
  ep = paste0("/v4/gateway/projects/findProjectByOwnerAndName?ownerName=",owner_name,"&projectName=",project_name)
  return(ep)
}
ep_project_summary = function(project_id) {paste0("/v4/projects/",project_id)}
ep_project_list = function() {paste0("/v4/gateway/projects?relationship=Owned&showCompleted=false")}
ep_scheduledrun_create = function(owner_name, project_name) {paste0("/u/mgarc135/",project_name,"/scheduledruns")}

#' @export
ep_app_url = function(app_id) {paste0("/modelproducts/",app_id)}
#' @export
ep_project_url = function(owner_name, project_name) {paste("u",owner_name,project_name, sep = "/")}