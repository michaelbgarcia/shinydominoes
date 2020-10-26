#' Initiate shinydominoes on an existing Shiny application
#'
#' This function sets up the necessary dependency files needed
#' in your project to scale out your Shiny application to multiple
#' project nodes
#'
#' @author Michael B. Garcia
#' @param shiny_app path to the shiny app to deploy
#' 
#' @return success or failure messages
#' 
#' @export
shinydominoes_init = function(shiny_app) {
  cat("Initializing shinydominoes...\n")
  if(missing(shiny_app)) {
    stop("You must supply the path of your Shiny app")
  }
  
  cat("Shiny app found\n")
  write_shinydominoes_appdir(shiny_app)
  
  cat("Creating master app\n")
  write_master_app()
  cat("app_master.R created\n")
  # cat("Creating child app\n")
  # write_child_app(shiny_app)
  # cat("app_child.R created\n")
}