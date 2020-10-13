# app_master.R

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(reactable)
library(shinydominoes)

host = "https://opendatasciencelab.jnj.com"
api_key = Sys.getenv("DOMINO_USER_API_KEY")
project_this = Sys.getenv("DOMINO_PROJECT_NAME")
owner_name = Sys.getenv("DOMINO_PROJECT_OWNER")
owner_id = get_owner_id(api_key, host)

ui = navbarPage(
  title = "RSV Analysis",
  theme = shinytheme("flatly"),
  useShinydashboard(),
  useShinyjs(),
  setBackgroundColor(color = "ghostwhite"),
  selected = "Main",
  tabPanel("Main",
           shinydashboard::box(title = "Applications", width = 12,
                               actionButton(inputId = "start_app",
                                            label = "Start a New App Instance",
                                            style = "color:white",
                                            class = "btn-primary"),
                               disabled(
                                 actionButton(inputId = "project_delete",
                                              label = "Delete App Instance",
                                              style = "color:white",
                                              class = "btn-danger")
                               ),

                               br(),
                               br(),
                               reactableOutput("table")
           )
  )
)


server <- function(input, output, session) {

  reac_data = reactiveValues(app_url_view = NULL,
                             project_tbl = NULL)

  reac_timer = reactiveValues(tick = 0,
                              start_app = 0)


  observe({
    invalidateLater(500)
    isolate({
      reac_timer$tick = reac_timer$tick + 1
    })
  })

  observeEvent(c(reac_timer$tick,
                 reac_timer$start_app), {
                   reac_data$project_tbl = projects_get(api_key, host) %>%
                     content() %>% {
                       tibble(
                         id = map(., pluck("id")) %>% flatten_chr,
                         name = map(., pluck("name")) %>% flatten_chr,
                         app_status = as.character(NA),
                         app_url= as.character(NA)
                       )
                     } %>%
                     bind_rows() %>%
                     filter(grepl("_childproject",name)) %>%
                     {
                       if(nrow(.) > 0) {
                         rowwise(.) %>%
                           mutate(app_status = app_get_details(id,api_key, host)$status,
                                  app_url = paste0(host,url_view_app(app_id = app_get_details(id,api_key, host)$id)))
                       } else {
                         .
                       }
                     }


                 })

  output$table <- renderReactable({
    req(reac_data$project_tbl)
    reactable(reac_data$project_tbl,
              selection = "multiple",
              paginationType = "simple",
              columns = list(
                app_url = colDef(
                  cell = function(value) {
                    url = value
                    tags$a(href = url, target = "_blank", "View app")
                  }
                )
              )
    )
  })

  observeEvent(input$start_app, {
    withProgress(message = 'Starting a new application instance', value = 1, {

      # Hash for project & app
      hash = as.integer(Sys.time())

      # Get parent project id
      project_id = get_project_id(owner_name = owner_name, project_name = project_this, api_key = api_key, host = host)
      # Create new project name
      project_name_new = project_create_name(project_name = project_this, hash = hash)

      # Copy Project
      project_id_new = project_copy(owner_id = owner_id,
                                    project_id = project_id,
                                    project_name_new = project_name_new,
                                    api_key = api_key, host = host) %>%
        content() %>% pluck("id")

      # #Upload app.sh
      project_file_upload_shell(owner_name, project_name_new, api_key, host)

      # Create new app name
      app_name_new = app_create_name(project_name_new = project_name_new)

      # Create app
      app_id_new = app_create(app_name = app_name_new,
                              project_id = project_id_new,
                              api_key = api_key, host = host) %>%
        content() %>% pluck("id")

      # Start app
      app_start(app_id = app_id_new, api_key = api_key, host = host)

      # Schedule project deletion
      project_file_upload_delete(owner_name, project_name_new, api_key, host)
      schedulerun_create(owner_name, project_name_new, api_key, host)

      reac_timer$start_app = reac_timer$start_app + 1

    })

  })


  observe({
    table_selected = getReactableState("table", "selected")
    shinyjs::toggleState("project_delete", isTruthy(table_selected))
  })

  observeEvent(input$project_delete, {
    table_selected = getReactableState("table", "selected")
    req(table_selected)

    withProgress(message = 'Deleting selected apps', value = 1, {


      table_selected_names = reac_data$project_tbl %>%
        slice(table_selected) %>%
        pull(name)

      table_selected_names %>% map(~project_delete(owner_name = owner_name,
                                                   project_name = .,
                                                   api_key = api_key,
                                                   host = host))
    })

  })

}

shinyApp(ui, server)
